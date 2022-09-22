# LIBRARIES ----
library(tidyverse)
library(skimr)
library(timetk)
library(modeltime)
library(tidymodels)
library(xgboost)
library(plotly)
library(tidyquant)
library(ggthemes)

# DATA PREPROCESSING ----
# * Load and combine datasets ----
df <- list.files(
  path='/Users/joonhoim/R/pge_project/daily_usage_elec', # Change to your directory
  full.names = TRUE,  
  pattern = "*.csv"
  ) %>% 
  map_df(~read_csv(., skip = 4)) # Combine multiple files, remove headers

# * Review data structure ----
head(df)
tail(df)
glimpse(df)
skim(df)

# * Data preparation ----
daily_elec <- df %>%
  select(-TYPE, -NOTES, -UNITS) %>%
  rename(start_time = `START TIME`, end_time = `END TIME`,
         Usage = USAGE, Cost = COST, Date = DATE) %>% 
  mutate(Cost = str_remove(Cost, pattern = "\\$"),
         Cost = as.numeric(Cost)) 

# * Reduce data by summarizing by day ----
daily_elec_sum <- daily_elec %>% 
  select(-start_time, -end_time) %>%
  pivot_longer(
    !Date, names_to = "type", values_to = "value"
  ) %>%
  group_by(type) %>%
  summarise_by_time(
    .date_var = Date,
    .by = "day",
    value = sum(value)
  ) %>%
  ungroup() %>%
  mutate(type = as_factor(type))

# FUNCTIONS ----
# * Summarize data by different lengths of time ----
sum_by_length <- function(df, length = "day"){
  
  df %>% 
    select(-start_time, -end_time) %>%
    pivot_longer(
      !Date, names_to = "type", values_to = "value"
    ) %>%
    group_by(type) %>%
    summarise_by_time(
      .date_var = Date,
      .by = length,
      value = sum(value)
    ) %>%
    ungroup() %>%
    mutate(type = as_factor(type))
  
}

# * Plot different variables ----
sum_plot <- function(data, x, y, point_size = 0.5){
  
  data <- data %>%
    pivot_wider(names_from = "type", values_from = "value") %>%
    mutate(cost_per_kwh = signif(Cost / Usage, digits = 3))
  
  x <- enquo(x)
  y <- enquo(y)
  
  ggp <- data %>% ggplot(aes(x = !! x, y = !! y, 
                    text = paste0("Date: ", Date, "\n", 
                                  "Cost per kWh: ", cost_per_kwh %>% scales::dollar(), "\n",
                                  "Total kWh: ", Usage, "\n", 
                                  "Total Cost: ", Cost %>% scales::dollar()))) + 
    geom_line(aes(group = 1), color = palette_light()[1]) +
    geom_point(color = palette_light()[2], size = point_size) + 
    geom_smooth(aes(group=2), method="loess", se = FALSE) +
    ggtitle(paste0("PG&E Energy ", as_label(y))) +
    theme_tq()
  
  return(ggp)
}

# TEST THE FUNCTIONS ----
# * Plot usage by month ----
sum_by_length(df = daily_elec, length = "month") %>%
  sum_plot(x = Date, y = Usage, point_size = 0.1) %>%
  ggplotly(tooltip = "text")

# * Plot cost by week ----
sum_by_length(df = daily_elec, length = "week") %>%
  sum_plot(x = Date, y = Cost, point_size = 0.1) %>%
  ggplotly(tooltip = "text")



# NESTED FORECASTING ----
# * Create nested table by type
nested_daily_tbl <- sum_by_length(df = daily_elec, length = "day") %>%
  group_by(type) %>%
  # Extend time series 90 days into the future
  extend_timeseries(
    .id_var = type,
    .date_var = Date,
    .length_future = 90
  ) %>%
  # Nest time series into the actual data and future data
  nest_timeseries(
    .id_var = type,
    .length_future = 90
  ) %>%
  # Split the nested time series data for training and testing
  split_nested_timeseries(
    .length_test = 90
  )

# Show first 6 nested time series
nested_daily_tbl %>% head()

# MODELING ----

set.seed(123)

# * Prepare recipe for XGBoost model
recipe_xgb <- recipe(value ~ ., extract_nested_train_split(nested_daily_tbl)) %>%
  # Augment the data by adding time-related features
  step_timeseries_signature(Date) %>%
  step_rm(Date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# View how data was augmented
bake(prep(recipe_xgb), extract_nested_train_split(nested_daily_tbl)) 

# * Workflow to create XGBoost model 
wflw_xgb <- workflow() %>%
  add_model(boost_tree("regression", learn_rate = 0.35) %>% set_engine("xgboost")) %>%
  add_recipe(recipe_xgb)

# * Workflow to create temporal hierarchical forecasting model
wflw_thief <- workflow() %>%
  add_model(temporal_hierarchy() %>% set_engine("thief")) %>%
  add_recipe(recipe(value ~ ., extract_nested_train_split(nested_daily_tbl)))

# * Workflow to create prophet model
wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% set_engine("prophet")
    ) %>%
  add_recipe(
    recipe(value ~ Date, extract_nested_train_split(nested_daily_tbl))
    )

# * Workflow to create ETS model
wflw_ets <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe(value ~ Date, extract_nested_train_split(nested_daily_tbl)))

# * Workflow to create Auto-ARIMA model
wflw_arima <- workflow() %>%
  add_model(arima_reg() %>% set_engine("auto_arima")) %>%
  add_recipe(recipe(value ~ Date, extract_nested_train_split(nested_daily_tbl)))

# * Prepare recipe for ARIMA-XGBoost model
recipe_arima_xgb <- recipe(value ~ ., extract_nested_train_split(nested_daily_tbl)) %>%
  # Augment the data by adding time-related features
  step_timeseries_signature(Date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# * Workflow to create Boosted Auto-ARIMA model
wflow_arima_bst <- workflow() %>%
  add_model(
    arima_boost(min_n = 2, learn_rate = 0.015) %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_arima_xgb)



# FIT MODELS ----
# * Apply models to nested data
nested_daily_modeltime_tbl <- nested_daily_tbl %>% 
  modeltime_nested_fit(
    
    model_list = list(
      wflw_xgb,
      wflw_thief,
      wflw_prophet,
      wflw_ets,
      wflw_arima,
      wflow_arima_bst
    ),
    
    control = control_nested_fit(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )

# Show new nested data with results of forecast
nested_daily_modeltime_tbl

# * Check for errors ----
nested_daily_modeltime_tbl %>% extract_nested_error_report()

# * Check data integrity
nested_daily_modeltime_tbl %>%
  extract_nested_train_split() %>% 
  tail()

# * Review test accuracy ----
nested_daily_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy()

# * Visualize a test forecast
nested_daily_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(type) %>%
  plot_modeltime_forecast(.interactive = TRUE)

# CHOOSE BEST MODEL ----
# Determine accuracy metric of choice
nested_daily_modeltime_best_tbl <- nested_daily_modeltime_tbl %>%
  modeltime_nested_select_best(metric = "mae")

# * Visualize fit of top models ----
nested_daily_modeltime_best_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(type) %>%
  plot_modeltime_forecast()

# REFIT MODEL ----

# Refit the best models
nested_best_daily_refit_tbl <- nested_daily_modeltime_best_tbl %>%
  modeltime_nested_refit(
    control = control_refit(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )

# Visualize future forecast of best-fitting model
nested_best_daily_refit_tbl %>%
  extract_nested_future_forecast() %>%
  #group_by(type) %>%
  filter(type != 'Cost') %>%
  plot_modeltime_forecast(.interactive = FALSE)

# Messing around with visualizations
g <- nested_best_daily_refit_tbl %>%
  extract_nested_future_forecast() %>%
  filter(type != 'Cost') %>%
  rename(Model = .model_desc,
         Value = .value,
         Date = .index,
         `CI Low` = .conf_lo,
         `CI High` = .conf_hi) %>%
  mutate(Model = str_to_title(Model)) %>%
  mutate(Model = Model %>% stringr::str_trunc(width = 25)) %>%
  ggplot(aes(x=Date,y=Value,color=Model)) + 
  geom_line() + 
  geom_ribbon(
    ggplot2::aes(
      ymin = `CI Low`,
      ymax = `CI High`,
      color = Model
    ),
    fill     = "grey20",
    alpha    = 0.20,
    linetype = 0
  ) + 
  theme_gdocs() +
  scale_color_tableau() +
  labs(color = "Legend") +
  theme(plot.title = element_text(color = "#424242", face = "bold"),
        legend.title = element_text(color = "#424242"),
        legend.text = element_text(color = "#616161")) 

ggplotly(g)


# * Check for errors ----
nested_best_daily_refit_tbl %>% extract_nested_error_report()

# Write to file
# nested_best_daily_refit_tbl %>%
#   write_rds("~/R/pge_project/artifacts/pge_best_models_tbl.rds")

# EXTEND FLOW TO MONTHLY BILL
# * Create nested table by type
nested_monthly_tbl <- sum_by_length(df = daily_elec, length = "month") %>%
  group_by(type) %>%
  # Extend time series 12 months into the future
  extend_timeseries(
    .id_var = type,
    .date_var = Date,
    .length_future = 12
  ) %>%
  # Nest time series into the actual data and future data
  nest_timeseries(
    .id_var = type,
    .length_future = 12
  ) %>%
  # Split the nested time series data for training and testing
  split_nested_timeseries(
    .length_test = 12
  )

# * Prepare recipe for XGBoost model
recipe_xgb_monthly <- recipe(value ~ ., extract_nested_train_split(nested_monthly_tbl)) %>%
  # Augment the data by adding time-related features
  step_timeseries_signature(Date) %>%
  step_rm(Date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# View how data was augmented
bake(prep(recipe_xgb_monthly), extract_nested_train_split(nested_monthly_tbl)) 

# * Workflow to create XGBoost model 
wflw_xgb_monthly <- workflow() %>%
  add_model(boost_tree("regression", learn_rate = 0.35) %>% set_engine("xgboost")) %>%
  add_recipe(recipe_xgb_monthly)

# * Workflow to create temporal hierarchical forecasting model
wflw_thief_monthly <- workflow() %>%
  add_model(temporal_hierarchy() %>% set_engine("thief")) %>%
  add_recipe(recipe(value ~ ., extract_nested_train_split(nested_monthly_tbl)))

# * Workflow to create prophet model
wflw_prophet_monthly <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% set_engine("prophet")
  ) %>%
  add_recipe(
    recipe(value ~ Date, extract_nested_train_split(nested_monthly_tbl))
  )

# * Workflow to create ETS model
wflw_ets_monthly <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe(value ~ Date, extract_nested_train_split(nested_monthly_tbl)))

# * Workflow to create Auto-ARIMA model
wflw_arima_monthly <- workflow() %>%
  add_model(arima_reg() %>% set_engine("auto_arima")) %>%
  add_recipe(recipe(value ~ Date, extract_nested_train_split(nested_monthly_tbl)))

# * Prepare recipe for ARIMA-XGBoost model
recipe_arima_xgb_monthly <- recipe(value ~ ., extract_nested_train_split(nested_monthly_tbl)) %>%
  # Augment the data by adding time-related features
  step_timeseries_signature(Date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# View how data was augmented
bake(prep(recipe_arima_xgb_monthly), extract_nested_train_split(nested_monthly_tbl)) 

# * Workflow to create Boosted Auto-ARIMA model
wflow_arima_bst_monthly <- workflow() %>%
  add_model(
    arima_boost(min_n = 2, learn_rate = 0.015) %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_arima_xgb_monthly)


# FIT MONTHLY MODELS ----
# * Apply models to nested data
nested_monthly_modeltime_tbl <- nested_monthly_tbl %>% 
  modeltime_nested_fit(
    
    model_list = list(
      wflw_xgb_monthly,
      wflw_thief_monthly,
      wflw_prophet_monthly,
      wflw_ets_monthly,
      wflw_arima_monthly,
      wflow_arima_bst_monthly
    ),
    
    control = control_nested_fit(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Show new nested data with results of forecast
nested_monthly_modeltime_tbl

# * Check for errors ----
nested_monthly_modeltime_tbl %>% extract_nested_error_report()

# * Check data integrity
nested_monthly_modeltime_tbl %>%
  extract_nested_train_split() %>% 
  tail()

# * Review test accuracy ----
nested_monthly_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy()

# * Visualize a test forecast
nested_monthly_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(type) %>%
  plot_modeltime_forecast(.interactive = TRUE)

# CHOOSE BEST MODEL ----
# Determine accuracy metric of choice
nested_monthly_modeltime_best_tbl <- nested_monthly_modeltime_tbl %>%
  modeltime_nested_select_best(metric = "mae")

# * Visualize fit of top models ----
nested_monthly_modeltime_best_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(type) %>%
  plot_modeltime_forecast()

# REFIT MODEL ----

# Refit the best models
nested_best_monthly_refit_tbl <- nested_monthly_modeltime_best_tbl %>%
  modeltime_nested_refit(
    control = control_refit(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )

# Visualize future forecast of best-fitting model
nested_best_monthly_refit_tbl %>%
  extract_nested_future_forecast() %>%
  #group_by(type) %>%
  filter(type != 'Cost') %>%
  plot_modeltime_forecast(.interactive = FALSE)

# * Check for errors ----
nested_best_monthly_refit_tbl %>% extract_nested_error_report()

# Write to file
# nested_best_monthly_refit_tbl %>%
#   write_rds("~/R/pge_project/artifacts/pge_monthly_best_models_tbl.rds")








