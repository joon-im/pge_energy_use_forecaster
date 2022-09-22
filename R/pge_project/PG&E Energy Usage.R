# PG&E Electric Bill History

# Load libraries
library(tidyverse)
library(skimr)
library(timetk)
library(lubridate)
library(plotly)
library(tidyquant)
library(modeltime)
library(rsample)
library(tidymodels)
library(xgboost)

# Load data
pge_electricity <- read_csv("~/R/pge_project/monthly_billing/pge_electricity_2012-07-09_to_2022-06-06.csv")

# Review data
head(pge_electricity)
tail(pge_electricity)
glimpse(pge_electricity)
skim(pge_electricity)

# Data preparation
pge_elec <- pge_electricity %>%
  select(-TYPE, -NOTES, -UNITS) %>%
  rename(start_date = `START DATE`, end_date = `END DATE`,
         Usage = USAGE, Cost = COST) %>% 
  mutate(Cost = str_remove(Cost, pattern = "\\$"),
         Cost = as.numeric(Cost),
         Date = floor_date(start_date, unit = "month")) %>%
  select(Date, everything())

# Write to file
# pge_elec %>%
#   write_rds("~/R/pge_project/artifacts/pge_monthly_bill_raw_tbl.rds")

pge_elec_plotting <- pge_elec %>%
  mutate(new_start_date = format(start_date , "%m/%d/%Y"),
         new_end_date = format(end_date, "%m/%d/%Y")) %>%
  unite("start_end_date", new_start_date:new_end_date, remove=FALSE, sep=" to ") %>%
  mutate(cost_per_kwh = signif(Cost / Usage, digits = 3)) %>%
  select(-start_date, -end_date, -new_start_date, -new_end_date)

# Plot energy usage with by date, usage, cost
cost_plot <- pge_elec_plotting %>% 
  ggplot(aes(x = Date, y = Cost, 
             text = paste0("Date: ", start_end_date, "\n", 
                           "Cost per kWh: ", cost_per_kwh %>% scales::dollar(), "\n",
                           "Total kWh: ", Usage, "\n", 
                           "Monthly Cost: ", Cost %>% scales::dollar()))) + 
  geom_line(aes(group = 1), color = palette_light()[1]) +
  geom_point(color = palette_light()[2], size = 1) +
  geom_smooth(aes(group=2), method="loess", se = FALSE) +
  ggtitle("PG&E Energy Cost") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_tq()


# Plotly
ggplotly(cost_plot, tooltip = "text")

usage_plot <- pge_elec_plotting %>% 
    ggplot(aes(x = Date, y = Usage, 
               text = paste0("Date: ", start_end_date, "\n", 
                             "Cost per kWh: ", cost_per_kwh %>% scales::dollar(), "\n",
                             "Total kWh: ", Usage, "\n", 
                             "Monthly Cost: ", Cost %>% scales::dollar()))) + 
    geom_line(aes(group = 1), color = palette_light()[1]) +
    geom_point(color = palette_light()[2], size = 1) +
    geom_smooth(aes(group=2), method="loess", se = FALSE) + 
    ggtitle("PG&E Energy Usage") +
    theme_tq()

# Plotly
ggplotly(usage_plot, tooltip = "text")

# Split Data 80/20
splits <- initial_time_split(pge_elec, prop = 0.8)

# Create and fit multiple models
# Model 1: auto_arima 
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Usage ~ Date, data = training(splits))

# Model 2: arima_boost
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
  ) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Usage ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
      data = training(splits))

# Model 3: ets
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Usage ~ Date, data = training(splits))

# Model 4: prophet
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Usage ~ Date, data = training(splits))


# Add fitted models to table
models_tbl <- modeltime_table(
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet
)

# Models table
models_tbl

# Calibrate the model to a testing set
calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl

# Testing Set Forecast & Accuracy Evaluation
calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = pge_elec
    ) %>%
    plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE
    )

# Accuracy metrics of model fit
calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
        .interactive = TRUE
    )

# Refit to Full Dataset & Forecast Forward
refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = pge_elec)

refit_tbl %>%
    modeltime_forecast(h = "1 year", actual_data = pge_elec) %>%
    plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE,
        .trelliscope = FALSE
    ) 

# Write to file
# refit_tbl %>%
#   write_rds("~/R/pge_project/artifacts/pge_monthly_bill_refit_tbl.rds")

