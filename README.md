# PG&E Home Energy Forecaster

This dashboard is built in R using the Shiny web application framework. It utilizes data downloaded from my personal account through the Pacific Gas and Electric company's website. The data consists of multiple comma-separated files of historical data containing energy usage and costs which are then merged together to create a single dataset with a nested data structure. It takes an iterative forecasting approach using tools from the [modeltime](https://github.com/business-science/modeltime) forecasting suite to predict my home's energy usage and costs based on both daily and monthly data. The main idea behind iterative forecasting is to "nest" multiple time series groups into a single dataset and then fit multiple models to each group. 

See it in action here: https://joon.shinyapps.io/pge_home_energy_forecaster

![Screen Shot 2022-09-22 at 4 37 38 PM](https://user-images.githubusercontent.com/32493276/191869342-8beb4227-88c9-4736-8b2c-6cd1d93637fe.png)

Built with R, Shiny, modeltime, timetk, tidyverse, and flexdashboard.
