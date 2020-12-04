# Stock-Prediction-Machine-Learning-Algorithm-
The project have developed by Team
Julian Alex Alvarez
Saroj Gopali 
Brittany Love 
Kyle Wipfli 
 CS 5311 : R Programming for Machine Learning and Data Analytics 
  Final Project: Stock Prediction Machine Learning Algorithm"

Shiny application that serves to forecast stock prices using Seasonal and Non-Seasonal ARIMA  and
univariate LSTM. It Will plot these predictions and will also calculate the mean 
absolute errors (MAE) for both forecasts through ARIMA & LSTM.
Stocks data are taken from Yahoo.com
# To run the project the following libraies are required
list.of.packages <- c('shiny',
                      'shinydashboard',
                      'shinycssloaders',
                      'tseries',
                      'plotly',
                      'ggplot2',
                      'quantmod',
                      'forecast',
                      'tidyquant',
                      'rsample',
                      'gridExtra',
                      'gridBase',
                      'reticulate',
                      'tensorflow',
                      'keras')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# loads packages -->
lapply(list.of.packages, require, character.only = TRUE)

# The system should have tensorflow for the keras Package 

# To predict Stock the from_data in line 119 and to_data line 121 and days_FC  in line 123 need to change accordingly 
from _data = "staring date for data"
to_data = "end date for data"
days_FC = " number of days to forecast"
