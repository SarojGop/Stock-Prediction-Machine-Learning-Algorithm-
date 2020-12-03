" Julian Alex Alvarez, Saroj Gopali, Brittany Love, & Kyle Wipfli -->  
  CS - 5311 - 001: R Programming for Machine Learning and Data Analytics 
  Final Project: Stock Prediction Machine Learning Algorithm"

" Shiny application that serves to forecast stock prices using ARIMA and
  univariate LSTM. Will plot these predictions and will also calculate the mean 
  absolute errors (MAE) for both forecasts through ARIMA & LSTM.
  Stocks are examined from Jan. 1, 2020 to Nov. 13, 2020"

#-------------------------------------------------------------------------------

#installing required libraries 
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


rm(list = ls()) #clears previous data

#-------------------------------------------------------------------------------

# definition of UI -->
ui <- dashboardPage(skin = "green",
                    
                    # definition of header of app -->
                    dashboardHeader(title = "Stock Prediction ML Algorithm", 
                                    titleWidth = 350),
                    
                    
                    # definition of sidebar -->
                    dashboardSidebar( 
                      textInput("StockCode", "Enter Stock Symbol (Ticker):", value='FB'),
                      actionButton(inputId = "click", label = "Go"),
                      tags$hr(),
                      p("Texas Tech University Whitacre College of Engineering"),
                      p("Course: CS-5311-001"),
                      p(" R Programming for Machine Learning and Data Analytics"),
                      p(" Creators: Saroj Gopali, Julian Alex Alvarez, Brittany Love, and Kyle Wipfli")
                      
                      ),# end of dashboard sidebar
                    
                    
                    # definition of dashboard main body -->
                    dashboardBody(h4('Take note that these models may take a moment to load...'),
                                  tabsetPanel(
                                    #ARIMA STARTS
                                    tabPanel(h4("ARIMA Time Series Model"),                      
                                             fluidRow(
                                                box(
                                                 title = "Non-seasonal ARIMA - Forecast",solidHeader = T,
                                                 status = "warning",
                                                 plotOutput("arima_non_seasonal_plot", height = 350)%>% withSpinner(type = 4, color = "#0dc5c1"),
                                                 height = 400 ),
                                                box(
                                                 title = "Non-seasonal ARIMA - Data Table", status = "warning",solidHeader = T,
                                                 tableOutput("arima_non_seasonal_table")%>% withSpinner(type = 4, color = "#0dc5c1"),
                                                 height = 400),
                                                ),
                                                         
                                             fluidRow(
                                               box(
                                                 title = "Seasonal ARIMA - Forecast",solidHeader = T,
                                                 status = "warning",
                                                 plotOutput("arima_seasonal_plot", height = 350)%>% withSpinner(type = 4, color = "#0dc5c1"),
                                                 height = 400),
                                               box(
                                                 title = "Seasonal ARIMA - Data Table", solidHeader = T,
                                                 status = "warning",
                                                 tableOutput("arima_seasonal_table")%>% withSpinner(type = 4, color = "#0dc5c1"),
                                                 height = 400)
                                               )),
                                             #ARIMA TAB ENDS
                     
                     #LSTM STARTS
                     tabPanel(h4("LSTM Time Series Model"),
                      fluidRow(
                         box(
                           title = "LSTM Forecast",solidHeader = T,
                           status = "warning",
                           plotlyOutput("lstm_gg", height = 350)%>% withSpinner(type = 4, color = "#0dc5c1"),
                           height = 400),
                         box(
                           title = "LSTM Forecast with Original Data",solidHeader = T,
                           status = "warning",
                           plotOutput("lstm_plot", height = 350)%>% withSpinner(type = 4, color = "#0dc5c1"),
                           height = 400)
                         ))#LSTM ENDS
                     )# end of tabset
                    )# end of dashboard main body
                    
          )# end of UI


#-------------------------------------------------------------------------------


# define server logic -->
server <- function(input, output) {
 
  #start date to collect data from
  from_data = "2020-03-23"
  #end date to collect data from
  to_data = "2020-11-12"
  #days to forecast the price of stock
  days_FC = 10
  
  
  # PLOTTING FORECAST FOR ARIMA NONSEASONAL -->
  output$arima_non_seasonal_plot <- renderPlot({
   
    # processes after go button pressed
    data <- eventReactive(input$click, {
      (input$StockCode)
    })
    #Get data from yahoo
    Stock <- as.character(data())
   
    Stock_data_frame <- as.data.frame(getSymbols(
      Symbols = Stock,
      src = "yahoo",
      from = from_data,
      to =to_data,
      env = NULL
    ))
  
    Stock_data_frame$Open = Stock_data_frame[, 1]
    Stock_data_frame$High = Stock_data_frame[, 2]
    Stock_data_frame$Low = Stock_data_frame[, 3]
    Stock_data_frame$Close = Stock_data_frame[, 4]
    Stock_data_frame$Volume = Stock_data_frame[, 5]
    Stock_data_frame$Adj = Stock_data_frame[, 6]
    Stock_data_frame <- Stock_data_frame[, c(7, 8, 9, 10, 11, 12)]
    
    #moving average  7, 30 taken to forecast 
    Stock_data_frame$v7_MA = ma(Stock_data_frame$Close, order = 7)
    Stock_data_frame$v30_MA <- ma(Stock_data_frame$Close, order = 30)
    
    # Seasonal & Trend Decomposition (STL)
    rental_ma <- ts(na.omit(Stock_data_frame$v7_MA), frequency = 30)
    decomp_rental <- stl(rental_ma, s.window = "periodic")
    adj_rental <- seasadj(decomp_rental)
    
    #model auto arima
    #fit <- auto.arima(Stock_data_frame$Close, ic = "bic")
    fit.Arima <- Arima(Stock_data_frame$Close, order = c(1,3,3))
    fit.forecast <- forecast(fit.Arima, h =days_FC)
  
    #plot forecast
    plot(fit.forecast,  main = Stock, xlab = "TIME (DAYS)",
         ylab = "PRICE (USD $)")
       
    fit.forecast
    
  })# end of plot for ARIMA nonseasonal forecast
  
 
  
  # PLOTTING TABLE FOR ARIMA NONSEASONAL -->
  output$arima_non_seasonal_table <- renderTable({
    
    # processes after predict button pressed
    data <- eventReactive(input$click, {
      (input$StockCode)
    })
    Stock <- as.character(data())
    print(Stock)
    #Get data from yahoo
    Stock_data_frame <- as.data.frame(getSymbols(
      Symbols = Stock,
      src = "yahoo",
      from = from_data,
      to = to_data,
      env = NULL
    ))
    
    Stock_data_frame$Open = Stock_data_frame[, 1]
    Stock_data_frame$High = Stock_data_frame[, 2]
    Stock_data_frame$Low = Stock_data_frame[, 3]
    Stock_data_frame$Close = Stock_data_frame[, 4]
    Stock_data_frame$Volume = Stock_data_frame[, 5]
    Stock_data_frame$Adj = Stock_data_frame[, 6]
    Stock_data_frame <- Stock_data_frame[, c(7, 8, 9, 10, 11, 12)]
    #moving avareage  7, 30 taken to forecast 
    Stock_data_frame$v7_MA = ma(Stock_data_frame$Close, order = 7)
    Stock_data_frame$v30_MA <- ma(Stock_data_frame$Close, order = 30)
    
    # Seasonal & Trend Decomposition (STL)
    rental_ma <- ts(na.omit(Stock_data_frame$v7_MA), frequency = 30)
    decomp_rental <- stl(rental_ma, s.window = "periodic")
    adj_rental <- seasadj(decomp_rental)
    #model auto arima
    #fit <- auto.arima(Stock_data_frame$Close, ic = "bic")
    fit.Arima <- Arima(Stock_data_frame$Close, order = c(1,3,3))
    fit.forecast <- forecast(fit.Arima, h =10)
    
    #table of forecast
    (fit.forecast)
    
  })# end of plot for ARIMA nonseasonal table


  
  # PLOTTING FORECAST FOR ARIMA SEASONAL -->
  output$arima_seasonal_plot <- renderPlot({
    
    # processes after go button pressed
    data <- eventReactive(input$click, {
      (input$StockCode)
    })
    #Get data from yahoo
    Stock <- as.character(data())
    print(Stock)
    Stock_data_frame <- as.data.frame(getSymbols(
      Symbols = Stock,
      src = "yahoo",
      from = from_data,
      to = to_data,
      env = NULL
    ))
    
    Stock_data_frame$Open = Stock_data_frame[, 1]
    Stock_data_frame$High = Stock_data_frame[, 2]
    Stock_data_frame$Low = Stock_data_frame[, 3]
    Stock_data_frame$Close = Stock_data_frame[, 4]
    Stock_data_frame$Volume = Stock_data_frame[, 5]
    Stock_data_frame$Adj = Stock_data_frame[, 6]
    Stock_data_frame <- Stock_data_frame[, c(7, 8, 9, 10, 11, 12)]
    #moving avareage  7, 30 taken to forecast 
    Stock_data_frame$v7_MA = ma(Stock_data_frame$Close, order = 7)
    Stock_data_frame$v30_MA <- ma(Stock_data_frame$Close, order = 30)
    
    # Seasonal & Trend Decomposition (STL)
    rental_ma <- ts(na.omit(Stock_data_frame$v7_MA), frequency = 30)
    decomp_rental <- stl(rental_ma, s.window = "periodic")
    adj_rental <- seasadj(decomp_rental)

    #model auto arima
    fit_s <- auto.arima(adj_rental, seasonal = TRUE)
    fcast_s <- forecast(fit_s, h = days_FC)
    #plot forecast
    plot(fcast_s,main = Stock , xlab = "TIME (DAYS)",
         ylab = "PRICE (USD $)")
       
    fcast_s
    
  })# end of plot for ARIMA seasonal forecast
  
  
  
  # PLOTTING TABLE FOR ARIMA SEASONAL -->
  output$arima_seasonal_table <- renderTable({
    
    # processes after go button pressed
    data <- eventReactive(input$click, {
      (input$StockCode)
    })
    
    Stock <- as.character(data())
    #Get data from yahoo

    Stock_data_frame <- as.data.frame(getSymbols(
      Symbols = Stock,
      src = "yahoo",
      from = from_data,
      to = to_data,
      env = NULL
    ))
    
    Stock_data_frame$Open = Stock_data_frame[, 1]
    Stock_data_frame$High = Stock_data_frame[, 2]
    Stock_data_frame$Low = Stock_data_frame[, 3]
    Stock_data_frame$Close = Stock_data_frame[, 4]
    Stock_data_frame$Volume = Stock_data_frame[, 5]
    Stock_data_frame$Adj = Stock_data_frame[, 6]
    Stock_data_frame <- Stock_data_frame[, c(7, 8, 9, 10, 11, 12)]
    #moving avareage  7, 30 taken to forecast 
    Stock_data_frame$v7_MA = ma(Stock_data_frame$Close, order = 7)
    Stock_data_frame$v30_MA <- ma(Stock_data_frame$Close, order = 30)
    
    # Seasonal & Trend Decomposition (STL)
    rental_ma <- ts(na.omit(Stock_data_frame$v7_MA), frequency = 30)
    decomp_rental <- stl(rental_ma, s.window = "periodic")
    adj_rental <- seasadj(decomp_rental)
    #model auto arima
    fit_s <- auto.arima(adj_rental, seasonal = TRUE)
    fcast_s <- forecast(fit_s, h =10)
    
    #table of forecast
    fcast_s
    
  })# end of plot for ARIMA seasonal table
  
  
  
  # PLOTTING LSTM FORECAST WITH ORIGINAL DATA -->
  output$lstm_plot <- renderPlot({

    rm(list = ls())
    
    # processes after go button pressed
    data1 <- eventReactive(input$click, {
      (input$StockCode)
    })
    
    Stock <- as.character(data1())
    #Get data from yahoo
    Stock_data_frame <-
      as.data.frame(
        getSymbols(
          Symbols = Stock,
          auto.assign = FALSE,
          src = "yahoo",
          from = from_data,
          to = to_data,
          env = NULL
        )
      )
    
    #select column 4 $close as  data
    stock_close_data <-
      data.frame(index(Stock_data_frame), as.numeric(Stock_data_frame[, 4])) 
    
    #  lagged dataset
    lag_1 <- Lag(stock_close_data[, 2], k = 1)
    lag_2 <- Lag(stock_close_data[, 2], k = 2)
    lag_3 <- Lag(stock_close_data[, 2], k = 3)
    lag_4 <- lag(stock_close_data[, 2], k = 4)
    
    stock_close_data <-
      data.frame(
        actual = stock_close_data[, 2],
        Lag1 = lag_1,
        Lag2 = lag_2,
        Lag3 = lag_3,
        lag4 = lag_4
      )
    stock_close_data <- stock_close_data[4:nrow(stock_close_data), ]
    
    
    #convert data in 0,1
    data_range <- function(x) {
      (x - min(x)) / (max(x) - min(x))
    }
    #convert timeseris data to 0,1 in matrix form
    stock_close_matrix <- as.matrix(sapply(stock_close_data, data_range))
    
    # split_dataset train and test data
    split_dataset <- initial_time_split(as.data.frame(stock_close_matrix), prop = 0.70)
    
    train_data <- training(split_dataset)
    test_data <- testing(split_dataset)
    #random seeding
    set.seed(40)
    
    
    #tranning data
    x_train <- as.matrix(train_data[, 2:5])
    y_train <- as.matrix(train_data[, 1])
    #testing data
    x_test <- as.matrix(test_data[, 2:5])
    y_test <- as.matrix(test_data[, 1])
    
    
    dim(x_train) <- c(nrow(x_train), ncol(x_train), 1)
    dim(x_test) <- c(nrow(x_test), ncol(x_test), 1)
    
    #model initializing ofr LSTM
    model <- keras_model_sequential()
    
    
    model %>%
      # LSTM  layers
      layer_lstm(30, input_shape = c(ncol(x_train), 1), activation = "relu") %>%
      layer_dense(units = 30, activation = "relu") %>% 
      layer_dense(units = 14) %>%
      layer_dense(units = 5) %>%
      #LSTM output layer
      layer_dense(units = 1, activation = "linear")
    #compile Neural Network optimizer
    model %>% compile(loss = "mae",
                      optimizer = "RMSprop",
                      metrics = c( "mae")
                      
    )
    #fit model
    model %>% fit(
      x_train,
      y_train,
      epochs =100,
      batch_size = 20,
      shuffle = F
      
    )
    #unscale function for data
    data_unscale <- function(x, max_x, min_x) {
      x * (max_x - min_x) + min_x
    }
    
    #lstm prediction
    forecast_lstm = model %>% predict(x_test)
    forecasted_lstm <-
      data_unscale(forecast_lstm, max(stock_close_data[, 1]), min(stock_close_data[, 1]))
    
    
    #plot with forecast and original data
    plot(
      stock_close_data[nrow(train_data):nrow(stock_close_data) + 10, 1],
      type = "l",
      xlab = "TIME (DAYS)",
      ylab = "PRICE (USD $)",
      main = Stock
    )
    axis(1, at = seq(0, length(forecasted_lstm)+10, by = 5), las=1)
    lines(forecasted_lstm, type = "l", col = "red")
    
  })# end of plotting LSTM model with original data
  
  
  
  # PLOTTING LSTM FORECAST -->
  output$lstm_gg <- renderPlotly({

    rm(list = ls())
    
    # processes after go button pressed
    data2 <- eventReactive(input$click, {
      (input$StockCode)
    })
    
    Stock <- as.character(data2())
    Stock_data_frame <-
      as.data.frame(
        getSymbols(
          Symbols = Stock,
          auto.assign = FALSE,
          src = "yahoo",
          from = from_data,
          to = to_data,
          env = NULL
        )
      )
    
    #select column 4 $close as  data
    stock_close_data <-
      data.frame(index(Stock_data_frame), as.numeric(Stock_data_frame[, 4])) 
    
    #  lagged dataset
    lag_1 <- Lag(stock_close_data[, 2], k = 1)
    lag_2 <- Lag(stock_close_data[, 2], k = 2)
    lag_3 <- Lag(stock_close_data[, 2], k = 3)
    lag_4 <- lag(stock_close_data[, 2], k = 4)
    
    stock_close_data <-
      data.frame(
        actual = stock_close_data[, 2],
        Lag1 = lag_1,
        Lag2 = lag_2,
        Lag3 = lag_3,
        lag4 = lag_4
      )
    stock_close_data <- stock_close_data[4:nrow(stock_close_data), ]
    
    
    #convert data in 0,1
    data_range <- function(x) {
      (x - min(x)) / (max(x) - min(x))
    }
    #convert timeseris data to 0,1 in matrix form
    stock_close_matrix <- as.matrix(sapply(stock_close_data, data_range))
    
    # split_dataset train and test data
    split_dataset <- initial_time_split(as.data.frame(stock_close_matrix), prop = 0.70)
    
    train_data <- training(split_dataset)
    test_data <- testing(split_dataset)
    #random seeding
    set.seed(40)
    
    
    #tranning data
    x_train <- as.matrix(train_data[, 2:5])
    y_train <- as.matrix(train_data[, 1])
    #testing data
    x_test <- as.matrix(test_data[, 2:5])
    y_test <- as.matrix(test_data[, 1])
    
    
    dim(x_train) <- c(nrow(x_train), ncol(x_train), 1)
    dim(x_test) <- c(nrow(x_test), ncol(x_test), 1)
    
    #model initializing ofr LSTM
    model <- keras_model_sequential()
    
    
    model %>%
      # LSTM  layers
      layer_lstm(30, input_shape = c(ncol(x_train), 1), activation = "relu") %>%
      layer_dense(units = 30, activation = "relu") %>% 
      layer_dense(units = 14) %>%
      layer_dense(units = 5) %>%
      #LSTM output layer
      layer_dense(units = 1, activation = "linear")
    #compile Neural Network optimizer
    model %>% compile(loss = "mae",
                      optimizer = "RMSprop",
                      metrics = c( "mae")
                      
    )
    #fit model
    model %>% fit(
      x_train,
      y_train,
      epochs =100,
      batch_size = 20,
      shuffle = F
      
    )
    #unscale function for data
    data_unscale <- function(x, max_x, min_x) {
      x * (max_x - min_x) + min_x
    }
    
    #lstm prediction
    forecast_lstm = model %>% predict(x_test)
    forecasted_lstm <-
      data_unscale(forecast_lstm, max(stock_close_data[, 1]), min(stock_close_data[, 1]))
    
    
    #plots
    #ggploty with plotly
   
    p2 <-
      ggplot(NULL, aes(x = 1:length(forecasted_lstm) + days_FC, y = forecasted_lstm)) + xlab("TIME (DAYS)") +
      ylab("PRICE (USD $)") +ggtitle(Stock)+theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = c(seq (from = 0, to = length(forecasted_lstm)+days_FC, by= 5)))+
      geom_line(colour = "Blue")
    
    ggplotly(p2)
    
    
  })# end of LSTM forecast plot

  
}# end of server


shinyApp(ui, server)

