#load data and packages
library(dplyr)
library(forecast)
library(ggplot2)
library(tidyr)
library(patchwork)

set.seed(123)
# load("Data/Logan_River_Train.RData")
# 
# load("Data/Logan_River_Test.RData")

Logan_River_train <- Logan_River_Updated_Train
Logan_River_test <- Logan_River_Updated_Test


#TRAINING, cleaning and using only the Franklin location
Logan_River_train %>%
  group_by(source) %>%
  summarise(n_less = sum(DO < 10, na.rm = TRUE))

Logan_River_Franklin <- Logan_River_train %>%
  mutate(across(where(is.numeric),
                ~ ifelse(. < 0, NA, .))) %>%
  filter(grepl("Franklin", source))


summary(Logan_River_Franklin)



#I don't know if this function is working properly
daily_DO <- Logan_River_Franklin %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize(DO_min = min(DO, na.rm = TRUE),
            DO_mean = mean(DO, na.rm = TRUE),
            DO_max = max(DO, na.rm = TRUE),
            discharge_ave = mean(discharge, na.rm = TRUE)
  ) %>%
  filter(is.finite(DO_min),
         is.finite(DO_mean),
         is.finite(DO_max),
         is.finite(discharge_ave))

#TESTING, cleaning and using only the Franklin Basin location
Logan_River_test %>%
  group_by(source) %>%
  summarise(n_less = sum(DO < 10, na.rm = TRUE))

Logan_River_Franklin_Test <- Logan_River_test %>%
  mutate(across(where(is.numeric),
                ~ ifelse(. < 0, NA, .))) %>%
  filter(grepl("Franklin", source))

summary(Logan_River_Franklin_Test)

daily_DO_Test <- Logan_River_Franklin_Test %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize(DO_min = min(DO, na.rm = TRUE),
            DO_mean = mean(DO, na.rm = TRUE),
            DO_max = max(DO, na.rm = TRUE),
            discharge_ave = mean(discharge, na.rm = TRUE)) %>%
  filter(is.finite(DO_min),
         is.finite(DO_mean),
         is.finite(DO_max),
         is.finite(discharge_ave))

summary(daily_DO_Test)


head(daily_DO_Test)





###################   SEASONAL MODEL   ############################

#MINIMUM
ggplot(daily_DO, aes(x = date, y = DO_min))+
  geom_point()

#Model Fitting
ARIMA_Franklin_min <- auto.arima(ts(daily_DO$DO_min, frequency = 365), seasonal = TRUE)

###Interpret this!
summary(ARIMA_Franklin_min)

saveRDS(ARIMA_Franklin_min, file = "ARIMA_Franklin_min.rds")



  forecast_week_min <- forecast(ARIMA_Franklin_min, h = 7)
plot(forecast_week_min)


#MEAN, this model is fit, but not used in testing
ggplot(daily_DO, aes(x = date, y = DO_mean))+
  geom_point()

#Model Fitting
#ARIMA_mean <- auto.arima(ts(daily_DO$DO_mean, frequency = 365), seasonal = TRUE)
#summary(ARIMA_mean)
#saveRDS(ARIMA_mean, file = "ARIMA_mean.rds")

#mean one week prediction
# forecast_week_mean <- forecast(ARIMA_mean, h = 7)
# plot(forecast_week_mean)








###############   SEASONAL MODEL WITH RMSE   #######################
#PREDICT

forecast_min_weekly <- list()

for(week in 1:52){
  new_data <- daily_DO_Test$DO_min[seq_len((week-1)*7)] #make dataframe to add in covariate data?
  
  combined <- c(daily_DO$DO_min, new_data)
  
  ts_data <- ts(combined, start = c(2014,1), frequency = 365)
  
  temp_fit <- Arima(ts_data, model = ARIMA_Franklin_min) #make sure that argument y= new week of y data, 
  #and xreg = new x predictors (discharge, etc...)
  
  predict_seven <- predict(temp_fit, n.ahead = 7) #predict function isn't flexible enough to 
 #add in an argument newxreg for things like discharge, that are x covariates
  
  forecast_min_weekly[[week]] <- data.frame(
    pred = predict_seven$pred,
    se = predict_seven$se,
    week = week,
    forecast_day = seq(1:7)
  )
}

forecast_min_weekly_df <- bind_rows(forecast_min_weekly)
summary(forecast_min_weekly_df)

forecast_min_weekly_df <- forecast_min_weekly_df %>%
  mutate(day = 1:n())

ggplot(data = forecast_min_weekly_df, aes(x = day, y = pred))+
  geom_line()


#RMSE of day of the week


#Forecast Day Averages
forecast_day_ave <- forecast_min_weekly_df %>%
  mutate(
    obs = daily_DO_Test$DO_min[1:364],
    week = rep(1:52, each = 7),
    day = rep(1:7, times = 52)
  ) %>%
  group_by(day) %>%
  summarise(
    rmse = sqrt(mean((obs - pred)^2, na.rm = TRUE))
  )


#graph
ggplot(forecast_day_ave, aes(x = day, y = rmse))+
  geom_point()





###################  RMSE WITH DISCHARGE--START HERE  ########################

#Add lag (using previous week's discharge level)
lag_df <- daily_DO %>%
  mutate(discharge_lag = lag(discharge_ave, 7)) %>%
  filter(!is.na(discharge_lag))

#Model 
ARIMAX_Franklin_min <- auto.arima(ts(lag_df$DO_min, frequency = 365), xreg = 
                           lag_df$discharge_lag,  seasonal = TRUE)
saveRDS(ARIMAX_Franklin_min, file = "ARIMAX_Franklin_min.rds")
summary(ARIMAX_Franklin_min)

#Forecasting


forx_min_weekly <- list()

for(week in 1:52){
  new_data <- daily_DO_Test$DO_min[seq_len((week-1)*7)]
  combined <- c(lag_df$DO_min, new_data)
  
  newx <- lag(daily_DO_Test$discharge_ave, 7)[seq_len((week - 1) * 7)]
  combinedx <- c(lag_df$discharge_lag, newx)
  
  ts_data <- ts(combined, start = c(2014,1), frequency = 365)
  temp_fit <- Arima(ts_data, model = ARIMAX_Franklin_min, xreg = combinedx)
  # y should be a column turned into a time series
  #xreg can be a data frame? or a matrix. 
  
  full_discharge <- c(daily_DO$discharge_ave,
                      daily_DO_Test$discharge_ave[seq_len((week - 1) * 7)])
  
  future_xreg <- tail(full_discharge, 7)
  
  predict_seven <- predict(temp_fit, n.ahead = 7, newxreg = future_xreg)
  
  forx_min_weekly[[week]] <- data.frame(
    pred = predict_seven$pred,
    se = predict_seven$se,
    week = week,
    forecast_day = seq(1:7)
  )
}

forx_min_weekly_df <- bind_rows(forx_min_weekly)
summary(forx_min_weekly_df)

forx_min_weekly_df <- forx_min_weekly_df %>%
  mutate(day = 1:n())

ggplot(data = forx_min_weekly_df, aes(x = day, y = pred))+
  geom_line()










######## FOURIER MODELING ATTEMPTS ###########
library(forecast)
# ?fourier
# ?ts


#original model
ARIMAX_Franklin_min <- auto.arima(ts(lag_df$DO_min, frequency = 365), xreg = 
                                    lag_df$discharge_lag,  seasonal = TRUE)

#converted:
library(forecast)
min_ts <- ts(lag_df$DO_min, frequency = 365)
K <- 5
fourier_terms <- fourier(min_ts, K = K)


xreg <- cbind(
  discharge_lag = lag_df$discharge_lag,
  fourier_terms
)


ARIMAX_Franklin_min_fourier <- auto.arima(
  min_ts,
  xreg = xreg,
  seasonal = FALSE
)

summary(ARIMAX_Franklin_min_fourier)









###ARIMAX FUNCTION
library(forecast)
library(dplyr)
library(tidyverse)

arimamodel <- function(K, freq, lag_df, lagged_covariates, refresh, start){
  
  min_ts <- ts(lag_df$DO_min, start = c(start[1], start[2] + refresh),
               frequency = freq)
  
  fourier_terms <- fourier(min_ts, K = K)

    #this command I just added in, if it breaks look here first. 
  #I am trying to make it so the dataframe doesn't have to have lagged cols yet.
  # lag_df <- lag_df %>%
  #   mutate(lag_df[lagged_covariates] = lag(lag_df[lagged_covariates], 7)) %>%
  #   filter(!is.na(lag_df[lagged_covariates]))
  
  xreg <- cbind(
    as.matrix(lag_df[lagged_covariates]),
    fourier_terms
  )
  
  ARIMAX_fourier_model <- auto.arima(
    min_ts,
    xreg = xreg,
    seasonal = FALSE)
  
  ARIMAX_fourier_model
}

arimamodel(1, 365, lag_df, lagged_covariates, 7)




###PREDICTION FUNCTION

running_forecast <- function(fit_data, new_data, K, lagged_covariates,
                             refresh, freq, length=NULL){
  if(is.null(length)){
    length <- nrow(new_lag_data)}

  #creating new lagged variables, filtering out "refresh" number of starting rows
  forx_min_weekly <- list()
  lag_df <-  fit_data %>%
    mutate(across(
      all_of(lagged_covariates),
      ~ lag(.x, refresh),
      .names = "{.col}_lag"
    )) %>%
  filter(if_all(ends_with("_lag"), ~ !is.na(.x)))
  
  #model = ARIMAX_Franklin_min
  ARIMAX_baseline <- arimamodel(K, freq, lag_df, lagged_covariates)
  ARIMAX_baseline
  
  #make it easier to call all lagged vars
  lagged_columns <- grep("_lag$", names(lag_df), value = TRUE)
length <- 1
  for(period in 1:length){
    
    new_y <- new_data$DO_min[seq_len((period-1)*refresh)]
    combined_y <- c(lag_df$DO_min, new_y)
    
    # Combine original, unlagged predictors
    combined_covariates <- rbind(
      lag_df[, lagged_covariates, drop = FALSE],
      new_data[seq_len((period - 1) * refresh),
               lagged_covariates,
               drop = FALSE]
    )
    
    # Lag the entire combined series
    combinedx <- dplyr::lag(
      combined_covariates,
      n = refresh
    )
    
    # Rename columns
    names(combinedx) <- lagged_columns
    
    ts_data <- ts(combined_y, start = c(2014,1), frequency = freq)
    
    # Generate Fourier terms
    fourier_terms <- fourier(ts_data, K = K)
    
    # Combine environmental predictors and Fourier terms
    combinedx <- cbind(as.matrix(combinedx), fourier_terms)
   
    # Fit ARIMAX
    temp_fit <- Arima(
      ts_data,
      model = ARIMAX_baseline,
      xreg = combinedx
    )
    
    future_fourier <- fourier(
      ts_data,
      K = K,
      h = refresh)
    
    future_covariates <- as.matrix(tail(full_covariates, refresh))
    
    future_xreg <- cbind(
      as.matrix(future_covariates),
      future_fourier
    )
    
    predict_ahead <- predict(
      temp_fit,
      n.ahead = refresh,
      newxreg = future_xreg
    )
    
  
  forx_min_weekly[[period]] <- data.frame(
    pred = predict_ahead$pred,
    se = predict_ahead$se,
    period = period,
    forecast_day = seq(1:refresh))
  }
}


#test for multiple variables
obj <- running_forecast(daily_DO, daily_DO_Test, 1, 
                        c("discharge_ave", "DO_max"), 2, 52)

#copy of Emma's code above for reference
forx_min_weekly <- list()
for(week in 1:52){
  new_data <- daily_DO_Test$DO_min[seq_len((week-1)*7)]
  combined <- c(lag_df$DO_min, new_data)
  
  newx <- lag(daily_DO_Test$discharge_ave, 7)[seq_len((week - 1) * 7)]
  combinedx <- c(lag_df$discharge_lag, newx)
  
  ts_data <- ts(combined, start = c(2014,1), frequency = 365)
  temp_fit <- Arima(ts_data, model = ARIMAX_Franklin_min, xreg = combinedx)
  # y should be a column turned into a time series
  #xreg can be a data frame? or a matrix. 
  
  full_discharge <- c(daily_DO$discharge_ave,
                      daily_DO_Test$discharge_ave[seq_len((week - 1) * 7)])
  
  future_xreg <- tail(full_discharge, 7)
  
  predict_seven <- predict(temp_fit, n.ahead = 7, newxreg = future_xreg)
  
  forx_min_weekly[[week]] <- data.frame(
    pred = predict_seven$pred,
    se = predict_seven$se,
    week = week,
    forecast_day = seq(1:7)
  )
}

forx_min_weekly_df <- bind_rows(forx_min_weekly)
summary(forx_min_weekly_df)

forx_min_weekly_df <- forx_min_weekly_df %>%
  mutate(day = 1:n())

ggplot(data = forx_min_weekly_df, aes(x = day, y = pred))+
  geom_line()











#TRY THIS:
#debugonce(running_forecast)
#running_forecast(...)

###CHAT'S RECOMMENDED FIXES

# running_forecast2 <- function(fit_data, new_data, K, lagged_covariates,
#                              refresh, freq, start = c(2014, 1)) {
#   
#   # Identify lagged predictor names
#   lagged_columns <- paste0(lagged_covariates, "_lag")
#   
#   # Number of forecasting periods
#   n_periods <- ceiling(nrow(new_data) / refresh)
#   
#   # Store forecasts
#   forx_min_weekly <- list()
#   
#   # --------------------------------------------------
#   # 1. Create initial lagged training data
#   # --------------------------------------------------
#   
#   initial_lags <- as.data.frame(
#     lapply(
#       fit_data[, lagged_covariates, drop = FALSE],
#       dplyr::lag,
#       n = refresh
#     )
#   )
#   
#   names(initial_lags) <- lagged_columns
#   
#   # Remove initial missing observations
#   initial_lags <- initial_lags[-seq_len(refresh), , drop = FALSE]
#   
#   initial_y <- fit_data$DO_min[-seq_len(refresh)]
#   
#   lag_df <- data.frame(
#     DO_min = initial_y,
#     initial_lags
#   )
#   
#   # --------------------------------------------------
#   # 2. Fit initial ARIMAX model
#   # --------------------------------------------------
#   
#   ARIMAX_baseline <- arimamodel(
#     K, freq, lag_df, lagged_covariates
#   )
#   
#   # --------------------------------------------------
#   # 3. Weekly forecasting loop
#   # --------------------------------------------------
#   
#   for (period in seq_len(n_periods)) {
#     
#     # Number of newly observed observations available
#     n_observed <- min(
#       (period - 1) * refresh,
#       nrow(new_data)
#     )
#     
#     # --------------------------------------------------
#     # Combine response data
#     # --------------------------------------------------
#     
#     combined_y <- c(
#       fit_data$DO_min,
#       new_data$DO_min[seq_len(n_observed)]
#     )
#     
#     # --------------------------------------------------
#     # Combine RAW environmental predictors
#     # --------------------------------------------------
#     
#     full_covariates <- rbind(
#       fit_data[, lagged_covariates, drop = FALSE],
#       new_data[
#         seq_len(n_observed),
#         lagged_covariates,
#         drop = FALSE
#       ]
#     )
#     
#     # --------------------------------------------------
#     # Calculate lags across ENTIRE combined dataset
#     # --------------------------------------------------
#     
#     combinedx <- as.data.frame(
#       lapply(
#         full_covariates,
#         dplyr::lag,
#         n = refresh
#       )
#     )
#     
#     names(combinedx) <- lagged_columns
#     
#     # Remove initial missing lag observations
#     combinedx <- combinedx[-seq_len(refresh), , drop = FALSE]
#     
#     combined_y <- combined_y[-seq_len(refresh)]
#     
#     # --------------------------------------------------
#     # Create time series
#     # --------------------------------------------------
#     
#     ts_data <- ts(
#       combined_y,
#       start = c(start[1], start[2] + refresh),
#       frequency = freq
#     )
#     
#     # --------------------------------------------------
#     # Generate Fourier terms
#     # --------------------------------------------------
#     
#     fourier_terms <- fourier(
#       ts_data,
#       K = K
#     )
#     
#     # Combine environmental and seasonal predictors
#     combinedx <- cbind(
#       as.matrix(combinedx),
#       fourier_terms
#     )
#     
#     # --------------------------------------------------
#     # Refit ARIMAX
#     # --------------------------------------------------
#     
#     temp_fit <- Arima(
#       ts_data,
#       model = ARIMAX_baseline,
#       xreg = combinedx
#     )
#     
#     # --------------------------------------------------
#     # Generate future environmental predictors
#     # --------------------------------------------------
#     
#     future_covariates <- as.matrix(
#       tail(full_covariates, refresh)
#     )
#     
#     colnames(future_covariates) <- lagged_columns
#     
#     # --------------------------------------------------
#     # Generate future Fourier terms
#     # --------------------------------------------------
#     
#     future_fourier <- fourier(
#       ts_data,
#       K = K,
#       h = refresh
#     )
#     
#     # Combine future regressors
#     future_xreg <- cbind(
#       future_covariates,
#       future_fourier
#     )
#     
#     # --------------------------------------------------
#     # Forecast
#     # --------------------------------------------------
#     
#     predict_ahead <- predict(
#       temp_fit,
#       n.ahead = refresh,
#       newxreg = future_xreg
#     )
#     
#     # --------------------------------------------------
#     # Store predictions
#     # --------------------------------------------------
#     
#     forx_min_weekly[[period]] <- data.frame(
#       pred = as.numeric(predict_ahead$pred),
#       se = as.numeric(predict_ahead$se),
#       period = period,
#       forecast_day = seq_len(refresh)
#     )
#     
#   }
#   
#   # Combine all forecasting periods
#   return(dplyr::bind_rows(forx_min_weekly))
#   
# }
