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

Logan_River_train <- Logan_River_Updated_train
Logan_River_test <- Logan_River_Updated_test


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
  new_data <- daily_DO_Test$DO_min[seq_len((week-1)*7)]
  
  combined <- c(daily_DO$DO_min, new_data)
  
  ts_data <- ts(combined, start = c(2014,1), frequency = 365)
  
  temp_fit <- Arima(ts_data, model = ARIMA_Franklin_min)
  
  predict_seven <- predict(temp_fit, n.ahead = 7)
  
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

summary(ARIMAX_Franklin_min)









###ARIMAX FUNCTION
library(fourier)

arimamodel <- function(K, freq, lag_df, lagged_covariates){
  min_ts <- ts(lag_df$DO_min, frequency = freq)
  fourier_terms <- fourier(min_ts, K = K)
  
    xreg <- cbind(
      discharge_lag = as.matrix(lag_df[lagged_covariates]),
      fourier_terms
    )
  
  ARIMAX_fourier_model <- auto.arima(
    min_ts,
    xreg = xreg,
    seasonal = FALSE)
  
  ARIMAX_fourier_model
}

arimamodel(1, 365, lag_df, lagged_covariates)
