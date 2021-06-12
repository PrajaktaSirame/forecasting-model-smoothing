#2)Generate a forecast from January 2016 to December 2020 and Plot them (the raw data and the
#forecast) on a chart 
data<- read.csv("C:\\Users\\Prajakta\\OneDrive\\Desktop\\homework\\time series\\final exam\\Chocolate_Sales.csv")
df <- ts (data$Sales,frequency = 12, start = c(2016), end = c(2020))
plot(df)
# forecasting for data time period for 2016-2020
fcast_data_sarima<- forecast(auto.arima(df, D= 1),h=6)
plot(fcast_data_sarima)
fcast_data_sarima


#3)Produce forecasts from January 2021 to December 2021.and Plot the forecast data on a chart.
chart <- ts (data$Sales,frequency = 12, start = c(2021))
plot(chart)
# forecasting for data time period for 2016-2020
fcast_data_sarima<- forecast(auto.arima(chart, D= 1),h=6)
plot(fcast_data_sarima)
fcast_data_sarima

#1) Develop your forecasting model and Justifying its selection over Forecast models.
#4)Compute and analyse the residuals from the selected model and compute the forecasting
#model. Accuracy


# the full model forcasting is done .
tsdata <- ts(data$Sales,frequency =60, start =2018)
plot(tsdata)
Acf(tsdata, main= 'ACF for tSeries')


plot(decompose(ts(data$Sales, frequency = 12)))

library(forecast)
fcast_data <- forecast(auto.arima(ts(data$Sales)), h= 6)
fcast_data
par(mfrow=c(1,1))
plot(fcast_data)


# ARIMA Model doent work with seasonal data and we cant get proper
#forcasting for the data .to resolve this problem we are using SARIMA model 
# seasonal auto regressive integrated moving average .

fcast_data_sarima<- forecast(auto.arima(ts(data$Sales, frequency = 12), D= 1),h=6)
plot(fcast_data_sarima)
fcast_data_sarima

plot(fcast_data_sarima$residuals)

accuracy(fcast_data_sarima) # accuracy is 98.46% 

# by using SARIMA model we get perfect forecast so in sarima model we get 
#(0,1,1)arima model and (0,1,1) seasonal model with 12 drift .so it forced
# ARIMA model to be more seasonal .
