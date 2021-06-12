library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
#pull data from yahoo January 1, 2018 to December 31, 2020
 
getSymbols('SNY',src="yahoo", from='2018-01-01' , to='2020-12-31')
class(SNY)
Sny_Close_Prices =SNY[,4]
plot(Sny_Close_Prices)
class(Sny_Close_Prices)
# graph the acf and pacf looking for identifiable lags pacf-> p acf-> Q for custom arimas 
par(mfrow=c(1,2))
Acf(Sny_Close_Prices, main= 'ACF for Differenced Series')
Pacf(Sny_Close_Prices, main='pacf for differenced Serirs')

# test findings on originalXTS objects 
# ADF test for p-value 
print(adf.test(Sny_Close_Prices)) #p-value = 0.01
auto.arima(Sny_Close_Prices, seasonal = FALSE) #ARIMA(0,1,0) AIC/BIC=1617.43/1622.05

fitA=auto.arima(Sny_Close_Prices,seasonal = FALSE)#custom arma
tsdisplay(residuals(fitA), lag.max = 30 , main = '(0,1,0) model residuals')
auto.arima(Sny_Close_Prices,seasonal = FALSE)

fitB=arima(Sny_Close_Prices,order = c(1,2,1))
tsdisplay(residuals(fitA), lag.max = 30 , main = '(1,2,1) model residuals')

fitC=arima(Sny_Close_Prices,order = c(5,3,2))
tsdisplay(residuals(fitA), lag.max = 30 , main = '(5,3,2) model residuals')

fitD=arima(Sny_Close_Prices,order = c(1,1,1))
tsdisplay(residuals(fitA), lag.max = 30 , main = '(1,1,1) model residuals')

# plot the arima models 
par(mfrow=c(2,2))
#auto arima
term<-60
fcast1<- forecast(fitA, h=term)
plot(fcast1)
#custom arima
fcast2<- forecast(fitB, h=term)
plot(fcast2)
fcast3<- forecast(fitC, h=term)
plot(fcast3)
fcast4<- forecast(fitD, h=term)
plot(fcast4)

# Mape accuracy subtract from 100.
accuracy(fcast1) #99.488%
accuracy(fcast2) #99.489%
accuracy(fcast3) #99.482%
accuracy(fcast4) #99.489%

# conclusion - by looking at fcast1 and fcast2 model for next 60 days trand is going upwords or constant and accurancy
# also 99.48% .
