
##### Import Library and install packages ######

install.packages("forecast")
install.packages("TTR")
install.packages("TSA")

library("forecast")
library("readxl")



#Question 1

#############   ***  Chocolate Web_Visitors ***   #####################

#a) Develop your forecasting model and justifying its selection over other Exponential smoothing models. (5.points)

# read data from excel:

visitors<- read.csv("C:\\Users\\Prajakta\\OneDrive\\Desktop\\homework\\time series\\final exam\\Web-Visitors.csv")

#plotting the time series data, frequency=184 as number of days and 3 is used as august comes in the 
visitorsTimeSeries <- ts(visitors$Visitors,frequency=184, start=c(2020,3))
visitorsTimeSeries
plot.ts(visitorsTimeSeries)


# calculating the natural log of the original data 
logVisitors <- log(visitorsTimeSeries)
plot.ts(logVisitors)


#***********SIMPLE EXPONENTIAL SMOOTHING :****************

simpleExponentialSmoothing <- HoltWinters(visitorsTimeSeries, beta = FALSE, gamma = FALSE)
simpleExponentialSmoothing

simpleExponentialSmoothing$fitted
simpleExponentialSmoothing$SSE
plot(simpleExponentialSmoothing)

#Holt-Winters exponential smoothing without trend and without seasonal component.


#Smoothing parameters:
#alpha: 0.3816797
#beta : FALSE
#gamma: FALSE

#Coefficients:
#[,1]
#a 6260.947

#******calculating the natural log of the original data using Simple exponential smoothing****

simpleExponential <- HoltWinters(logVisitors, beta = FALSE, gamma = FALSE)
simpleExponential

#simpleExponential
#Holt-Winters exponential smoothing without trend and without seasonal component.

#Smoothing parameters:
  #alpha: 0.4270361
#beta : FALSE
#gamma: FALSE

#Coefficients:
  #[,1]
#a 8.745193




#**********DOUBLE EXPONENTIAL SMOOTHING :****************

doubleExponentialSmoothing <- HoltWinters(visitorsTimeSeries, gamma = FALSE)
doubleExponentialSmoothing


doubleExponentialSmoothing$fitted
doubleExponentialSmoothing$SSE
plot(doubleExponentialSmoothing)

#Holt-Winters exponential smoothing with trend and without seasonal component.

#Smoothing parameters:
 # alpha: 0.407916
#beta : 0.03434997
#gamma: FALSE
#Coefficients:
 # [,1]
#a 6288.51282
#b   15.92989

#******calculating the natural log of the original data using double exponential smoothing****


doubleExponential <- HoltWinters(logVisitors, gamma = FALSE)
doubleExponential

#Holt-Winters exponential smoothing with trend and without seasonal component.
#Smoothing parameters:
 # alpha: 0.4717226
#beta : 0.07093266
#gamma: FALSE

#Coefficients:
  #[,1]
#a 8.750537284
#b 0.003995497


#*********TRIPLE EXPONENTIAL SMOOTHING :****************

tripleExponential <- HoltWinters(visitorsTimeSeries)

# OUTPUT-->  time series has no or less than 2 periods

logVisitorsforecasts <- HoltWinters(logVisitors)
logVisitorsforecasts

logVisitorsforecasts$fitted
logVisitorsforecasts$SSE
plot(logVisitorsforecasts)


# logVisitorsforecasts
#Holt-Winters exponential smoothing with trend and additive seasonal component.

#Call:
#HoltWinters(x = logVisitors)

#Smoothing parameters:
#alpha: 0.9950746
#beta : 0
#gamma: 0

#Coefficients:
#[,1]


##Conclusion, We go with *** Double exponential smoothing technique *** as we have trend and 
#without seasonal component.

#Smoothing parameters:

# alpha: 0.407916
#beta : 0.03434997
# the estimated values alpha(0.41) estimates the level at current time point based on the recent obs 
#and some obs in the more distant past.The value of beta is 0.00, indicating that the estimate of the
#slope b of the trend component is not updated over the time series, and instead is set equal to
#its initial value. as the level changes quite a bit over the time series,
# but the slope b of the trend component remains roughly the same. 

### b)Generate a forecast from August, 1st 2020 to January, 31st 2021 and Plot them (the raw data and the forecast) on a chart (10 points)



doubleExponentialSmoothing$SSE # sum-of-squared-errors for the forecast errors is 51844642
plot(doubleExponentialSmoothing)



#########################################################################################################
### c) Produce forecasts from February 1st. 2021 to February 28, 2021 and Plot the forecast data on a chart (10 points)

expSmooth_fc2 <- forecast(doubleExponential, h=28)
expSmooth_fc2
plot(expSmooth_fc2)

########################################################################################################
##d) Determine the residuals from the selected model and compute the accuracy of your forecasting model.

plot(expSmooth_fc2$residuals)
#plot the residuals (sample vs theoretical)
qqnorm(expSmooth_fc2$residuals)

#### get the accuracy by MAPE and other leading factors (method 1)
summary(autoarimal)
