######################################################
# This files analyzes monthly Passengers Car Registrations Data as Time Series
# 
# Source: https://www.acea.auto/figure/passenger-car-registrations-in-europe-since-1990-by-country/
#
# Created with ♥ by Alberto Frison - July 2022
# Thanks to Adam Check for the great YouTube videos: https://www.youtube.com/watch?v=dBNy_A6Zpcc
# and to Scott Burk for the (quite complex) but inspiring YouTube Lectures: https://www.youtube.com/watch?v=HdYBuDMJ40Y&list=PLX-TyAzMwGs-I3i5uiCin37VFMSy4c50F&index=1
######################################################

##### 
# Section 00 Initialization and Load of Packages
rm (list =ls())   # clears all variables in the workspace
library (fpp2)    # forecasting package


##### 
# Section 01 Load the Data
data <- read.delim ("data/Passengers_Cars_Registrations_Italy.csv", sep = ";") # Passenger Cars Registrations in Italy from 1990 - Today (updated June 2022)

Y_entire <- ts(data[,2], start = c(1990,1), end = c(2022,6), frequency = 12) # declare a new variable Y as Time Series (just the second column)
Y <- window (Y_entire, start = c(2005,1), end = c(2021,12)) # this is the TRAINING SET --- you can play with the start // end of the training set to see what works better


#####
# Section 02 Preliminary Analysis
# Time Plot
autoplot (Y) + 
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registrations") +
  xlab ("Year")
# data looks SEASONAL (to be confirmed) and with a downward TREND


# DIFFERENCE FUNCTION DATA TO GET RID OF THE TREND
DY <- diff(Y) # change of registrations from month to month (feb 1990 - jan 1990)
autoplot (DY) + 
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registration - Month over Month Difference") +
  xlab ("Year")
# DY now contains the month over month changes in the time series

# INVESTIGATING SEASONALITY
# we got rid of the Trend, let's see if there is a monthly seasonality 

# ONE WAY TO LOOK AT SEASONALITY
ggseasonplot (DY) +
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registration - Month over Month Difference")
# there is a clear seasonality in the data with increase of sales in January, as well as March, June and September and a decrease in August

# SECOND WAY TO EXPLORE SEASONALITY
ggsubseriesplot(DY) +
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registration - Month over Month Seasonality")

# CONCLUSION: THE SERIES HAS TREND AND SEASONALITY, WE CAN NOW EXPLORE DIFFERENT WAYS TO FORECAST FOR THE FUTURE


#####
# Section 03  Let's try to forecast
# 01. Benchmark Methods - SEASONAL NAIVE METHOD: T_y = T_(y-s) + Random Error
# Remember to use the DIFFERENCE DATA and not the RAW DATA due to the TREND

fit <- snaive(DY) 
print(summary(fit))   
checkresiduals(fit)
class(fit)
# snaive doesn't look that performs very well, SD is 28k cars and ACF chart suggest lots of autocorrelation

# as alternative to checkresiduals function
res <- residuals (fit)
Acf(res, main ="ACF of Residuals - Naive Method")
hist(res, nclass= "FD", main ="Histogram of Residuals - Naive Method")



#-----------------
# 02. Exponential Smoothing Model 
# We can use REGULAR RAW DATA
fit_ets <- ets(Y)         #tries a number of exponential smoothing models and returns the best
print(summary(fit_ets))   
checkresiduals(fit_ets)


#-----------------
# 03. ARIMA
# Data in ARIMA needs to be STATIONARY: use the Diff data and tell there ise
fit_arima <- auto.arima (Y,d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #we can use the original Y data, using the d=1 parameter
print(summary(fit_arima))   # Residual SD 21293.75 #warning ARIMA returns VARIANCE ==> SD^2  // 21161.46 (1990 - 2020)
checkresiduals(fit_arima)
#-----------------


#####
# Section 04 LET's FORECAST

# Seasonal Naive Forecast
#fcst_snv <- forecast(fit,h=12)
#plot(fcst_snv)

# ETS Forecast
fcst_ets <- forecast(fit_ets,h=6)

# ARIMA Forecast
fcst_arima <- forecast(fit_arima,h=6)


#####
# Check Forecasts against Actuals - Six Months of Data 2022 up to June
actual_data <- window (Y_entire, start = c(2022,1))

# SNAIVE METHOD
#accuracy (fcst_snv,actual_data)
#autoplot(fcst_snv)

# ETS METHOD
accuracy (fcst_ets,actual_data) # 
autoplot(fcst_ets)

# ARIMA METHOD
accuracy (fcst_arima,actual_data) # 
autoplot(fcst_arima)


# PLOTTING 
plot (actual_data, ylim = c (80000,150000), main ="Italian Market - New Car Registrations - Forecast")
points(actual_data, col = "black", pch = 19)
lines(fcst_ets$mean, lty= "solid", col = "red")
points(fcst_ets$mean, col = "red")
lines(fcst_arima$mean, lty="solid", col = "blue")
points(fcst_arima$mean, col = "blue")
legend ("bottomright", legend = c("2022 - Actuals", "ETS Method Forecast", "ARIMA Forecast"), lty = "solid", col = c("black","red", "blue"))


#####
# Trying to get better looking charts... no luck so far...

x_axis <- c("01 - Jan", "02 - Feb", "03 - Mar", "04 - Apr", "05 - May", "06 - Jun")

axis(1, at = 1:6, labels = 1:6)# c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))

ggplot () +
  geom_line(aes (x = x_axis, y = actual_data) , color = "black", stat = "identity") +
  geom_point(aes (x = x_axis, y = fcst_ets$mean), color = "red") +
  geom_point(aes (x = x_axis, y = fcst_arima$mean), color = "blue")


autoplot(actuals_2022, geom = "point")

class(actuals_2022)
class(fcst_arima)
fcst_arima <- ts (fcst_arima)

autoplot(fcst_arima)

fcst_arima$series
geom_point(fcst_ets$mean, color = "red")# +
  geom_point(aes (x = x_axis, y = fcst_arima$mean), color = "blue")#

plot (1:5, xaxt ="n", xlab ="Some Letters")
axis (1, at =1:5, labels = c("a","b","c","d","e"))
