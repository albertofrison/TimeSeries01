######################################################
# This files analyzes monthly Passengers Car Registrations Data as Time Series
# 
# Source: https://www.acea.auto/figure/passenger-car-registrations-in-europe-since-1990-by-country/
# Created with ♥ by Alberto Frison - July 2022
######################################################

##### 
# Section 00 Initialization and Load of Packages
rm (list =ls())   # clears all variables in the workspace
library (fpp2)    # forecasting package
library(ggfortify)
library(forecast)



##### 
# Section 01 Load the Data
data <- read.delim ("data/Passengers_Cars_Registrations_Italy_1990_2021.csv", sep = ";")
data

Y <- ts(data[,2], start = 1990, frequency = 12) # declare a new variable Y as Time Series (just the second column)
Y


#####
# Section 02 Preliminary Analysis
# Time Plot
autoplot (Y) + 
  ggtitle ("New Passengers Cars Registrations 1990 - 2021 [Italy]") + 
  ylab ("Registrations")


# DIFFERENCE DATA TO GET RID OF THE TREND
DY <- diff(Y) # change of registrations from month to month (feb 1990 - jan 1990)
autoplot (DY) + 
  ggtitle ("New Passengers Cars Registrations 1990 - 2021 [Italy]") + 
  ylab ("Registration - Difference")


# INVESTIGATING SEASONALITY
# we got rid of the Trend, let's see if there is a monthly sasonality 

# one way to look at seasonality
ggseasonplot (DY) +
  ggtitle ("Seasonal Plot of New Passengers Cars Registrations 1990 - 2021 [Italy]") + 
  ylab ("Registration - Difference")
# there is a clear seasonality in the data with increase of sales in January, as well as March, June and September and a decrease in August

# another way to look at seasonality
ggsubseriesplot(DY)


#####
# Section 03 The Series has TREND and SEASONALITY
# Let's try to forecast


# 01. Benchmark Methods - SEASONAL NAIVE METHOD: T_y = T_(y-s) + Random Error
# Remember to use the DIFFERENCE DATA and not the RAW DATA due to the TREND

fit <- snaive(DY)
print(summary(fit))   # Residual SD 28006.2207
checkresiduals(fit)
#-----------------

# 02. Exponential Smoothing Model 
# We can use REGULAR RAW DATA

fit_ets <- ets(Y)         #tries a number of exponential smoothing models and returns the best
print(summary(fit_ets))   # Residual SD 0.1312
checkresiduals(fit_ets)
#-----------------


# 03. ARIMA
# Data in ARIMA needs to be STATIONARY: use the Diff data and tell there ise

fit_arima <- auto.arima (Y,d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #we can use the original Y data, using the d=1 parameter
print(summary(fit_arima))   # Residual SD 21293.75 #warning ARIMA returns VARIANCE ==> SD^2
checkresiduals(fit_arima)
#-----------------


#####
# Section 04 LET's FORECAST

fcst <- forecast(fit_ets,h=24) # we try to forecast two years of data ahead
autoplot(fcst, include = 60)
