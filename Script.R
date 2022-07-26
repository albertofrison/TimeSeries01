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
library (tidyverse) #we will need to tidy the data - later - for plotting on ggplot the right way



##### 
# Section 01 - Loading the Data
# Passenger Cars Registrations in Italy from 1990 - Today (updated June 2022)
data <- read.delim ("data/Passengers_Cars_Registrations_Italy.csv", sep = ";") 
class (data) # data.frame
head (data)

# declare a new variable Y as Time Series (just the second column) - Y_entire gets the WHOLE data
Y_entire <- ts(data[,2], start = c(1990,1), end = c(2022,6), frequency = 12)
class (Y_entire) # time series
head (Y_entire) # this is consistent with the head() of the data.frame only that now is a ts() format
tail (Y_entire) # it ends where it should - jan to june 2022 - thereore I expect no errors in the ts() definition

# this is the TRAINING SET --- you can play with the start // end of the training set to see what works better to forecast
Y <- window (Y_entire, start = c(2005,1), end = c(2021,12))
head (Y) # deciding to start in 2005 is completely subjective
tail (Y) # yes, we leave the remaining 2022 months outside the TRAINING SET to be used to evaluate the correctness of our foreasts

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
# 23/07 SOLVED IT!!
# 1. convert all ts and forecast data into a data frame - use as.numeric to coherce the bastard
# 2. add x labels into the data frame to tame the x axis
# 3. use group = 1 to allow for geom_line to work - https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
# 4. Still cannot control how legends works, found a turnaround, but cannot express my own colors and line shapes
# 4.a COLORS needs to stay inside the aes() otherwise the legend will NOY show up!
# 4.b the sames goes with the linetype which you can "name" but not select and have to play around

# now that I think about it , maybe the dataframe was not necessary and I could have plotted ts() and forecast data alltogether if only I used the bloody group = 1 inside the aes () from the beninning!!!!!


x_axis <- c("01 - Jan", "02 - Feb", "03 - Mar", "04 - Apr", "05 - May", "06 - Jun")

# Conversion of time series and forecast data into a simple data frame with intelligible x axis
a <- data.frame(x_month = x_axis, act = as.numeric(actual_data), fct_ets = as.numeric(fcst_ets$mean), fct_arima = as.numeric(fcst_arima$mean))


# plot on ggplot - method 01 (wrong way)
ggplot (data = a, aes (x  = x_month, y = act)) +
  geom_point(aes (x = x_month, y = act , color = "Actuals")) +
  geom_line (aes (x = x_month, y = act, group = 1, color = "Actuals", linetype = "Actuals")) + # do not forget the group = 1 parameter for lines
  
  geom_point(aes (x = x_month, y = fct_ets, color = "ETS")) +
  geom_line (aes (x = x_month, y = fct_ets, group = 1 , color = "ETS", linetype = "Forecast")) + # do not forget the group = 1 parameter for lines
  
  geom_point(aes (x = x_month, y = fct_arima, color = "ARIMA")) +
  geom_line (aes (x = x_month, y = fct_arima, group = 1 , color = "ARIMA", linetype = "Forecast")) + # do not forget the group = 1 parameter for lines
  
  labs (title = "New Passenger Cars Registrations - Italian Market",
        subtitle = "Actual 2022 vs Forecast (ETS and ARIMA)",
        caption ="Elaborated by Alberto Frison on - ACEA data - https://www.acea.auto/") +
  xlab("Month") +
  ylab ("Vehicles #") +
  scale_y_continuous(labels = scales::comma) # adds the comma in the thousands


# plot on ggplot - method 02 (right way, tidy data)
# trying to tide data
b <- a %>%
      pivot_longer(c("act","fct_ets","fct_arima"), names_to = "type", values_to = "val")

ggplot (data = b, aes (x = x_month, y = val)) +
  geom_point(aes(color = type)) +
  geom_line(aes(color = type, linetype = type, group = 1), stat = "identity")

#, group = 1
