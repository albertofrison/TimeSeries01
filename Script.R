######################################################
# This files analyzes monthly Passengers Car Registrations Data as Time Series and makes a general review on time series tools
# Data Source: https://www.acea.auto/figure/passenger-car-registrations-in-europe-since-1990-by-country/
#
# Created with ♥ by Alberto Frison in July 2022
# Revised in February 2024
# Thanks to Adam Check for the great YouTube videos: https://www.youtube.com/watch?v=dBNy_A6Zpcc
# and to Scott Burk for the (quite complex) but inspiring YouTube Lectures: https://www.youtube.com/watch?v=HdYBuDMJ40Y&list=PLX-TyAzMwGs-I3i5uiCin37VFMSy4c50F&index=1
# and to this article - https://towardsdatascience.com/beginners-introduction-to-time-series-analysis-and-forecasting-c2c2918603d9
######################################################


#####
# Section 00 - Initialization and Load of Packages
rm (list =ls())   # clears all variables in the workspace
library (fpp2)    # forecasting package
library (tidyverse) #we will need to tidy the data - later - for plotting on ggplot the right way
library (tseries) #used for some tests


##### 
# Section 01 - Loading the Data
# Passenger Cars Registrations in Italy from 1990 - Today (refer to ACEA or DataForce websites to update the enclosed .csv)
data <- read.delim ("data/Passengers_Cars_Registrations_Italy.csv", sep = ",") 
class (data) # data.frame
head (data) # first 06 records 
tail (data) # last 06 records

#####
# Section 02 - Cleaning the Data
# Substituting 2 outliers in data due to COVID with monthly averages
# Unfortunately March and April 2020 shows two outliers, I chose to deal with them by substituting to them the mean of these months of the previous years
# Note, I will also add May as by looking at RESIDUALS, while checking the forecast fit, it seems that even May 2020 is too extreme

# Let's create a new dimension to store the month name (01 for January) so we can calculate monthly averages
# I add here the Month_Name column to identify each month of the series
data <- data %>%
  mutate (Month_Name = format(as.Date(Month, tryFormats = "%d/%m/%Y"),"%m"),
          Year_Name = format(as.Date(Month, tryFormats = "%d/%m/%Y"),"%Y"))

# Let's graphically see these outliers
data %>%
  ggplot() +
  geom_boxplot(aes(x=Month_Name, y= Registrations))

# DO NOT DELETE ME as working with dates is always a HELL
# in the as.Date function use TRYFORMATS to tell in which format the date is, then extract the YEAR (for instance) using the %Y in format()
# format(as.Date("01/04/2020",tryFormats = "%d/%m/%Y"),"%Y")

# Now we are there let's also have a look at the data by year
# Note: the reason that 2022 registrations are difficult to forecast as the market is performing very badly in respect to the past years
data %>%
  ggplot() +
  geom_boxplot(aes(x=Year_Name, y= Registrations))

# Now I filter out the two "wrong" months and I calculate, visually, the averages to be used to "correct" the outliers
data %>%
  filter (Month != c("01/03/2020", "01/04/2020", "01/05/2020")) %>%
  group_by (Month_Name) %>%
  summarize (Mean = mean(Registrations))

# Finally I substitute the averages into the "actual" data
data[which(data[,1]=="01/03/2020"),2] <- 201242
data[which(data[,1]=="01/04/2020"),2] <- 171174
data[which(data[,1]=="01/05/2020"),2] <- 184491

# ... now if you run the boxplot chart here above the outliers have disappeared...
#data %>%
# ggplot() +
# geom_boxplot(aes(x=Month_Name, y= Registrations))


#####
# Section 03 - Let's start to work with time series
# DECLARING THE TIMESERIES
# declare a new variable Y as Time Series (just the second column) - Y_entire gets the WHOLE 
Y_entire <- ts(data[,2], start = c(1990,1), end = c(2023,12), frequency = 12) # remember to check the ending month at each update
class (Y_entire) # time series
head (Y_entire) # this is consistent with the head() of the data.frame only that now is a ts() format
tail (Y_entire) # it ends where it should - jan to june 2022 - therefore I expect no errors in the ts() definition


# SELECTING THE TRAINING SET
# in order to effectively understand the validy of our forecast modesl, we can split the data in two: the TRAINING set and the TEST set
# this is the TRAINING SET --- you can play with the start // end of the training set to see what works better to forecast
# for the sake of the exercise we can use the last year (2023) as test data and play with the rest of the data for the training set
Y <- window (Y_entire, start = c(1990,1), end = c(2022,12))
head (Y) # deciding to start in xxxx is completely subjective
tail (Y) # yes, we leave the remaining 2022 months outside the TRAINING SET to be used to evaluate the correctness of our foreasts



#####
# Section 04 - Preliminary Analysis
# Time Plot - using autoplot function
par (mfrow = c(1,1))
autoplot (Y) + 
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registrations") +
  xlab ("Year")
# data looks both SEASONAL (to be confirmed) and with a downward TREND


# additional functions to analyse the time series
# ggseasonplot
ggseasonplot(Y, ylab = "Registrations", xlab = "Month", main = "New Passengers Cars Registration [Italy]", year.labels = TRUE, year.labels.left = TRUE, col= 1:20)

#ggmonthplot
ggmonthplot(Y, ylab = "Registrations", xlab = "Month", main = "New Passengers Cars Registration [Italy]")

# CORRELATION AND HOW TO EXPLORE IT 
lag.plot(Y, lags =12, do.lines = FALSE) # the ggmonthplot highlighted that monthly registration values were correlated, now at lag 12 it becomes clear that data is autocorrelated
Acf(Y) # see Lag #12 and Lag #24


#####
# Section 05 - Small section in order to understand how CORRELATION WORDKS
# small section on correlation on random numbers (you guess it there is none)
set.seed (100)
x <- ts(rnorm(1000)) # 1000 numbers randomly generated from a normal distribution
plot (x, main = "white noise")
Acf (x) # NO correlation in random numbers


#####
# Section 06 - Stationarity
# Testing Stationarity of ts via UNIT ROOTS test and ACF plot
# A stationary time series is one with the following properties:
# a) Constant mean through time
# b) Constant variance (auto covariance) through time
# c) Statistical properties do not change and are consistent

PP.test(Y, lshort = FALSE) # p-value = 0.01 --> the serie is stationary

# in alternative, we ca use the AUgmented Dickey-Fuller Test
# H0: the series has UNIT ROOT (non-statiornary)
# H1: the series is stationary
adf.test(Y) # p-value = 0.01 --> again, the serie is stationary

Acf(Y)
Pacf(Y)

# Differentiating is used to GET RIF of the TREND in a Time Serie
DY <- diff(Y) # First Difference of the Time Series - DY now contains the month over month changes in the time series

autoplot (DY) + 
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registration - Month over Month Difference") +
  xlab ("Year")
# now the trend has been removed

PP.test(DY)
Acf(DY)

#####
# Section 06 A - BONUS CHAPTER - HOW MANI TIMES YOU NEED TO DIFFERENCIATE?
# To determine the number of times to difference a time series, we can choose the one that gives the lowest overall variance.
Y.var <- var(Y)

for(i in 1:10) {
  diff <- diff(Y, lag=1, differences = i)
  Y.var[i+1] <- var(diff)
}
Y.var <- data.frame(diff=0:10, var=Y.var)

# Plot variance against the number of times data is differentiated 
plot(Y.var, type="l", ylab="Variance", xlab="d") # in any case the 1-th differentiation has a LOWER variance than Y, so DY is more stationary than Y
which(Y.var$var == min(Y.var$var))
Y.var[which(Y.var$var == min(Y.var$var)),]$diff # one differentiation

# In Diff_methods I save, I store, the value of the variance of the data obtained by the various Difference Methods
Diff_methods <- matrix(nrow = 3, ncol = 2)
Diff_methods[1,1] <- "1 LAG"
Diff_methods[1,2] <- Y.var[2,]$var

#####
# Section 06B - LEAST SQUARES TREND REMOVAL
# The least-square trends removal involves fitting a linear model to the time series and subtracting the fitted values from the data points.
# It is another method to remove Trends.

time <- time(Y)
fit <- lm (Y ~ time) # fit a linear model
yt <- fit$fitted.values
zt <- fit$residuals


# Plot time series with superimposed linear model and residuals 
par(mfrow=c(2,1)) # 2 rows by 1 column
plot(Y, col="blue", main="Least Square Trend Removal", ylab="Registrations") # Original Time Series
abline(fit, col="red") # This is the plot of Linear Model
plot(Y-yt, type="l", col="green", xlab="Time", ylab="Residuals") # Original Time Series LESS the Trend (the abline)
par(mfrow=c(1, 1)) # resets to graphical pararemeter to 1 row by 1 column

# not sure the next 2 lines makes sense # under review
# Y_removed <- (Y - yt) 
# var(Y_removed) # lowest variance so far



#####
# Section 07 - INVESTIGATING SEASONALITY
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


# HOW TO GET RID OF SEASONALITY ? 
# 1. SEASONAL DIFFERENCING 
# Seasonal differentiating aims to subtract each data point by a previous data point of a fixed lag i.e. 12
seasonal_diff_Y <- diff (Y, lag = 12, differences = 1) # calculates the difference ts with a 12 month lag

# check the results
par (mfrow = c(2,2)) # two rows by two columns
plot (Y, main = "New Car Registrations", ylab = "Registrations")
Acf (Y, main = "ACF", ylab = "ACF")
plot (seasonal_diff_Y, main = "Differenciated Data - Lag 12", ylab = "Registrations")
Acf (seasonal_diff_Y, main = "ACF", ylab = "ACF")
par (mfrow = c(1,1)) # graphical parameters reset

Diff_methods[2,1] <- "SEASONAL DIFFERENCING"
Diff_methods[2,2] <- var (seasonal_diff_Y)

# 2. SEASONAL MEANS
# Seasonal means aims to subtract each data point by its respective group average, the monthly average
# Y goes from 2005 to 2021 included
seasonal_mean_Y <- data.frame (
  year = rep (1990:2022, each = 12),
  month = rep (1:12),
  value = Y) # prepare a dataframe to be used to calculate the MONTHLY AVERAGE in the next line of code

seasonal_mean_Y_xbars <- aggregate (value ~ month, data = seasonal_mean_Y, mean) # aggregate values by month, of the dataframe hence created using the mean as formula
seasonal_mean_Ydiff <- Y - seasonal_mean_Y_xbars$value # store into a variable the difference of each data point versus the monthly average

# check the results
par (mfrow = c(2,2)) # two rows by two columns
plot (Y, main = "New Car Registrations", ylab = "Registrations")
Acf (Y, main = "ACF", ylab = "ACF")
plot (seasonal_mean_Ydiff, main = "Differenciated Data - Monthly Average", ylab = "Registrations")
Acf (seasonal_mean_Ydiff, main = "ACF", ylab = "ACF") # doesn't look good
par (mfrow = c(1,1)) # graphical parameters reset

Diff_methods[3,1] <- "SEASONAL MEANS"
Diff_methods[3,2] <- var (seasonal_mean_Ydiff)

# 3. METHOD OF THE MOVING AVERAGES - DECOMPOSING A TIME SERIES 
decomposed_Y <- decompose (Y) # decomposes the ts into DATA (observed), the Trend, the Seasonal effect an the Random (noise) effects
plot(decomposed_Y) 

trend_Y <- decomposed_Y$trend
seasonal_Y <- decomposed_Y$seasonal
random_Y <- decomposed_Y$random

# check the results
plot (Y, ylab ="", main = "Composition of the Time Series", col = "grey", lty = 2)
lines (trend_Y, col = "red")
lines (seasonal_Y + trend_Y , col = "blue")
legend ("topright", legend = c ("Data", "Trend", "Seasonal + Trend"), col = c("grey", "red", "blue"), lty = 1)


#####
# Section 08 - FORECASTING
# Let's try to forecast by fitting and ARIMA model and using manual parameters through ACF and PACF interpretation
# Arima (p,d,q) is composed of 3 hyperparameters:
# p = Autoregression (AR)
# d = Differencing (I)
# q = Moving Average (MA)
m <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
layout(m)
plot (Y, main= "", ylab ="")
abline(a = mean(Y), b =0, lty = 3, col = "blue") #mean
abline(a = mean(Y) + sd(Y), b =0, lty = 4, col = "red") #mean +sd
abline(a = mean(Y) - sd(Y), b =0, lty = 4, col = "red") #mean +sd
Acf(Y,main="")
Pacf(Y,main="")

# let's fit a potential arima model and see how it performs -
# TODO: Review more on how to interpret ACF and PACF to instruct a model

fit_arima <- auto.arima(Y, stepwise = FALSE, approximation = FALSE, parallel = TRUE, allowdrift = TRUE)

# Techniques to check the FIT of a Model
# a.look at the ERRORS TABLE (ME,MAE, RMSE, ....) and the VARIANCE 
print(summary(fit_arima)) # here you check errors Mean Error, RMSE, Mean Percentage Error, and so on

# b. Graph of Residuals - checking their MEAN (that should be ZERO) and how VARIANCE changes over time STABLE VARIANCE
# here you check residuals chart, acf and distribution - ACF on the residuals should be SMALL and demonstrate NO significant patterns and 
# in order to conclude that the RESISUALS are independent
checkresiduals(fit_arima)  # 
mean(fit_arima$residuals) # this is he MEAN ERROR, the first error type (ME) returned by summary(fit_arima)

# c. let's see Lijung-Box Test
# this test is a statistical test that measure whether or not a group of auto-correlations in a time series are different from ZERO
# if the p-value of the test is GREATER than 0.05 (5%) then we do NOT reject the null hypotheses and conclude that the model is a good fit
# each model can always be compared one another by looking at the smalles Standard Deviation of the Residuals
Box.test (fit_arima$residuals, type = "Ljung-Box", lag = 24) #p-value = IF xxxx > 5% the model is a good fit for the data


# d. AKAIKE INFORMATION CRITERION (AIC) or, a measure of the trade-between the goodness of the fit of the model and the number of parameters in the model
# it is a way to try to avoid OVERFITTING: many parameters might provide a good fit for the data, but not generate a good PREDICTOR of the future - in the
# other hand a model with few parameters might not be sufficient to capture the patterns in the data.
# Steps: 
#   1. we establish an ARIMA (p,d,q) model and cycle over for loops to identity which p,q combination yields the lower AIC (or BIC??? see below)
#   2. we loop over p,d,q parameters to look for the best (lowest) AIC

aic_result <- numeric (4) # 

for (p in 0:2) {
  for (d in 0:2) {
    for (q in 0:2) {
      aic <- arima (Y, order = c(p,d,q), seasonal = c(2,1,1))$aic  
      aic_result <- rbind(aic_result, c(p,d,q,aic))
    }
  }
}

aic_result <- aic_result [-1,]
colnames (aic_result) <- c("p","d","q", "AIC")
aic_result
min(aic_result[,4]) #8597.504 c(2,2,1)

# TO DO look at the best model fit ARIMA and compare the AICs, the best ARIMA model (auto.arima) should have the lowest AIC (or BIC??? see below)


#####
# Section 09  Let's try to forecast
# Foreword: one can start with a very simple model (i.e. the NAIVE) just to have a benchmart for the most advanced models

# 01. Benchmark Methods - SEASONAL NAIVE METHOD: T_y = T_(y-s) + Random Error
# Remember to use the DIFFERENCE DATA and not the RAW DATA due to the TREND
fit_snaive <- snaive(DY) # RESISUAL STD DEV = 24519.7198
print(summary(fit_snaive))   
checkresiduals(fit_snaive)
class(fit_snaive) 

# as alternative to checkresiduals function
res <- residuals (fit_snaive)
Acf(res, main ="ACF of Residuals - Naive Method")
hist(res, nclass= "FD", main ="Histogram of Residuals - Naive Method")


# 02. Exponential Smoothing Model 
# We can use REGULAR RAW DATA
# tries a number of exponential smoothing models and returns the best
fit_ets <- ets(Y)         
print(summary(fit_ets))   
checkresiduals(fit_ets)


# 03. ARIMA
# Auto.arima automatically fits the best model with differing (p,d,q) coordinates - then the BEST model is the one with the LOWEST BIC (Bayesian Info Criterion)
# TO DO: learn better if the parameer that auto.arima looks for is BIC or AIC
fit_arima <- auto.arima (Y, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
sqrt (fit_arima$sigma2) # This is the STD DEV of the Model, a measure of accuracy
checkresiduals(fit_arima)



#####
# Section 10 LET's FORECAST

# ETS Forecast
fcst_ets <- forecast(fit_ets,h=12) # remember to change the number of months (8 for Jan to Aug)

# ARIMA Forecast
fcst_arima <- forecast(fit_arima,h=12)


#####
# Check Forecasts against Actuals
actual_data <- window (Y_entire, start = c(2023,1))

# SNAIVE METHOD
#accuracy (fcst_snv,actual_data)
#autoplot(fcst_snv)

# ETS METHOD
accuracy (fcst_ets,actual_data) # 
autoplot(fcst_ets)

# ARIMA ETHOD
accuracy (fcst_arima,actual_data) # 
autoplot(fcst_arima)


# PLOTTING 
par (mfrow = c(1,1)) 
plot (actual_data, ylim = c (60000,200000), main ="Italian Market - New Car Registrations - Forecast")
points(actual_data, col = "black", pch = 19)
lines(fcst_ets$mean, lty= "solid", col = "red")
points(fcst_ets$mean, col = "red")
lines(fcst_arima$mean, lty="solid", col = "blue")
points(fcst_arima$mean, col = "blue")
legend ("bottomleft", legend = c("2023 - Actuals", "ETS Method Forecast", "ARIMA Forecast"), lty = "solid", col = c("black","red", "blue"))


#####
# Trying to get better looking charts... no luck so far...
# 23/07 SOLVED IT!!
# 1. convert all ts and forecast data into a data frame - use as.numeric to coherce the numeric figures
# 2. add x labels into the data frame to tame the x axis
# 3. use group = 1 to allow for geom_line to work - https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
# 4. Still cannot control how legends works, found a turnaround, but cannot express my own colors and line shapes
# 4.a COLORS needs to stay inside the aes() otherwise the legend will NOY show up!
# 4.b the sames goes with the linetype which you can "name" but not select and have to play around
# now that I think about it , maybe the dataframe was not necessary and I could have plotted ts() and forecast data alltogether if only I used the bloody group = 1 inside the aes () from the beninning!!!!!


x_axis <- c("01.23", "02.23", "03.23", "04.23", "05.23", "06.23", "07.23", "08.23", "09.23", "10.23", "11.23", "12.23")
x_axis <- c("23_01", "23_02", "23_03", "23_04", "23_05", "23_06", "23_07", "23_08", "23_09", "23_10", "23_11", "23_12")

# Conversion of time series and forecast data into a simple data frame with intelligible x axis
a <- data.frame(x_month = x_axis, act = as.numeric(actual_data), fct_ets = as.numeric(fcst_ets$mean), fct_arima = as.numeric(fcst_arima$mean))


##### 
# wrong way to plot. see better way below
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



#####
# plot on ggplot - method 02 (right way, tidy data) 
# trying to tide data
b <- a %>%
      pivot_longer(c("act","fct_ets","fct_arima"), names_to = "type", values_to = "val")

ggplot (data = b, aes (x = x_month, y = val, linetype=type, color = type, group = type)) +
  geom_point() +
  geom_line() +
  labs (title = "Year 2022 Passengers' Cars Registration in Italy",
        subtitle = "Actual Values VS Arima, ETS Forecast Methods",
        caption = "Made with ♥ by Alberto Frison - https://github.com/albertofrison/TimeSeries01") +
  xlab("Month") +
  ylab("Registrations #units")+
  scale_y_continuous(labels = scales::comma) # adds the comma in the thousands

# Saving (last chart) to file // Remember: 1 inch = 96 pixels
ggsave (filename = "charts/Chart01_Actual_vs_Forecasts_2023.png", device = "png", dpi = "retina", height = 1018/96, width = 1920/96)



################################################################################
#####
# Section dedicated to forecast 2024 data
# Remember to fully perform all code from Sections 00 - 02 before this section

# new ts series containing the training data - including 2023
Y_2024 <- window (Y_entire, start = c(2017,1), end = c(2023,12))

# new ts series containing the training data - including 2023
Y_2024 <- window (Y_entire, start = c(2019,1), end = c(2023,12))

# Time Plot - using autoplot function
par (mfrow = c(1,1))
autoplot (Y_2024) + 
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registrations") +
  xlab ("Year")
# data looks both SEASONAL (to be confirmed) and with a downward TREND

# ggseasonplot
ggseasonplot(Y_2024, ylab = "Registrations", xlab = "Month", main = "New Passengers Cars Registration [Italy]", year.labels = TRUE, year.labels.left = TRUE, col= 1:20)

#ggmonthplot
ggmonthplot(Y_2024, ylab = "Registrations", xlab = "Month", main = "New Passengers Cars Registration [Italy]")

lag.plot(Y_2024, lags =12, do.lines = FALSE) # the ggmonthplot highlighted that monthly registration values were correlated, now at lag 12 it becomes clear that data is autocorrelated
Acf(Y_2024) # see Lag #12 and Lag #24

DY_2024 <- diff(Y_2024) # First Difference of the Time Series - DY now contains the month over month changes in the time series

autoplot (DY_2024) + 
  ggtitle ("New Passengers Cars Registrations [Italy]") + 
  ylab ("Registration - Month over Month Difference") +
  xlab ("Year")
# now the trend has been removed

pp.test(DY_2024)
Acf (DY_2024)


m <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
layout(m)
plot (Y_2024, main= "", ylab ="")
abline(a = mean(Y_2024), b =0, lty = 3, col = "blue") #mean
abline(a = mean(Y_2024) + sd(Y), b =0, lty = 4, col = "red") #mean +sd
abline(a = mean(Y_2024) - sd(Y), b =0, lty = 4, col = "red") #mean +sd
Acf(Y_2024,main="")
Pacf(Y_2024,main="")

##### MODELLING SECTION
# ARIMA MODEL
fit_arima_2024 <- auto.arima(Y_2024, stepwise = FALSE, approximation = FALSE, parallel = TRUE, allowdrift = TRUE)
print(summary(fit_arima_2024)) # here you check errors Mean Error, RMSE, Mean Percentage Error, and so on
checkresiduals(fit_arima_2024)  # yes, except LAG-13 where there is a significant plot
mean(fit_arima_2024$residuals) # this is he MEAN ERROR, the first error type (ME) returned by summary(fit_arima)

Box.test (fit_arima_2024$residuals, type = "Ljung-Box", lag = 24)

#EXPONENTIAL SMOOTHING
fit_ets_2024 <- ets(Y_2024)         
print(summary(fit_ets_2024))   
checkresiduals(fit_ets_2024)


##### FORECAST SECTION
# ETS Forecast
fcst_ets_2024 <- forecast(fit_ets_2024,h=12) # remember to change the number of months (8 for Jan to Aug)
autoplot(fcst_ets_2024)

# ARIMA Forecast
fcst_arima_2024 <- forecast(fit_arima_2024,h=12)
autoplot(fcst_arima_2024) 
autoplot(fcst_arima_2024)

# x_axis is needed to solve some plottin issues
x_axis <- c("24_01", "24_02", "24_03", "24_04", "24_05", "24_06", "24_07", "24_08", "24_09", "24_10", "24_11", "24_12")

# Conversion of time series and forecast data into a simple data frame with intelligible x axis
data_2023 <- window (Y_entire, start = c(2023,1))
a <- data.frame(x_month = x_axis, past_year = as.numeric(data_2023), fct_ets_2024 = as.numeric(fcst_ets_2024$mean), fct_arima_2024 = as.numeric(fcst_arima_2024$mean))

b <- a %>%
  pivot_longer(c("past_year", "fct_ets_2024","fct_arima_2024"), names_to = "type", values_to = "val")

ggplot (data = b, aes (x = x_month, y = val, color = type, group = type)) +
ggplot (data = b, aes (x = x_month, y = val, linetype=type, color = type, group = type)) +
  geom_point() +
  geom_line() +
  labs (title = "FORECAST Year 2024 Passengers' Cars Registration in Italy",
        subtitle = "Arima and ETS Forecast Methods",
        caption = "Made with ♥ by Alberto Frison - https://github.com/albertofrison/TimeSeries01") +
  xlab("Month") +
  ylab("Registrations #units")+
  scale_y_continuous(labels = scales::comma) # adds the comma in the thousands

# Saving (last chart) to file // Remember: 1 inch = 96 pixels
ggsave (filename = "charts/Chart01_Forecasts_2024_V04.02.2024.png", device = "png", dpi = "retina", height = 1018/96, width = 1920/96)
