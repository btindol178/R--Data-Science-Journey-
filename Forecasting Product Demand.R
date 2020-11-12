#Forecasting Product Demand
#################################################################################################
#################################################################################################
#################################################################################################
rm(list=ls()); 
setwd("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT")
library(xts)

# Load dataset
bev<- read.csv("bevsales.csv");colnames(df)[1] <- "M.hi.p"

# loading data make date range
dates <- seq(as.Date("2014-01-19"), length = 176, by = "weeks")

# using dataframe use dates to index information with dates
bev_xts <- xts(bev, order.by = dates)

# Look at xts object
head(bev_xts[,"M.hi"], n = 3)

# Create the individual region sales as their own objects
MET_hi <- bev_xts[,"MET.hi"]
MET_lo <- bev_xts[,"MET.lo"]
MET_sp <- bev_xts[,"MET.sp"]

# Sum the region sales together
MET_t <- MET_hi + MET_sp + MET_lo

# Plot the metropolitan region total sales
plot(MET_t)

##############################################################################################################
#ARIMA Modeling
library(forecast)

# Split the data into training and validation
MET_t_train <- MET_t[index(MET_t) < "2017-01-01"]
MET_t_valid <- MET_t[index(MET_t) >= "2017-01-01"]

# Use auto.arima() function for metropolitan sales
MET_t_model <- auto.arima(MET_t_train)
MET_t_model
###########################################
# Forecasting and evaluating
# Forecast the first 22 weeks of 2017
forecast_MET_t <- forecast(MET_t_model, h = 22)

# Plot this forecast #
plot(forecast_MET_t)

# Convert to numeric for ease
for_MET_t <- as.numeric(forecast_MET_t$mean)
v_MET_t <- as.numeric(MET_t_valid)

# Calculate the MAE
MAE <- mean(abs(for_MET_t - v_MET_t))

# Calculate the MAPE
MAPE <- 100*mean(abs((for_MET_t - v_MET_t)/v_MET_t))

# Print to see how good your forecast is!
print(MAE)
print(MAPE)

# Convert your forecast to an xts object
for_dates <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
for_MET_t_xts <- xts(forecast_MET_t$mean, order.by = for_dates)

# Plot the validation data set
plot(MET_t_valid, main = 'Forecast Comparison', ylim = c(4000, 8500))

# Overlay the forecast of 2017
lines(for_MET_t_xts, col = "blue")

# Plot the validation data set
plot(MET_t_valid, main = 'Forecast Comparison', ylim = c(4000, 8500))

# Overlay the forecast of 2017
lines(for_MET_t_xts, col = "blue")

# Convert the limits to xts objects
lower <- xts(forecast_MET_t$lower[,2], order.by = for_dates)
upper <- xts(forecast_MET_t$upper[,2], order.by = for_dates)

# Adding confidence intervals of forecast to plot
lines(lower, col = "blue", lty = "dashed")
lines(upper, col = "blue", lty = "dashed")
