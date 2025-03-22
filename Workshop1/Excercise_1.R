# Excercise 1: 

library(tidyverse)
library(astsa)
library(forecast)
library(TSstudio)
library(ggplot2)

# a) Load the dataset into R
sales_data <- data.frame(
  Year = c(1995, 1996, 1997, 1998),
  I = c(153, 133, 145, 111),
  II = c(189, 177, 200, 170),
  III = c(221, 241, 187, 243),
  IV = c(215, 228, 201, 178),
  V = c(302, 283, 292, 248),
  VI = c(223, 255, 220, 202),
  VII = c(201, 238, 233, 163),
  VIII = c(173, 164, 172, 139),
  IX = c(121, 128, 119, 120),
  X = c(106, 108, 81, 96),
  XI = c(86, 87, 65, 95),
  XII = c(87, 74, 76, 94)
)

# b) Investigate the structure of the dataset
str(sales_data)

###
# The dataset has 4 observations and 13 variables:

# Year: num 1995 1996 1997 1998
# I, II, III, ..., XII: Numeric variables representing coded sales 
# in different periods of the year.
###

# c) Plot the data and describe the behavior of the series
# Convert the data to a time series
# First, transpose the data frame to have the periods as rows
sales_matrix <- as.matrix(sales_data[,-1])
sales_ts <- ts(as.vector(t(sales_matrix)), start = c(1995, 1), frequency = 12)

# Plot the time series using ggplot2
autoplot(sales_ts) +
  ggtitle("Company X Sales (1995-1998)") +
  xlab("Year") +
  ylab("Coded Sales")

###
# The plot shows the coded sales of Company X from 1995 to 1998. 
# The time series displays fluctuations over the years, with peaks and troughs 
# in different periods, indicating possible seasonality in the data.
###

# d) Does the time series appear to be stationary?
# Conduct an augmented Dickey-Fuller test to check for stationarity
adf_test <- adf.test(sales_ts)
print(adf_test)  # Resolving item d)

# If the p-value is greater than 0.05, the series is not stationary

# The result of the augmented Dickey-Fuller (ADF) test shows a p-value of 0.01, 
# indicating that the series is stationary (p-value < 0.05). 
# Therefore, it is not necessary to apply differencing or other transformations 
# to make the series stationary.

# e) Assess the trend and seasonal effects
# Decompose the time series into trend, seasonal, and residual components
sales_decomp <- decompose(sales_ts)
plot(sales_decomp)  # Resolving item e)

# Show the decomposition components
print(sales_decomp$trend)
print(sales_decomp$seasonal)
print(sales_decomp$random)

# Plot the trend and seasonality
autoplot(sales_decomp$trend) + ggtitle("Sales Trend") + xlab("Year") + ylab("Coded Sales")
autoplot(sales_decomp$seasonal) + ggtitle("Sales Seasonality") + xlab("Year") + ylab("Coded Sales")

# Plot the residuals
autoplot(sales_decomp$random) + ggtitle("Sales Residuals") + xlab("Year") + ylab("Coded Sales")

# Check the autocorrelation of the residuals
acf(sales_decomp$random, na.action = na.pass, main = "Residuals ACF")
pacf(sales_decomp$random, na.action = na.pass, main = "Residuals PACF")

# Summary of the decomposition
summary(sales_decomp) 

###
# The decomposition of the time series shows the following components:

# Trend: The overall trend of the sales appears to be decreasing from 1995 to 1998.
# Seasonality: There is a clear seasonal pattern, with peaks in certain periods of the year.
# Residuals: The residuals show the fluctuations not explained by the trend and seasonality.
###

