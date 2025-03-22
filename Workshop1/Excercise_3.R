# Excercice 3:

library(TSA)
data(wages)

# a) Plot the time series. What basic pattern do you see from the plot?
plot(wages, main = "Average Hourly Wages (Jul 1981 - Jun 1987)",
     xlab = "Time", ylab = "Average Wage (USD)", col = "blue", type = "o")
# The plot shows an increasing trend in wages over time,
# with no clear seasonal pattern. This suggests that wages increased steadily
# during the analyzed period.

# b) Fit a linear time trend model using least squares. Give the plot of the linear trend overlain on the data, and give the estimated regression equation. 
linear_trend <- lm(wages ~ time(wages))

# Display the model summary
cat("=== Summary of the Linear Trend Model ===\n")
summary(linear_trend)
# The fitted linear trend model is:
# Wage_t = -549.0 + 0.2811 * t + ε_t.
# The slope (0.2811) indicates that wages increased by approximately 0.2811 USD per month.
# The R² value of 0.9728 suggests that the model explains 97.28% of the variability in wages.

# Plot the series with the linear trend overlaid
plot(wages, main = "Linear Trend of Wages (Jul 1981 - Jun 1987)",
     xlab = "Time", ylab = "Average Wage (USD)", col = "blue", type = "o")
abline(linear_trend, col = "red", lwd = 2)
# The red line represents the linear trend. It is observed that the model fits the data well,
# confirming that the linear trend is appropriate for describing the behavior of wages.

# c) Plot the residuals from the linear regression over time. Comment on any notable pattern. 
residuals <- residuals(linear_trend)

# Plot the residuals over time
plot(residuals, main = "Residuals of the Linear Trend Model",
     xlab = "Time", ylab = "Residuals", col = "blue", type = "o")
abline(h = 0, col = "red", lty = 2)
# The residuals fluctuate around zero without a clear pattern, suggesting that the linear trend
# model is adequate. However, there are some notable deviations, indicating that the model does not
# fully capture the variability in the data.

# d) Plot the autocorrelation function for the residuals. What do you conclude about the residuals? 
acf(residuals, main = "ACF of the Residuals", col = "blue")
# The ACF shows no significant autocorrelation in the residuals, as all bars are within the
# confidence limits. This suggests that the residuals are white noise, indicating that the linear trend
# model is appropriate in terms of capturing the temporal structure of the data.

# e) Investigate the normality of the residuals (error terms) from the regression. What is your conclusion?
# Histogram of the residuals
hist(residuals, main = "Histogram of the Residuals", xlab = "Residuals", col = "blue", breaks = 15)
# The histogram shows that the residuals have an approximately symmetric distribution,
# but with slight deviations in the tails, suggesting that the residuals may not perfectly follow a
# normal distribution.

# Q-Q plot of the residuals
qqnorm(residuals, main = "Q-Q Plot of the Residuals", col = "blue")
qqline(residuals, col = "red")
# The Q-Q plot shows that the residuals slightly deviate from the straight line in the tails,
# indicating that the distribution of the residuals is not perfectly normal.

# Normality test (Shapiro-Wilk)
cat("\n=== Normality Test of the Residuals (Shapiro-Wilk) ===\n")
shapiro.test(residuals)
# The p-value = 0.00474 is less than 0.05, indicating that the hypothesis of normality is
# rejected. This confirms that the residuals do not follow a normal distribution, suggesting that
# the linear trend model could be improved.