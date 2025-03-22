library(forecast)

# Part a: Compute the sample ACF and PACF for the square root of sunspot data
data(sunspot.year)  # Load sunspot data from base R
Y_t <- sqrt(sunspot.year)  # Transform the data

# Plot the transformed series
plot(Y_t, type = "l", main = "Square Root of Sunspot Data", xlab = "Time", ylab = "Y_t")

# Compute and plot the sample ACF and PACF
acf(Y_t, lag.max = 20, main = "Sample ACF of Y_t")
pacf(Y_t, lag.max = 20, main = "Sample PACF of Y_t")

# Save the plots
png("Square_Root_Sunspot_Data.png", width = 800, height = 400)
plot(Y_t, type = "l", main = "Square Root of Sunspot Data", xlab = "Time", ylab = "Y_t")
dev.off()

png("Sample_ACF_Y_t.png", width = 800, height = 400)
acf(Y_t, lag.max = 20, main = "Sample ACF of Y_t")
dev.off()

png("Sample_PACF_Y_t.png", width = 800, height = 400)
pacf(Y_t, lag.max = 20, main = "Sample PACF of Y_t")
dev.off()

# Part b: Yule-Walker estimates for the AR(2) model
gamma_X <- c(1382.2, 1114.4, 591.73)  # Sample autocovariances
Gamma <- matrix(c(gamma_X[1], gamma_X[2], gamma_X[2], gamma_X[1]), nrow = 2, byrow = TRUE)
gamma <- matrix(gamma_X[2:3], nrow = 2)

# Solve for phi_1 and phi_2
phi <- solve(Gamma, gamma)
phi_1 <- phi[1]
phi_2 <- phi[2]

# Estimate sigma^2
sigma2 <- gamma_X[1] - phi_1 * gamma_X[2] - phi_2 * gamma_X[3]

cat("Yule-Walker Estimates:\n")
cat("phi_1 =", phi_1, "\n")
cat("phi_2 =", phi_2, "\n")
cat("sigma^2 =", sigma2, "\n")

# Part c: Durbin-Levinson algorithm for sample PACF
# Use the acf() function to compute PACF
pacf_values <- pacf(Y_t, lag.max = 3, plot = FALSE)
phi_11 <- pacf_values$acf[1]
phi_22 <- pacf_values$acf[2]
phi_33 <- pacf_values$acf[3]

cat("Sample PACF Values:\n")
cat("phi_11 =", phi_11, "\n")
cat("phi_22 =", phi_22, "\n")
cat("phi_33 =", phi_33, "\n")

# Test compatibility with AR(1), AR(2), MA(1), MA(2) processes
critical_value <- 1.96 / sqrt(length(Y_t))  # 95% significance level
cat("Critical value for PACF (95% significance level):", critical_value, "\n")
cat("Is phi_33 significant?", abs(phi_33) > critical_value, "\n")

# Part d: Forecasting using the fitted AR(2) model
# Fit the AR(2) model
ar_model <- arima(Y_t, order = c(2, 0, 0), method = "CSS")

# Forecast for h = 1, 2, 3, 4
forecasts <- predict(ar_model, n.ahead = 4)
forecast_values <- forecasts$pred
forecast_se <- forecasts$se

cat("Forecasts for h = 1, 2, 3, 4:\n")
print(forecast_values)

# Part e: Plot all data and forecasts
# Combine original data and forecasts
forecast_index <- (length(Y_t) + 1):(length(Y_t) + 4)
plot_data <- c(Y_t, forecast_values)

# Plot the data and forecasts
plot(plot_data, type = "l", main = "Sunspot Data and Forecasts", xlab = "Time", ylab = "Y_t", col = "black")
lines(forecast_index, forecast_values, col = "red", lty = 2)
legend("topright", legend = c("Data", "Forecasts"), col = c("black", "red"), lty = c(1, 2))

# Save the plot
png("Sunspot_Forecasts.png", width = 800, height = 400)
plot(plot_data, type = "l", main = "Sunspot Data and Forecasts", xlab = "Time", ylab = "Y_t", col = "black")
lines(forecast_index, forecast_values, col = "red", lty = 2)
legend("topright", legend = c("Data", "Forecasts"), col = c("black", "red"), lty = c(1, 2))
dev.off()