library(stats)

# Part a: Simulating AR(2) series

set.seed(123)  # Ensure reproducibility

# Define AR(2) parameters
n <- 100       # Number of observations
phi1 <- 1.3    # Coefficient for lag 1
phi2 <- -0.4   # Coefficient for lag 2
sigma2 <- 1    # Variance of white noise

# Generate white noise
Z <- rnorm(n, mean = 0, sd = sqrt(sigma2))

# Initialize the AR(2) series
X <- numeric(n)
X[1] <- rnorm(1, mean = 0, sd = 1)  # Initial value X_0
X[2] <- rnorm(1, mean = 0, sd = 1)  # Initial value X_1

# Generate the AR(2) series recursively
for (t in 3:n) {
  X[t] <- phi1 * X[t-1] + phi2 * X[t-2] + Z[t]
}

# Display the simulated series plot on screen
plot(X, type = "l", main = "Simulated AR(2) Series", xlab = "Time", ylab = "X_t")

# Save the simulated series plot
png("Simulated_AR2_Series.png", width = 800, height = 400)
plot(X, type = "l", main = "Simulated AR(2) Series", xlab = "Time", ylab = "X_t")
dev.off()

# Display the ACF plot on screen for h=0,1,2,3,4,5,6
acf(X, lag.max = 6, main = "ACF for AR(2) Model")

# Save the ACF plot
png("ACF_AR2_Model.png", width = 800, height = 400)
acf(X, lag.max = 6, main = "ACF for AR(2) Model")
dev.off()

# Part b: Simulating and saving ACF/PACF plots for a shorter series

# Simulate a shorter AR(2) series
x_sim <- numeric(n)
x_sim[1] <- rnorm(1, mean = 0, sd = 1)  # Initial value X_0
x_sim[2] <- rnorm(1, mean = 0, sd = 1)  # Initial value X_1

# Generate the series
for (t in 3:n) {
  x_sim[t] <- phi1 * x_sim[t-1] + phi2 * x_sim[t-2] + rnorm(1, mean = 0, sd = sqrt(sigma2))
}

# Display the shorter simulated series plot on screen
plot(x_sim, type = "l", main = "Simulated AR(2) Series (100 Observations)", xlab = "Time", ylab = "X_t")

# Save the shorter simulated series plot
png("Short_Simulated_AR2_Series.png", width = 800, height = 400)
plot(x_sim, type = "l", main = "Simulated AR(2) Series (100 Observations)", xlab = "Time", ylab = "X_t")
dev.off()

# Display the sample ACF plot for h=1 to 20 on screen
acf(x_sim, lag.max = 20, main = "Sample ACF of Simulated Series")

# Save the sample ACF plot
png("Sample_ACF_Short_Series.png", width = 800, height = 400)
acf(x_sim, lag.max = 20, main = "Sample ACF of Simulated Series")
dev.off()

# Display the sample PACF plot for h=1 to 20 on screen
pacf(x_sim, lag.max = 20, main = "Sample PACF of Simulated Series")

# Save the sample PACF plot
png("Sample_PACF_Short_Series.png", width = 800, height = 400)
pacf(x_sim, lag.max = 20, main = "Sample PACF of Simulated Series")
dev.off()
