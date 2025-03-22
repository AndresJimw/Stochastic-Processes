# Given data
gamma0 <- 0.596
gamma1 <- -0.276

# Solve for theta
a <- 0.463
b <- 1
c <- 0.463
discriminant <- b^2 - 4*a*c
theta1 <- (-b + sqrt(discriminant)) / (2*a)
theta2 <- (-b - sqrt(discriminant)) / (2*a)

# Choose the invertible solution
theta <- ifelse(abs(theta1) < 1, theta1, theta2)

# Solve for sigma2
sigma2 <- gamma0 / (1 + theta^2)

cat("Estimated theta:", theta, "\n")
cat("Estimated sigma^2:", sigma2, "\n")