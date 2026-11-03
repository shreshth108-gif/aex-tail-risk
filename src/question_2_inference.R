# Question 2a
# Function to compute the autocorrelation at a given k
acf_at_lag_k <- function(r, k) {
  n <- length(r)            
  r_mean <- mean(r)         
  r_var <- var(r)           
  
  sum_acf <- sum((r[1:(n-k)] - r_mean) * (r[(k+1):n] - r_mean))
  acf_k <- sum_acf / ((n - k) * r_var)
  
  return(acf_k)
}

# Function to compute the ACF up to n-1
sample_acf <- function(r, max_lag) {
  acf_vals <- numeric(max_lag + 1)
  
  for (k in 0:max_lag) {
    acf_vals[k + 1] <- acf_at_lag_k(r, k)
  }
  
  return(acf_vals)
}
# Example for 2019 data
max_lag <- 254  
acf_vals_custom <- sample_acf(daily_return_2019, max_lag)

# Plot the custom ACF values
plot(0:max_lag, acf_vals_custom, type = "h", main = "Sample ACF (Custom Implementation)",
     xlab = "Lag", ylab = "ACF", ylim = c(-1.5, 1), col = "blue", lwd = 1)

# Compare with R's built-in acf 
acf_vals_builtin <- acf(daily_return, plot = FALSE, lag.max = max_lag)$acf

# Plot the built-in ACF values
plot(0:max_lag, acf_vals_builtin[1:(max_lag + 1)], type = "h", main = "Sample ACF (Built-in acf())",
     xlab = "Lag", ylab = "ACF", ylim = c(-1, 1), col = "red", lwd = 1)

# Overlay both for comparison
plot(0:max_lag, acf_vals_custom, type = "h", col = "blue", lwd = 1, 
     main = "Comparison of Custom and Built-in ACF", xlab = "Lag", ylab = "ACF", ylim = c(-1.5, 1))
points(0:max_lag, acf_vals_builtin[1:(max_lag + 1)], col = "red", type = "h", lwd = 1, lty = 2)  # Dashed red line
legend("topright", legend = c("Custom ACF", "Built-in ACF"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2), cex = 0.75)


#Question 2b-c
# Calculate daily returns in 2004
time_2004 <- subset(data, format(Date, "%Y") == "2004")

daily_return_2004 <- (time_2004$Close - time_2004$Open) / time_2004$Open

summary(daily_return_2004)
sd(daily_return_2004)

boxplot(daily_return_2004, 
        main = "Distribution of Daily Returns in 2004",
        ylab = "Daily Return",  
        col = "lightblue",
        outpch = 1)

dens_2004 <- density(daily_return_2004)
hist(daily_return_2004,
     main="Histrogram of Daily Returns in 2004",
     breaks=20, 
     xlab="Daily Return",
     prob=TRUE)
lines(dens_2004, col = "red", lwd = 2)

qqnorm(daily_return_2004)
qqline(daily_return_2004, col = "red") 

plot(time_2004$Date, daily_return_2004, 
     type = "p", 
     pch = 5,  
     cex = 0.1,  #
     xlab = "Month", 
     ylab = "Daily Return", 
     main = "Daily Returns in 2004",
     xaxt = "n")  # Suppress automatic x-axis
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 month"), format = "%m")



# Calculate daily returns in 2019
time_2019 <- subset(data, format(Date, "%Y") == "2019")


daily_return_2019 <- (time_2019$Close - time_2019$Open) / time_2019$Open

summary(daily_return_2019)
sd(daily_return_2019)

boxplot(daily_return_2019, 
        main = "Distribution of Daily Returns in 2019",
        ylab = "Daily Return",  
        col = "lightblue",
        outpch = 1)

dens_2019 <- density(daily_return_2019)
hist(daily_return_2019,
     main="Histrogram of Daily Returns in 2019",
     breaks=20, 
     xlab="Daily Return",
     prob=TRUE)
lines(dens_2019, col = "red", lwd = 2)

qqnorm(daily_return_2019)
qqline(daily_return_2019, col = "red") 

plot(time_2019$Date, daily_return_2019, 
     type = "p", 
     pch = 5,  
     cex = 0.1,  #
     xlab = "Month", 
     ylab = "Daily Return", 
     main = "Daily Returns in 2019",
     xaxt = "n")  # Suppress automatic x-axis
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 month"), format = "%m")


# Combine 2004 and 2019 returns into a single data frame
combined_returns_y <- data.frame(
  years = rep(c("2004", "2019"), c(length(daily_return_2004), length(daily_return_2019))),
  returns = c(daily_return_2004, daily_return_2019)
)

# Boxplot comparing 2004 and 2019 returns
boxplot(returns ~ years, data = combined_returns_y, 
        main = "Comparison of Daily Returns: 2004 vs 2019",
        ylab = "Daily Return", col = c("orange", "lightblue"))

# Histogram comparing 2004 and 2019 returns
max_density <- max(c(dens_2004$y, dens_2019$y))

# Plot the histogram for 2004
hist(daily_return_2004, breaks = 20, col = rgb(1, 0, 0, 0.5), 
     xlim = range(c(daily_return_2004, daily_return_2019)), 
     ylim = c(0, max_density),  
     main = "Comparison of Daily Returns: 2004 vs 2019",
     xlab = "Daily Return", prob = TRUE)
hist(daily_return_2019, breaks = 20, col = rgb(0, 0, 1, 0.5), 
     add = TRUE, prob = TRUE)
legend("topright", legend = c("2004", "2019"), 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))


#Question 2d
dlaplace <- function(x, mu, b) {
  return((1 / (2 * b)) * exp(-abs(x - mu) / b))
}

fit_laplace_model <- function(data, title = "Laplace Distribution Fit") {
  # Estimate the parameters (location and scale)
  mu_hat <- median(data)  
  b_hat <- mean(abs(data - mu_hat))  
  
  # Print the estimates
  cat("Estimated location (mu):", mu_hat, "\n")
  cat("Estimated scale (b):", b_hat, "\n")
  
  #Calculate density for data and fitted Laplace distribution
  dens <- density(data) 
  laplace_dens <- dlaplace(dens$x, mu_hat, b_hat)  
  
  #Plot histogram of the data
  hist(data, prob = TRUE, breaks = 40, col = "lightblue", 
       main = title, xlab = "Daily Returns", ylab = "Density",
       ylim = c(0, max(c(dens$y, laplace_dens))))  # Set y-axis limit based on the max of both densities
  
  #Overlay the fitted Laplace density curve
  curve(dlaplace(x, mu_hat, b_hat), col = "red", lwd = 2, add = TRUE)
  
  #Overlay a non-parametric density estimate
  lines(dens, col = "blue", lwd = 2, lty = 2)  
  
  legend("topright", legend = c("Fitted Laplace curve", "Density curve"),
         col = c("red", "blue"), lwd = 2, lty = c(1, 2), cex = 0.5)  # Adjust 'cex' for smaller legend size
}

# Daily returns in 2004
fit_laplace_model(daily_return_2004, title = "Fitted Laplace Distribution for 2004")

# Daily returns in 2019
fit_laplace_model(daily_return_2019, title = "Fitted Laplace Distribution for 2019")


# Question 2e-f
laplace_quantile <- function(p, mu, b) {
  return(mu - b * log(2 * p)) 
}

# Function to compute parametric VaR1% 
compute_VaR <- function(data, year) {
  mu_hat <- median(data)
  b_hat <- mean(abs(data - mu_hat))
  
  p <- 0.01 
  VaR <- laplace_quantile(p, mu_hat, b_hat)
  
  return(VaR)
}
VaR1_2004 <- compute_VaR(daily_return_2004, year = 2004)
VaR1_2019 <- compute_VaR(daily_return_2019, year = 2019)


# Function to compute non-parametric VaR1% 
compute_nonparametric_VaR <- function(data, year) {
  p <- 0.01
  VaR <- -quantile(data, probs = p)

  return(VaR)
}

# Compute non-parametric VaR1% for 2004 and 2019
VaR1_nonparam_2004 <- compute_nonparametric_VaR(daily_return_2004, year = 2004)
VaR1_nonparam_2019 <- compute_nonparametric_VaR(daily_return_2019, year = 2019)


# Question 2g
# Number of samples (Example for 2019 data)
n <- 255


# Parameters
mu <- 1  # True value of mu (location)
b <- 1   # True value of b (scale)

# Number of repetitions of the experiment
N <- 100000

# Initializing vectors to store results
mu_ML <- rep(0, N)
b_ML <- rep(0, N)

for (k in seq(1, N)) {
  # Generate data from the Laplace distribution
  x <- mu + b * (rexp(n) - rexp(n))
  
  # Maximum likelihood estimators for Laplace distribution
  mu_ML[k] <- median(x)  
  b_ML[k] <- mean(abs(x - mu_ML[k])) 
}

# Pivotal quantity Q for mu
Q <- (mu_ML - mu) / b_ML

# Computing the 0.025 and 0.975 quantiles of Q for the 95% CI
a <- quantile(Q, 0.025)
b <- quantile(Q, 0.975)

x <- daily_return_2019 

# Estimate mu (location) from the sample data
hat_mu <- median(x)
hat_b <- mean(abs(x - hat_mu))

# Compute the 95% confidence interval for mu
cat("A 95% CI for mu is [", hat_mu - b*hat_b, ", ", hat_mu - a*hat_b, "]\n")



