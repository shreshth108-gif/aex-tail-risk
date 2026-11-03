data <- read_csv('data/AEX19942023.csv')

# Ensure the date column is properly formatted
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Question 1a
plot(data$Date, data$Open, 
     type = "p",  
     pch = 5, 
     cex = 0.1,  
     xlab = "Year", 
     ylab = "Opening Price", 
     main = "AEX Opening Prices Over Time",
     col = "green",
     xaxt = "n")  # Suppress automatic x-axis
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "2 years"), format = "%Y")

plot(data$Date, data$Close, 
     type = "p",  
     pch = 5,  
     cex = 0.1,  
     xlab = "Year", 
     ylab = "Closing Price", 
     main = "AEX Closing Prices Over Time",
     col = "red",
     xaxt = "n")  # Suppress automatic x-axis
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "2 years"), format = "%Y")


# Question 1b
boxplot(data$Close, 
        main = "Distribution of Closing Prices",
        ylab = "Closing Price",  
        col = "lightblue",
        outpch = 1)

close <- data$Close

summary(close)
sd(close)
dens2 <- density(close)
hist(close, 
     main = "Histogram of Closing Prices", 
     breaks = 40, 
     xlab = "Closing Prices", 
     prob = TRUE,  
     xlim = range(dens2$x))  
lines(dens2, col = "red", lwd = 2)  

qqnorm(data$Close)
qqline(data$Close, col = "red") 


# Question 1c-d
# Calculate daily return
daily_return <- (data$Close - data$Open) / data$Open

summary(daily_return)
sd(daily_return)

boxplot(daily_return, 
        main = "Distribution of Daily Returns",
        ylab = "Daily Return Rate",  
        col = "lightblue",
        outpch = 1)

dens1 <- density(daily_return)
hist(daily_return,
     main="Histrogram of Daily Returns",
     breaks=40, 
     xlab="Daily Returns",
     prob=TRUE, 
     ylim = c(0, max(dens1$y)))
lines(dens1, col = "red", lwd = 2)

qqnorm(daily_return)
qqline(daily_return, col = "red") 

plot(data$Date, daily_return, 
     type = "p",  
     pch = 5,  
     cex = 0.1,  
     xlab = "Year", 
     ylab = "Daily Returns", 
     main = "Daily Returns Over Time",
     xaxt = "n")  # Suppress automatic x-axis
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "2 years"), format = "%Y")


# Question 1e
# Extract rows between May 31 and September 1 of each year
summer_period <- subset(data, format(Date, "%m-%d") >= "05-31" & format(Date, "%m-%d") <= "09-01")

# Calculate daily returns in summer
daily_return_summer <- (summer_period$Close - summer_period$Open) / summer_period$Open

summary(daily_return_summer)
sd(daily_return_summer)

boxplot(daily_return_summer)

dens <- density(daily_return_summer)
hist(daily_return_summer,
     main="Histrogram of Daily Returns Over Summer",
     breaks=40, 
     xlab="Daily Returns",
     prob=TRUE,
     ylim = c(0, max(dens$y)))
lines(dens, col = "red", lwd = 2)

qqnorm(daily_return_summer)
qqline(daily_return_summer, col = "red") 

plot(summer_period$Date, daily_return_summer, 
     type = "p", 
     pch = 5,  
     cex = 0.1,  #
     xlab = "Year", 
     ylab = "Daily returns", 
     main = "Daily returns Over Summer Period",
     xaxt = "n")  # Suppress automatic x-axis
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "2 years"), format = "%Y")

# Extract rows between November 1 and March 31 of each year
winter_period <- subset(data, (format(Date, "%m-%d") >= "11-01" | format(Date, "%m-%d") <= "03-31"))

# Calculate daily returns in winter
daily_return_winter <- (winter_period$Close - winter_period$Open) / winter_period$Open

summary(daily_return_winter)
sd(daily_return_winter)

# Combine summer and winter returns into a single data frame
combined_returns <- data.frame(
  period = rep(c("Summer", "Winter"), c(length(daily_return_summer), length(daily_return_winter))),
  returns = c(daily_return_summer, daily_return_winter)
)

# Boxplot comparing summer and winter returns
boxplot(returns ~ period, data = combined_returns, 
        main = "Comparison of Daily Returns: Boxplot",
        ylab = "Daily Returns", col = c("orange", "lightblue"))

# Histogram for summer and winter returns
hist(daily_return_summer, breaks = 40, col = rgb(1, 0, 0, 0.5), 
     xlim = range(c(daily_return_summer, daily_return_winter)), 
     main = "Comparison of Daily Returns: Histogram",
     xlab = "Daily Returns", prob = TRUE)
hist(daily_return_winter, breaks = 40, col = rgb(0, 0, 1, 0.5), 
     add = TRUE, prob = TRUE)
legend("topright", legend = c("Summer", "Winter"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

