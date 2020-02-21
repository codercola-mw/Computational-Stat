# Question 1

## 1-1. Import data, make scatter plot

lottery_data <- readxl::read_xls("~/Computational Stat/732A90_VT2020_Materials/lottery.xls")
Day <- lottery_data[colnames(lottery_data)=="Day_of_year"]
Draft <- lottery_data[colnames(lottery_data)=="Draft_No"]
data <- data.frame(X=Day, Y=Draft)
plot(x=data$Day_of_year, y=data$Draft_No,
     main="Scatter Plot of Draft No VS Day of Year",
     xlab = "Day of year", ylab="Draft No")
  ## Above plot seems that draft number is independent 
  ## of day of year

## 1-2. Plot estimated Y using loess on the same plot

loess_fit <- loess(Draft_No~Day_of_year, data=data)
y_hat <- predict(loess_fit, newdata = data$Day_of_year)
points(y_hat, col="red")
legend("topright",legend=c("Original Data", "Fitted Points"), 
       col=c("black", "red"), pch=c(1,1))

  ## the lottery does not seem random when the prediction is
  ## made base on loess function. The Draft No seems to be
  ## decreasing as the day of year increases according to the
  ## prediction points on the plot.

# 1-3. Perform bootstrap of B=2000

library(boot)
statistic <- function(data, vn){
  data <- data[vn,]
  loess_fit <- loess(Draft_No~Day_of_year, data=data)
  y_hat <- predict(loess_fit, newdata = data$Day_of_year)
  x_a <- data$Day_of_year[which.min(y_hat)]
  x_b <- data$Day_of_year[which.max(y_hat)]
  t_stat <- (max(y_hat)-min(y_hat)) / (x_b-x_a)
  return(t_stat)
}
res <- boot(data=data, statistic = statistic, R=2000)
hist(res$t, breaks=30, main="Histogram of T-Statistics",
     xlab="T Statistic")
cat("The p-value of the distribution of T statistics is: ", "\n")
mean(res$t > 0)
  ## When the bootstrap is performed, the resulting histogram
  ## seems that nearly all t-statistics are placed below 0.
  ## Furthermore, the p-value is very small.
  ## It would be plausible to conclude that this data is random.