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

  ## the lottery does not seem random when the prediction is
  ## made base on loess function. The Draft No seems to be
  ## decreasing as the day of year increases according to the
  ## prediction points on the plot.

# 1-3. Perform bootstrap of B=2000
estimate_data <- data.frame(Y=y_hat, X=Day)
head(estimate_data)
estimate_data[89,]
estimate_data[366,]
