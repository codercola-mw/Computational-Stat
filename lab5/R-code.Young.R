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

set.seed(12345)
res <- boot(data=data, statistic = statistic, R=2000)
hist(res$t, breaks=50, main="Histogram of T-Statistics",
     xlab="T Statistic")
cat("The p-value of the distribution of T statistics is: ", "\n")
mean(res$t > 0)
  ## H0: T-stat = 0. H1: T-stat > 0
  ## p-value: probability of obtaining test results at least
  ##     as extreme as the results actually observed during
  ##     the test, assuming that the null hypothesis is correct.
    ##[Aschwanden, Christie (2015-11-24). "Not Even Scientists Can Easily Explain P-values". FiveThirtyEight. Archived from the original on 25 September 2019. Retrieved 11 October 2019.]


  ## When the bootstrap is performed, the resulting histogram
  ## seems that nearly all t-statistics are placed below 0.
  ## However, the p-value under null hypothesis is very small.
  ## (see definition of p-value above)
  ## It would be plausible to conclude that the null 
  ## hypothesis is rejected, therefore the data is not random.
  ## This result may be caused as polinomial regression is
  ## used to make prediction, thus relationship is given
  ## while prediction is made.

## 1-4. Permutation hypothesis test
### the function returns p-value, B=2000

permut_test <- function(data, B){
  stat = numeric(B)
  n = dim(data)[1]
  for (b in 1:B){
    set.seed(b)
    Gb = sample(data$Day_of_year, n)
    fit1 = loess(data$Draft_No~Gb)
    y_hat = predict(fit1, newdata = Gb)
    x_a <- Gb[which.min(y_hat)]
    x_b <- Gb[which.max(y_hat)]
    t_stat <- (max(y_hat)-min(y_hat)) / (x_b-x_a)
    stat[b] <- t_stat
  }
  loess_fit <- loess(Draft_No~Day_of_year, data=data)
  y_hat <- predict(loess_fit, newdata = data$Day_of_year)
  x_a <- data$Day_of_year[which.min(y_hat)]
  x_b <- data$Day_of_year[which.max(y_hat)]
  t_stat <- (max(y_hat)-min(y_hat)) / (x_b-x_a)
  return(mean(abs(stat) >= abs(t_stat)))
}

permut_test(data, 2000)

  ## H0: data is random   H1: data is not random
  ## using two-sided test.

  ## p-value is 0.144 which is not low. It would be 
  ## plausible to conclude that this hypothesis testing
  ## hardly rejects the null hypothesis and thus the data
  ## could be random.


## 1-5. crude estimate of power of test
X <- Day
y_gen <- function(X, alpha){
  n <- dim(X)[1]
  Y <- c()
  for (i in 1:n){
    x <- X[i,1]
    set.seed(i)
    beta <- rnorm(1, mean=183, sd=10)
    y <- max(0, min(366, ((alpha*x)+beta)))
    Y <- append(Y, y)
  }
  return(data.frame(Draft_No=Y))
}

Y <- y_gen(X, alpha=0.1)
new_data <- cbind(X, Y)

permut_test(new_data, 200)

  ## H0: Data is random    H1: Data is not random

  ## Since Y values are generated with accordance to the 
  ## values of X, it is expected that this new data is likely
  ## to be random --- therefore very low p-value is expected
  ## to be returned so that the null hypothesis can be rejected.

  ## when the function is run, returned value is 0.
  ## Null hypothesis is rejected, therefore this data is not\
  ## random.

result <- c()
sequence <- seq(from=0.2, to=10, by=0.1)

for (i in sequence){
  Y <- y_gen(X, alpha=i)
  new_data <- cbind(X, Y)
  
  if (permut_test(new_data, 200) < 0.05) {
    result <- append(result, "Reject")
  } else {
    result <- append(result, "Do not reject")
  }
}

cat("The power of this test is: ", "\n")
length(result=="Reject") / length(result)

  ## power of the test: probability that the test rejects the null hypothesis (H0) when a specific alternative hypothesis (H1) is true.
  ## [https://en.wikipedia.org/wiki/Power_(statistics)]

  ## Since Y value of the dataset is produced based on the
  ## values of X, it is known that the data is not random,
  ## therefore the alternative hypothesis is true.

  ## The power of the test is 1, when the possible values of
  ## alphas are ranged from 0.1 to 10, with significant value
  ## of 0.05.

