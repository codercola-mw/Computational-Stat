# 1.

## 1-1.
original_data <- read.csv2("~/Computational Stat/732A90_VT2020_Materials/mortality_rate.csv")
new <- data.frame(log_Rate = log(original_data$Rate))
data <- cbind(original_data, new)

n <- dim(data)[1]
set.seed(123456)
id = sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]


## 1-2.
myMSE <- function(lambda, pars){
  fit <- loess(pars$Y~pars$X, enp.target = lambda)
  test_pred <- predict(fit, newdata = pars$Xtest)
  pred_MSE <- sum((pars$Ytest - test_pred)^2)/length(pars$Ytest)
  return(pred_MSE)
}

## 1-3.

#construct list, lambda vector and MSE vector
pars_list <- list(X=train$Day, Y=train$log_Rate, Xtest = test$Day, Ytest = test$log_Rate)
lambda_vect <- seq(from=0.1, to=40, by=0.1)
MSE_vect <-c()

for (i in 1:length(lambda_vect)) {
  res <- myMSE(lambda_vect[i], pars_list)
  MSE_vect <- append(MSE_vect, res)
}

## 1-4.
library(ggplot2)
MSE_vect_df <- data.frame(Lambda=lambda_vect, MSE = MSE_vect)
ggplot(data = MSE_vect_df, mapping=aes(x=Lambda, y=MSE)) + 
  geom_point(colour="black", size=1) + labs(title = "MSE VS Lambda",
        x="Lambda", y="MSE")+
  geom_vline(xintercept = lambda_vect[which.min(MSE_vect)], color="red")
cat("The minimum MSE, ", MSE_vect[which.min(MSE_vect)], 
    ", is obtained when the value of lambda equals ", 
    lambda_vect[which.min(MSE_vect)], "\n")
## but the one mentioned above is the first lambda that obtains minimum MSE.
## to find all indicies that has the minimum MSE, run:
check <- (MSE_vect==min(MSE_vect))
which(check %in% TRUE)
### number of evaluations required:
length(lambda_vect)

which.min(MSE_vect)

## 1-5.

MSE_min <- optimize(myMSE, lambda_vect, pars_list, tol=0.01)
print(MSE_min)
### minimum from optimize function is different from 1-3.
### how many evaluations??

## 1-6.
MSE_min2 <- optim(par=35, fn=myMSE, method="BFGS", pars=pars_list)
MSE_min2$counts


# Question 2

## 2-1.
load("~/Computational Stat/732A90_VT2020_Materials/data.RData")

## 2-2.
n <- length(data)
mu_ml <- sum(data)/n
sigma_ml <- sqrt(sum((data-mu_ml)^2)/n)


## 2-3.

### defining function for minus_llik
minus_llik <- function(par, data) {
  n <- length(data)
  mu <- par[1]
  sigma <- par[2]
  llik <- ((-n/2)*log(2*pi*(sigma^2))) + 
    ((-1/(2*(sigma^2)))*sum((data-mu)^2))
  return(-llik)
}

### defining functin for gradient
grad <- function(par, data) {
  n <- length(data)
  mu <- par[1]
  sigma <- par[2]
  grad_mu <- -(sum(data)-(mu*n))/(sigma^2)
  grad_sigma <- (n/sigma) - (sum((data-mu)^2)/(2*(sigma^3)))
  return(c(grad_mu, grad_sigma))
}

CG_without <- optim(par=c(0,1), fn=minus_llik, method="CG", data=data)
CG_with <- optim(par=c(0,1), fn=minus_llik, method="CG", data=data2, gr=grad)

BFGS_without <- optim(par=c(0,1), fn=minus_llik, method="BFGS", data=data2)
BFGS_with <- optim(par=c(0,1), fn=minus_llik, method="BFGS", data=data2, gr=grad)

### why is it bad to use just likelihood function?

## 2-4.
