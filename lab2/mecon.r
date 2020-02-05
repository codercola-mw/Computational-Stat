RNGversion("3.5.1")
#Question 1
data = read.csv2("~/Desktop/Computational-Stat/lab2/mortality_rate.csv")

#1
data$LMR = log10(data$Rate)

n = dim(data)[1]
set.seed(123456)
id=sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]

#2



myMSE <- function(lambda, pars){
  loess_model =loess(pars$Y~pars$X, enp.target=lambda)
  fYpred = predict(loess_model, newdata=pars$Xtest)
  # test_set=c()
  # fYpred = as.vector(fYpred)
  # for(i in 1:length(pars$Ytest)){
  #   test_set[i] = (pars$Ytest[i]-fYpred[i])^2
  # }
  Pred_MSE = mean((pars$Ytest-fYpred)^2)
  return(Pred_MSE)
}

#3

pars = list(X=train$Day, Y=train$LMR, Xtest=test$Day, Ytest=test$LMR)

lambda = seq(0.1,40,0.1)
mse=c()
for( i in 1:length(lambda) ){
  mse[i]=myMSE(lambda[i], pars)
}

#4
plot(x=lambda, y = mse)

# 5
lambda[which.min(mse)]
mse[which.min(mse)]

# > lambda[which.min(mse)]
# [1] 11.7
# > mse[which.min(mse)]
# [1] 0.02471699


optimize(myMSE, c(0.1,40), tol=0.01, pars=pars)

#$minimum
#[1] 10.69361

#$objective
#[1] 0.02492393

# 6

optim(35, myMSE, method = "BFGS", pars=pars)
# $value
# [1] 0.03244114



# question 2

a <- load("~/Desktop/732A90_VT2020_Materials/data.RData",verbose = T)


