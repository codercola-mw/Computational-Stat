# Question 1
data1 =readxl::read_xls("~/Desktop/732A90_VT2020_Materials/lottery.xls") 

X = data1$Day_of_year
Y = data1$Draft_No

### 1.
plot(Y,X, "p")

# Yes, it's so random

### 2.
estimate_Y = loess(Y~X, data=data1)


plot(Y,X, "p")
lines(X, estimate_Y$fitted,col="red")

#The line is slightly going downward, it might implied at the end of the year, the number of draft going lower.

### 3. use the test statistics to check whether the lottery is random



library(boot)
stat1 <- function(data,vn){
    data<- data[vn,]
    loess_fun = loess(Draft_No~Day_of_year, data=data)
    yhat = loess_fun$fitted
    Xb= data$Day_of_year[which.max(yhat)]
    Xa = data$Day_of_year[which.min(yhat)]
    stat0 <- (max(yhat)-min(yhat))/(Xb-Xa)
    
    return(stat0)
  
}

set.seed(12345)
res = boot(data=data1,stat=stat1, R=2000)
hist(res$t, breaks=50, main="Histogram of T-Statistics",
     xlab="T Statistic")

# H0= T>0, H1 = T<=0
mean(res$t>0)
## From the histogram, we can see all the values are below zero.
## So, after we calculate the P-value = 0.001<0.05 which is true, so 
## we reject H0, meaning that the data is not random

### 4.using a permutation test

# H0: it is random, H1: It is non-random

permutation <- function(data,B){
  stat=numeric(B)
  n=dim(data)[1]
  for(b in 1:B){
    Gb = sample(data$Day_of_year,n)
    loess_fun = loess(Draft_No~Gb,data=data)
    yhat = loess_fun$fitted
    Xb= Gb[which.max(yhat)]
    Xa = Gb[which.min(yhat)]
    stat[b]=(max(yhat)-min(yhat))/(Xb-Xa)
  }
  
  loess_fun = loess(Draft_No~Day_of_year, data=data)
  yhat = loess_fun$fitted
  Xb= data$Day_of_year[which.max(yhat)]
  Xa = data$Day_of_year[which.min(yhat)]
  stat0=(max(yhat)-min(yhat))/(Xb-Xa)
 
  res = mean(abs(stat)>=abs(stat0))
  return(res)
}
set.seed(12345)
permutation(data1, 2000)
#0.156 > 0.05, so we cant reject the H0

### 5. make a crude estimate of the power in step4

new_Y=c()

for( i in 1:366){
  beta = rnorm(n=1, mean=183, sd=10)
  new_Y[i] =max(0, min((0.1*X +beta), 366))
}

new_data= cbind("Day_of_year"=X,"Draft_No"=new_Y)
new_data= as.data.frame(new_data)
set.seed(12345)
permutation(new_data,200)


## repeat the step


# Question 2
data =read.csv2("~/Desktop/732A90_VT2020_Materials/prices1.csv") 
hist(data$Price)

cat("The mean of the housing price:\n")
mean(data$Price)
#1080.473
set.seed(12345)
B = 2000
n = dim(data)[1]
stat = c()
for (b in 1:B){
  bootstrap_sample = sample(data$Price, n, replace=TRUE)
  stat[b] = mean(bootstrap_sample)
}
# mean(stat)
#Bias-corrected estimator
T1 = 2*mean(data$Price)-mean(stat)
# > T1
# [1] 1080.819

#the variance of estimator
var_boot = sum((stat-mean(stat))^2)/(B-1)
#> var_boot
#[1] 1280.552

#### use the boot* find the bias-correction
stat <- function(data,vn){
  data<- data[vn,]
  stat0 <- mean(data)
  return(stat0)
}
price = as.matrix(data$Price)
res <- boot(data=price, stat, R=1000)

# > mean(res$t)
# [1] 1079.24
#bias    std. error
#0.4852364    34.71439

cat("95% confidence interval is: \n")
print(boot.ci(res))

### 3 . jackknife
B = 1000
n = dim(data)[1]
T_i = c()
stat = mean(data$Price)
for (b in 1:n ){
  
  T_i[b] = n*stat-(n-1)*mean(data$Price[-b])
}
J_T = mean(T_i)

var_jack = sum((T_i-J_T)^2)/(n*(n-1))


### 4. compare with the C.I
