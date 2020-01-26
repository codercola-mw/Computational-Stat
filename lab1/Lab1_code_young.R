# 1.
x1 <- 1/3 ; x2 <- 1/4
if (x1-x2 == 1/12) {
  print("Subtraction is correct")
} else {
  print("Subtraction is wrong")
}

isTRUE(all.equal(x1-x2, 1/12))

x1 <- 1 ; x2 <- 1/2
if (x1-x2 == 1/2) {
  print("Subtraction is correct")
} else {
  print("Subtraction is wrong")
}

## Problem of representing floating number in computer
################### MORE DETAIL WHILE WRITING REPORT



# 2.

derivative <- function(x) {
  eps <- 10^(-15)
  res <- ((x+eps)-x)/eps
  return(res)
}

derivative(1)
derivative(100000)

# the way the computer store number is binary
# since 100000 is a large number with less significatn digit,
# very small number, epsilon, is considered to be negligible








# 3.

myvar <- function(X) {
  n <- length(X)
  res <- (sum(X^2) - ((sum(X)^2)/n))/(n-1)
  return(res)
}

set.seed(12345)
x <- rnorm(10000, mean=10^8, sd=1)
y <- c()

for (i in (1:length(x))) {
  ones <- x[1:i]
  res <- myvar(ones) - var(ones)
  y <- append(y, res)
}

y <- y[2:length(y)]
ress <- data.frame(x=2:length(x), y=y)

library(ggplot2)
ggplot(data=ress, mapping=aes(x=x, y=y)) + geom_point(colour="black", 
        size=1) + labs(title="Relationship between Y and i",
            x = "Values of i", y="Values of Y")

setwd("C:/Users/Jooyoung/Documents/Computational Stat/732A90_VT2020_Materials")

data <- read.csv("tecator.csv")
data <- data[,-1]

X <- as.matrix(data[,-102])
Y <- as.matrix(data[,102])
colnames(Y) <- "Protein"

A <- t(X) %*% X
b <- t(X) %*% Y

solve(A,b)
# Error in solving A ... "system is computationally singular"

kappa(A)

scale_X <- scale(X)
scale_Y <- scale(Y)

scale_A <- t(scale_X) %*% scale_X
scale_b <- t(scale_X) %*% scale_Y
solve(scale_A, scale_b)
kappa(scale_A)
