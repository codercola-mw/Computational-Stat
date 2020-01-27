#question1 
x1<-1/3
x2<-1/4
if ( x1-x2==1/12) {
  print ( "Subtraction is correct" )
} else {
  print ( "Subtraction is wrong" )
}

x1<-1 
x2<-1/2
if ( x1-x2==1/2) {
  print ( "Subtraction is correct" )
} else {
  print ( "Subtraction is wrong" )
}

# 1.
# In this case, 1/3 - 1/4 == 1/12 gets the result of "Subtraction is wrong", but
# in the second snippet, 1-1/2 == 1/2 gets the result of "Subtraction is correct".
# We can't get the expected result from the first snippet because the real number 
# of 1/3 is rounded to a floating number, so the computational error will appear 
# when the number is changed. In this case, since the result of 1/3 - 1/4 is ta infinite 
# floating number, so after rounded to a finite floating number, the result has changed 
# and differ from the original result, thus, it is not equal to 1/12.

# We can get the expected result from the second snippet because the real number is not an
# infinite number, so that the computer has enough space to store the whole number.

# 2
# How to improve?
a =round(1/3-1/4, digits = 3)
b = round(1/12, digtis=3)
a == b

# If we round the both numbers with digits equal to 3, then we can get the expected result that
# 1/3 - 1/4 == 1/12



#question 2 

#1.
df <- function(x){
  eps = 10^(-15)
  out = (x + eps - x)/eps
  return(out)
}

#2.
df(1)
#[1] 1.110223
df(100000)
# 0

#3.
# Since 1 is a small value, so when it add up eps(10^-15), the value is still small
# so that the computer can store with some floating-numbers which is 1+eps=1.000000000000001110223,
# that is the reason why the result is 1.11 > 0. 
# On the contrary, the result of df(100000) =0, of which 100000 is a large value so it requires a 
# large storage, thus, the computer
# has no more space to store with the small floating number for 100000. In this case, 
# it means 100000 + 0 - 100000 to computer, so the results becames 0.



# question 3

#1.
myvar <- function(x){
  n = length(x)
  output = 1/(n-1)*(sum(x^2)- (1/n)*(sum(x)^2))
  return(output)
}

#2.
x = rnorm(10000, 10^8, 1)

#3
X=0
for( i in (1:length(x)) ){
  X = x[1:i]
  Y[i]=myvar(X)-var(X)
}

plot(x=(1:length(x)), y=Y, xlab="i(1:100000)", ylab="Y")

# How well does the function work?
# According to the plot, the function works not well in its result. Since we can see
# the most of the variances are not equal to 0, which means a lot of the random value 
# are not exactly equal to the real numbers. Since the random generated numbers are large
# value floating numbers, when we use myvar() function to calcute it's variance will cause
# the inaccurracy. Thus, we couldn't have the same result as var() function does.

#4. Impovement
# Young's algorithm

## v_x is a numerical vector of length greater than 2
## this function calculates the sample variance 
## using the Youngs and Cramer algorithm
var_YC<-function(v_x){
T<-v_x[1]
RSS<-0
n<-length(v_x)
for (j in 2:n){
  T<-T+v_x[j]
  RSS<-RSS+((j*v_x[j]-T)^2)/(j*(j-1))
}
RSS/(n-1)
}

X=0
for( i in (1:length(x)) ){
  X = x[1:i]
  Y[i]=var_YC(X)-var(X)
}

plot(x=(1:length(x)), y=Y, xlab="i(1:100000)", ylab="Y")


# The Young's cramer algothrim works better than myvar(), the values are very closed
# to 0 with lower variances.



#quention4
