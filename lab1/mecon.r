#question1 

#the real number is rounded to a floating number, so 
# the computational error will appear when the number is 
# changed. In this case, since the result is the infinite 
# floating number, so after rounded to a finite floating number
# the result has changed and differ from the original result.


#question 2

deriv()


df <- function(x){
  eps = 10^(-15)
  out = (x + eps - x)/eps
  return(out)
}

df(1)
#[1] 1.110223
df(100000)
0

# The reason of this one because
# 1 + epx ( close to 1 ) -1 , 
# 100000 + epx -100000, 



# question 3

myvar <- function(x){
  n = length(x)
  output = 1/(n-1)*(sum(x^2)- (1/n)*(sum(x)^2))
  return(output)
}

x = rnorm(10000, 10^8, 1)

X=0
for( i in (1:length(x)) ){
  X = x[1:i]
  Y[i]=myvar(X)-var(X)
}

plot(x=(1:length(x)), y=Y, xlab="i(1:100000)", ylab="Y")


# Young's algorithm

## v_x is a numerical vector of length greater than 2
## this function calculates the sample variance 
## using the Youngs and Cramer algorithm
T<-v_x[1]
RSS<-0
n<-length(v_x)
for (j in 2:n){
  T<-T+v_x[j]
  RSS<-RSS+((j*v_x[j]-T)^2)/(j*(j-1))
}
RSS/(n-1)
}

