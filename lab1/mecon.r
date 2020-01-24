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

Y = myvar(x)-var(x)
Y

