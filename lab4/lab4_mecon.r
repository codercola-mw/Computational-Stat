
RNGversion("3.5.1")
#question1

set.seed(12345)
MH_normal <- function(tmax, X0){
  t=1
  X=rep(X0,tmax)
  while(t < tmax){
    Y <- rlnorm(1, X[t], 1)
    U <- runif(1,0,1)
  
    alpha_fun <- function(X,Y){
      pi_Y = (Y^5)*exp(-Y)
      pi_Xt = (X^5)*exp(-X)
      q_Xt = dlnorm(X, Y,1)
      q_Y = dlnorm(Y,X,1)
      out = min(1,pi_Y*q_Xt/(pi_Xt*q_Y))
      return(out)
    }
      if(U < alpha_fun(X[t],Y)){
        X[t+1] = Y
      }else{
        X[t+1] = X[t]
        }
    t=t+1
  }
　return(X)
}
tmax=1000
X0=1
plot(1:tmax, MH_normal(tmax,X0),"l", xlab="Time Series", ylab="X", main="Normal MCMC")


# 2
MH_chi <- function(tmax, X0){
  t=1
  X=rep(X0,tmax)
  while(t < tmax){
    Y <- rchisq(1, X[t]+1)
    U <- runif(1,0,1)
    
    alpha_fun <- function(X,Y){
      pi_Y = (Y^5)*exp(-Y)
      pi_Xt = (X^5)*exp(-X)
      q_Xt = dchisq(X, Y+1)
      q_Y = dchisq(Y,X+1)
      out = min(1,pi_Y*q_Xt/(pi_Xt*q_Y))
      return(out)
    }
    if(U < alpha_fun(X[t],Y)){
      X[t+1] = Y # floor?
    }else{
      X[t+1] = X[t]
    }
    t=t+1
  }
  return(X)
}

tmax=500
Xt = 1
plot(1:tmax, MH_chi(tmax,Xt))  


# 3


