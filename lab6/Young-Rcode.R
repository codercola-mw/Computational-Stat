# Question 1

## 1-1. Defining a function
f <- function(x){
  return(((x^2)/(exp(x)))-(2*exp(-(9*sin(x))/((x^2)+x+1))))
}

## 1-2. Defining a function
crossover <- function(x,y){
  return((x+y)/2)
}

## 1-3. Defining a function
mutate <- function(x){
  return(x%%30)
}

## 1-4. Write a function that depends on..

genetic <- function(maxiter, mutprob){
  
  # a,b) plotting f, defining initial population
  x <- seq(from=0, to=30, by=0.1)
  plot(x=x, y=f(x), main = c("Maxiter: ", maxiter, "Mutprob: ", mutprob), xlab="Population",
       ylab="Values", ylim=c(-3, 1), type="l")
  x <- seq(from=0, to=30, by=5)
  points(x=x, y=f(x))
  abline(v=x[which.max(f(x))], col="green")
    ## The maximum value is easily found. On the plot, its
    ## position is shown in green colored line

  # c) computes vector Values for each population point
  Values <- f(x)
  
  # d) performs maxiter iterations
  current_pop = x
  max_vals = vector(length = maxiter)
  for (i in 1:maxiter){
    ## i) sampling two indexes from the current population
    parents = sample(1:length(current_pop), size=2)
    ## ii) index of the smallest objective function
    victim = order(Values)[1]
    ## iii) crossover or mutation
#    kid = crossover(current_pop[parents[1]], current_pop[parents[2]])
#    u <- runif(1)
#    if (u < mutprob) {kid = mutate(kid)}
    cross_kid = crossover(current_pop[parents[1]], current_pop[parents[2]])
    mut_kid = mutate(cross_kid)
    kid = sample(x = c(mut_kid, cross_kid), size=1, 
                 prob=c(mutprob, 1-mutprob))
    ## iv) replacing victim with kid
    current_pop[victim] = kid
    Values[victim] = f(kid)
    max_vals[i] = max(Values)
  }
  points(x=current_pop, y=Values, col="red", pch=3)
  cat("Maximum value with ", mutprob, " mutation probability is:", "\n")
  return(max(max_vals))
}

## 1-5.
par(mfrow=c(1,2))
set.seed(12345)
genetic(maxiter=10, mutprob=0.1)
genetic(maxiter=100, mutprob=0.1)

genetic(maxiter=10, mutprob=0.5)
genetic(maxiter=100, mutprob=0.5)

genetic(maxiter=10, mutprob=0.9)
genetic(maxiter=100, mutprob=0.9)

  ## As the number of maxiter increases, the current population
  ## tends to converge.

  ## When the iterations are over, the maximum value obtained 
  ## through it never decreases from the maximum value obtained
  ## using the initial population.
