#  ----------------------------------------------------------------------------------
#  Problem 2 – Optimizing the Likelihood Function
#  MARIA VARGA
#
# Gradient Descent optimization routine applyed to maximum likelihood
#
#  ---------------------------------------------------------------------------------

library(tidyverse)

# get data 
adv <- read_csv('Advertising.csv')

# add column of 1's
adv$x0 <- 1 

# create the x- matrix of explanatory variables
x <- as.matrix(adv[c('x0','TV')])

# create the y-matrix of dependent variables
y <- as.matrix(adv['Sales'])
m <- nrow(y)

#  feature normalization
x.scaled <- x
for (i in 2:dim(x)[2]) {
  x.scaled[,i] <- (x[,i] - mean(x[,i]))/sd(x[,i])
}


# define the gradient functions for theta0 and theta1
grad <- function(x, y, theta,sigma) {
  gradient <- -(1/sigma^2)* (t(x) %*% (y - (x %*% t(theta)) ))
  return(t(gradient))
}

# define the gradient functions for sigma
sig <- function(x, y, theta,sigma) {
  sig <- (m*sigma^-1)-(sigma^-3)*sum(y - (x %*% t(theta)) )
  return(sig)
}



# define gradient descent  algorithm
grad.descent <- function(x, num_iter, alpha = .01){
  theta <- matrix(rnorm(dim(x)[2]), nrow=1) # random initialize the parameters
  sigma <- rnorm(1) # random initialize sigma
  
  for (i in 1:num_iter) {
    theta <- theta - alpha*grad(x, y, theta, sigma)   
    sigma <- sigma - alpha*sig(x, y, theta, sigma) 
  }
  return(theta)
}

set.seed(3)

# Estimation by Maximum Likelihood using Gradint Decent
# results with feature normalization
print(grad.descent(x.scaled,100000, alpha = .0001))
#         x0       TV
#Sales 14.02964 4.082675


# results using lm function with feature normalizating (least-squares )
model<-lm(y ~ x.scaled[,2])
print(model$coefficients)
#(Intercept) x.scaled[, 2] 
#14.022500      4.081222


#  ----------------------------------------------------------------------------------
#  CONCLUSION:

# Gradient Descent applyed to the maximum likelihood approach converges to the least-squares estimators.
#  ---------------------------------------------------------------------------------



