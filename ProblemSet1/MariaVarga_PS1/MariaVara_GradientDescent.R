#  ----------------------------------------------------------------------------------
#  Problem 2 - Optimization
#  MARIA VARGA
#
# Gradient Descent optimization routine
#
#  ---------------------------------------------------------------------------------

library(tidyverse)

# get data 
adv <- read_csv('Advertising.csv')

# add column of 1's
adv$x0 <- 1 

# create the x- matrix of explanatory variables
x <- as.matrix(adv[c('x0','TV','Radio','Newspaper')])

# create the y-matrix of dependent variables
y <- as.matrix(adv['Sales'])
m <- nrow(y)

#  feature normalizating
x.scaled <- x
for (i in 2:dim(x)[2]) {
  x.scaled[,i] <- (x[,i] - mean(x[,i]))/sd(x[,i])
}
  

# define the gradient function dJ/dtheata
grad <- function(x, y, theta) {
  gradient <- (1/m)* (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}


# define gradient descent  algorithm
grad.descent <- function(x, num_iter, alpha = .01){
  theta <- matrix(rnorm(dim(x)[2]), nrow=1) # random initialize the parameters
  
  for (i in 1:num_iter) {
    theta <- theta - alpha  * grad(x, y, theta)   
  }
  return(theta)
}

set.seed(3)

# results without feature normalizating
print(grad.descent(x,100000,alpha = .00001))

#x0         TV     Radio Newspaper
#Sales -0.4253572 0.05495201 0.2271233  0.019397


# results with feature normalizating
print(grad.descent(x.scaled,1000, alpha = .01))

#x0       TV    Radio   Newspaper
#Sales 14.0219 3.928884 2.796029 -0.01949891


# results using lm function without feature normalizating
summary(lm(y ~ x[, 2:4])) 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        2.938889   0.311908   9.422   <2e-16 ***
#   x[, 2:4]TV         0.045765   0.001395  32.809   <2e-16 ***
#   x[, 2:4]Radio      0.188530   0.008611  21.893   <2e-16 ***
#   x[, 2:4]Newspaper -0.001037   0.005871  -0.177     0.86   

# results using lm function with feature normalizating
summary(lm(y ~ x.scaled[, 2:4]))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               14.0225     0.1192 117.655   <2e-16 ***
#   x.scaled[, 2:4]TV          3.9291     0.1198  32.809   <2e-16 ***
#   x.scaled[, 2:4]Radio       2.7991     0.1278  21.893   <2e-16 ***
#   x.scaled[, 2:4]Newspaper  -0.0226     0.1279  -0.177     0.86  


#  ----------------------------------------------------------------------------------
#  CONCLUSION:

# It was required to use a very small learning rate in case of not normalizing the features, due to the fact that the algorithm diverges
# When using lower learning rate, it is usually required more iteration and, consequently more time to train the model.
# However, this was still not enough to get exactly the same coefficients estimatives obtained using lm function (OLS estimator)
# On the other hand, when using feature scaling the algorithm can converge much faster using a higher learning rate.
# Comparing to the lm model, we can see that it almost perfectly converge to the OLS estimator after 1000 iteration
#  ---------------------------------------------------------------------------------



