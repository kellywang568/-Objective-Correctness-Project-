#' 
#' # A new example (in R)
#' 

## CANDICE DJORNO

### Challenge: Write the body of the function, below, which
### seeks to return the data frame with one type of modification:
### Every numeric (but not integer) column should be standardized
### to have mean 0 and variance 1.


myfunc <- function(x) {
  
  # Start your work here
  numerics <- unlist(lapply(x, is.numeric))
  numeric_dat <- x[, numerics] 
  numeric_dat <- scale(numeric_dat)
  x[, numerics] <- numeric_dat
  
  # End your work here
  
  return(x)  
}

# Example for your testing (if you want).  It should modify the
# first four columns but leave the last column unchanged.

ans <- myfunc(iris)

summary(iris)
summary(ans)
