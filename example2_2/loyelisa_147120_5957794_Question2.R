#' 
#' # A new example (in R)
#' 

### Challenge: Write the body of the function, below, which
### seeks to return the data frame with one type of modification:
### Every numeric (but not integer) column should be standardized
### to have mean 0 and variance 1.


myfunc <- function(x) {
  
  # Start your work here
  
  means <- rep(0, length(x))
  sds <- rep(0, length(x))
  
  for (col in 1:length(x)){
    if (is.integer(x[1,col]) == TRUE){
      means[col] <- mean(x[, col])
      sds[col] <- sd(x[, col])
    }
  }
  
  for (col in 1:length(x)){
    if (is.integer(x[1,col]) == TRUE){
      for (row in x)
      x[row, col] <- (x[row, col] - means[col])/sds[col]
    }
  }
  
  # End your work here
  
  return(x)  
}

# Example for your testing (if you want).  It should modify the
# first four columns but leave the last column unchanged.

ans <- myfunc(iris)

summary(iris)
summary(ans)
