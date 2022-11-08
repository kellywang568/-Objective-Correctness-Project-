#' 
#' # A new example (in R)
#' 

### Challenge: Write the body of the function, below, which
### seeks to return the data frame with one type of modification:
### Every numeric (but not integer) column should be standardized
### to have mean 0 and variance 1.


myfunc <- function(x) {
  
  # Start your work here
  type <- sapply(x, class)
  
  for (i in 1:length(type)) {
    if (type[i] == "numeric" & !is.integer(x[, i])) {
      x[, i] <- (x[, i] - mean(x[, i])) / sd(x[, i])
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
