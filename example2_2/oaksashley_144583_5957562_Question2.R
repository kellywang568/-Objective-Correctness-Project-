#' 
#' # A new example (in R)
#' 

### Challenge: Write the body of the function, below, which
### seeks to return the data frame with one type of modification:
### Every numeric (but not integer) column should be standardized
### to have mean 0 and variance 1.
install.packages("dplyr")                            # Install dplyr
library("dplyr") 

myfunc <- function(x) {
  
  # Start your work here
  col <- select_if(x,is.numeric)
  scaled.x <- scale(col)
  
  
  
  # End your work here
  
  return(scaled.x)  
}

# Example for your testing (if you want).  It should modify the
# first four columns but leave the last column unchanged.

ans1 <- myfunc(iris)

summary(iris)
summary(ans1)
