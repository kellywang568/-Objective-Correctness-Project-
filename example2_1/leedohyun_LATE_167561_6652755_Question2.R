#' 
#' # A problem requiring a function
#' 

# Modify the body of myfunc().  You may assume that x is a data.frame.
# Your function should return the number of missing values found in
# each column, with the answer being a vector of length ncol(x).

myfunc <- function(x) {
  # Use sapply to find the sum of NA's for every column
  ans <- sapply(x, function(n) sum(is.na(n)))         
  return(ans)        
}
