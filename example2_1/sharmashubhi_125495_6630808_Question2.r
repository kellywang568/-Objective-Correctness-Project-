#' 
#' # A problem requiring a function
#' 

# Modify the body of myfunc().  You may assume that x is a data.frame.
# Your function should return the number of missing values found in
# each column, with the answer being a vector of length ncol(x).

myfunc <- function(x) {
  
  # ans <- NULL         # Replace this line with one or more lines of code
  ans <- apply(x, 2, function(w) sum(is.na(w)))

  return(ans)         # Probably you won't need to modify this
  
}

