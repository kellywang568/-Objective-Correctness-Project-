#' 
#' # A problem requiring a function
#' 

# Modify the body of myfunc().  You may assume that x is a data.frame.
# Your function should return the number of missing values found in
# each column, with the answer being a vector of length ncol(x).

myfunc <- function(x) {
  
  # first apply is.na to each column to get a logical for the number of NAs
  # in each column
  na_values <- apply(x, 2, is.na) 
  # then, summing na_values gives a count of NAs
  ans <- apply(na_values,2,sum)
  return(ans)         # Probably you won't need to modify this
}
