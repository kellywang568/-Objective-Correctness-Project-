#' # A problem requiring a function
#' 

# Modify the body of myfunc().  You may assume that x is a data.frame.
# Your function should return the number of missing values found in
# each column, with the answer being a vector of length ncol(x).

myfunc <- function(x) {
  ans <- colSums(is.na(x))         # Replace this line with one or more lines of code
  return(ans)         # Probably you won't need to modify this
}
#'
#' Testing
#' 
# Name <- c("Jon", "Bill", "Maria", "Ben", "Tina", NA)
# Age <- c(23, 41, 32,  NA, 26, NA)
# df <- data.frame(Name, Age)
# myfunc(df)

