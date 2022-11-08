#' 
#' # A problem requiring a function
#' 

# Modify the body of myfunc().  You may assume that x is a data.frame.
# Your function should return the number of missing values found in
# each column, with the answer being a vector of length ncol(x).

myfunc <- function(x) {
  ans <- vector(mode = "character", length = ncol(x))
  ans <- colSums(is.na(x))   
  return(ans)         
}
mydata <- data.frame(x1 = 1:5,   
                   x2 = LETTERS[1:5],
                   x3 = c(4, 9, NA, 0, 1),
                   x4 = c("sahil", NA, "siddharth", "Alex", NA))
mydata
myfunc(mydata) 
