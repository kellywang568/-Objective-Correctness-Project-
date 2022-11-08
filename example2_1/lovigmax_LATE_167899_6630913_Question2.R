#' 
#' # A problem requiring a function
#' 

# Modify the body of myfunc().  You may assume that x is a data.frame.
# Your function should return the number of missing values found in
# each column, with the answer being a vector of length ncol(x).

myfunc <- function(x) {
  numNA <- c(1:length(names(x)))
  dat <- asplit(x, MARGIN = 2)
  for (i in 1:length(dat)){
    numNA <- sum(is.na(dat[i][[1]]))
  }
  return(ans)         # Probably you won't need to modify this
  
}

# I ran out of time so I just submitted at end of class, I dont know how to count the num of NA's