#' 
#' # Question 3
#' 

### Challenge: Write the body of the function, below, which
### takes as its only argument an integer greater than 1 and returns a vector
### of all prime numbers less than or equal to that integer.

primefunc <- function(n) {
  prime <- c()
  if (n <= 1) {
    return(NA)
  }
  else {
    for (i in c(2:n)) {
      #if a 0 is not included in the mods list 
      prime <- ifelse(! 0 %in% i %% 2:(i-1), append(n,prime), NA)   
    }
    return(prime)
  }
}


# For testing:
primefunc(2)      # Should return 2
primefunc(3)      # Should return c(2, 3)

### ProblemEndsHere ###

