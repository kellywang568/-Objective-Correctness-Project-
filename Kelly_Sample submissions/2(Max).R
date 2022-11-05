
### ProblemStartsHere ###

#' 
#' # Question 3
#' 

### Challenge: Write the body of the function, below, which
### takes as its only argument an integer greater than 1 and returns a vector
### of all prime numbers less than or equal to that integer.

primefunc <- function(n) {
  
  # Start your work here
  pos <- c(F, rep(T, n-1))
  if (n >= 4){
    for (i in 4:n){
      for (j in 2:floor(sqrt(i))){
        if((i / j) %% 1 == 0){
          pos[i] <- F
          break
        }
      }
    }
  }

  ans <- (1:n)[pos]
  
  # End your work here
  
  return(ans)  
}

# For testing:
primefunc(2)      # Should return 2
primefunc(3)      # Should return c(2, 3)

### ProblemEndsHere ###
