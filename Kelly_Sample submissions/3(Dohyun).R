#'
#' FOR S&DS 662 on November 1, 2022: 
#' 
#' 1. Problem 1 you have done before.  Here, please provide a different
#' solution from what you submitted earlier this term (if you remember).
#' If you don't remember, you can do whatever you want.  Don't spend more
#' than 5 minutes on this.  Really.
#' 
#' 2. Problems 2 and 3 are new (and are from two of you from earlier this
#' term).  Please choose ONE of these and attempt a solution -- leave the
#' other one as it is.  Do not attempt to solve both problems today!  Don't
#' waste time working on one and then switching!  If you were confronted with
#' advice like this in a job interview/coding test, take it seriously.
#' Attempting two questions and not doing either particularly well would be
#' a poor strategy.
#' 
#' 3. Upload (before 5:15 PM, the end of class) to Canvas.
#' 

### ProblemStartsHere ###

#' 
#' # Problem 1
#' 

NR <- 4
NC <- 100000
x <- matrix(sample(1:10, NR*NC, replace = TRUE), NR, NC)

### Challenge: calculate the column means; save the answer
### in a vector of length 'NC' called 'ans'.  

#'
#' ### Your work below
ans <- colMeans(x)
#' 
#' When this script is run, it should have created a vector of length
#' `NC` called `ans`.  Obviously we expect it to be numeric and have
#' values that are often close to 5.5 plus or minus a little given
#' what we know about probability and sampling.
#' 



### ProblemEndsHere ###

### ProblemStartsHere ###

#' 
#' # Question 2
#' 
#' Consider a well-shuffled deck of $n$ cards, labelled $1$ through $n$.
#' You flip over the cards one by one, saying the numbers $1$ through $n$
#' as you do.  That is, as you turn over the first card, you say "ONE".
#' As you turn over the second card, you say "TWO" and so on.
#' 
#' You yell "SNAP" (and win the game) if, at some point, the number you 
#' say aloud is the same as the number on the card being flipped over 
#' (for example, if the $5^{th}$ card in the deck is in fact card number 5,
#' you yell "SNAP").  Clearly, you lose the game if this never happens by
#' the time you reach the end of the deck.  Using a simulation, estimate
#' the probability of winning the game (i.e. yelling "SNAP").
#'
#' Conduct a simulation to assign your answer (an estimated probability) to
#' an object called 'ans'.  Your answer will be approximate but
#' you should be fairly confident in its accuracy to 3 decimal places.




### ProblemEndsHere ###

### ProblemStartsHere ###

#' 
#' # Question 3
#' 

### Challenge: Write the body of the function, below, which
### takes as its only argument an integer greater than 1 and returns a vector
### of all prime numbers less than or equal to that integer.

is_prime <- function(n) {
  for (i in 2:n) {
    if (n %% i == 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

primefunc <- function(n) {
  ans <- c()
  if (n <= 1) {
    return("Enter an integer greater than 1")
  }
  for (i in 2:n) {
    if (is_prime(i)) {
      ans <- c(ans, i)
    }
  }
  return(ans)  
}

# For testing:
primefunc(2)      # Should return 2
primefunc(3)      # Should return c(2, 3)
primefunc(7)

#' Function isn't working the way it's supposed to -- I ran into trouble getting
#' 2 into ans. I made a function that checks if a number is prime and primefunc
#' adds that value if it is prime. I think it's the way I'm making my loop or
#' checking if my values are prime.
