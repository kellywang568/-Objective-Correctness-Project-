# First read in solution (I just made a simple one)
solution <- parse('Example1_Solution1.R')

# Setting seed for reproducibility since problem depends on random sample
set.seed(123) 
# Make an environment for the solution and evaluate inside the environment
sol_env <- new.env()
eval(solution, envir = sol_env) 
# Note: we expect the instructor's given solution to be correct, so no need to
# account for potential errors. But could be something to keep in mind

# Now we want to read in all of our student submissions and store them as a list

# Would this step be something for the user to do themselves before running, 
# or should there be a function that creates this structure for them
# (either way would be easy, it's a simple step)

files <- list.files("ExampleSubmissions-1", full.names = TRUE)

submissions <- lapply(files, function(x) return(parse(x)))

grade <- function(x, sol = sol_env, var = 'ans'){
  # Now to evaluate our students' code and provide feedback!!
  #print('Next one!')
  set.seed(123)
  try(eval(x))
  
  # Need to account for cases in which variable is not called 'ans'
  # Is there a better way to do that than just eval(paste(...)) ? 
  
  # Also, what happens if the student doesn't have a variable called `ans`
  # (or whatever) in their code? What happens then? Need some way to 
  # check for that.
  
  # testthat expect_equal() ??
  
  # all.equal gives error messages saying exactly HOW the submission differs
  # from the answer key. (type difference, different length, etc)
  # from there we can cluster/sort based on their error messages to identify
  # common mistakes
  
  return(all.equal(sol_env$ans, ans))
  # return list with T/F, comments, and timeout
  # look into testthat/all.equal/etc capabilities of comparing data types
}

# This takes a long time - probably since some students are using loops?
res <- lapply(submissions, grade)

# Evaluate the error messages from the students - we can see some people are 
# getting the same error
table(unlist(res))

