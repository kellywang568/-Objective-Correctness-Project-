#' Check the Objective Correctness of Submitted Code
#'
#' @param files a character vector of file names possibly with optional paths
#' (and R and not Rmd at the moment)
#' @param solution_file path to a solution script (R and not Rmd at the moment)
#' @param timeout timeout threshold.
#' @param ... optional arguments to FUN.
#' @return Dumb pasted info at this point
#' @examples
#' \dontrun{
#'   # Update this example for this function...
#'   correctCheck_directory('ClinicSubmissions-1',
#'                          'Solutions-1/Solution3.R',
#'                          timeout = 10)
#' }
#' @export
correctCheck_filesdir <- function(files, solution_file, timeout = 10, ...) {

  # Not happy with the name of this function.  Definitely change, but wait
  # until we're 100% sure what we really want later on.

  # Consider documenting some of these functions together rather than
  # separately... good thing to look up in Hadley's package book.

  # How many submissions were given to me here?
  n <- length(files)
  # Initilize a list
  k <- vector("list", n)
  # Perform correctCheck on all files to the solution file
  for (i in 1:n) {
    cat("\n\nChecking", files[i], "\n")
    k[[i]] <- correctCheck_file(files[i], solution_file, timeout = timeout, ...)
  }
  return(k)
}

#' Check the Objective Correctness of Submitted Code
#'
#' @param student_dir path to a single directory with submitted solution
#' scripts (R and not Rmd at the moment)
#' @param solution_file path to a solution script (R and not Rmd at the moment)
#' @param timeout timeout threshold.
#' @param ... optional arguments to FUN.
#' @return Dumb pasted info at this point
#' @examples
#' \dontrun{
#'   correctCheck_directory('ClinicSubmissions-1', 'Solutions-1/Solution3.R',
#'                          timeout = 10)
#' }
#' @export
correctCheck_directory <- function(student_dir, solution_file,
                                   timeout = 10, ...) {

  # This design is possibly fine even if student_dir is a vector of paths,
  # but we need to revisit once we think about possible gradebook usage or
  # some database later on that might require a single dir rather than
  # multiple sections with special dirs.

  # Submissions
  files <- dir(student_dir, full.names = TRUE)
  # How many submissions in student_dir
  n <- length(files)
  # Initilize a list
  k <- vector("list", n)
  # Perform correctCheck on all files to the solution file
  for (i in 1:n) {
    k[[i]] <- correctCheck_file(files[i], solution_file, timeout = timeout, ...)
  }
  return(k)
}

#' Check the Objective Correctness of Submitted Code
#'
#' @param student_file path to a submitted script (R and not Rmd at the moment)
#' @param solution_file path to a solution script (R and not Rmd at the moment)
#' @param timeout timeout threshold.
#' @param ... optional arguments to FUN.
#' @examples
#' \dontrun{
#' correctCheck_file('Submission.R', 'Solution.R', timeout = 10)
#' correctCheck_file('Submission.R', 'Solution.R', timeout = 10, iris)
#' }
#' @export
correctCheck_file <- function(student_file, solution_file, timeout = 10, ...) {
  # Using wrapper function to get results of solution script
  x <- eval_file(student_file, timeout = timeout)

  # If student file had a timeout:
  if (identical(x, 'Timeout!')) {
    return(list(student_file = student_file,
                timeout = TRUE,
                codefailure = FALSE,
                correctCheck = 0,
                message = "Timeout"))
  }

  # If student file had a code failure:
  if (identical(x, 'Code Failure!')) {
    return(list(student_file = student_file,
                timeout = FALSE,
                codefailure = TRUE,
                correctCheck = 0,
                message = "Code failure"))
  }

  # If no problems we run test:
  ans <- correctCheck_object(x, solution_file, timeout, ...)
  return(list(student_file = student_file,
              timeout = ans$timeout,
              codefailure = ans$codefailure,
              correctCheck = ans$score,
              message = ans$message))
}

#' Check the Objective Correctness of An Object
#' @param x the object that is being compared to `solution_file`
#' @param solution_file path to a submitted solution script (R and not Rmd at
#' the moment)
#' @param timeout timeout threshold.
#' @param ... optional arguments to FUN.
#' @details The last line of code (non-empty, non-comment line) in the solution
#' file is what is being evaluated. This will be an important RULE to document!
#' @examples
#' \dontrun{
#' correctCheck_object(answer, 'Solution.R', timeout = 10)
#' correctCheck_object(answer, 'Solution.R', timeout = 10, iris)
#' }
#' @export
correctCheck_object <- function(x, solution_file, timeout = 10, ...) {

  # Again, revisit the choice of _object above, as it doesn't feel quite
  # right.  We've already done eval_file(x) prior to getting here.

  # Using wrapper function to get results of solution script
  s <- eval_file(solution_file, timeout)

  # Here, both x and s have survived the application of 'eval_file'.

  # Check if s had a timeout, stop if so
  if (identical(s, 'Timeout!')) {
    stop('Timeout in solution file; script not able to finish')
  }

  # Check if s had a code failure, stop if so
  if (identical(s, 'Code Failure!')) {
    stop('Instructure code failure!')
  }

  # Step 1: Check if s is a function, if so, run it on '...' (w. timeoutCatch)
  if (is.function(s)) {
    # Returns the results from the script run on '...'
    sv <- timeoutCatch(s, timeout, ...)

    # Check if sv had a timeout, stop if so
    if (identical(sv, 'Timeout!')) {
      stop('Timeout in solution file; function not able to finish')
    }

    # Check if sv had a code failure, stop if so
    if (identical(sv, 'Code Failure!')) {
      stop('Instructure code failure!')
    }

    # Step 2: Check if x is a function, if so, run it on ... (w. timeoutCatch)
    if (is.function(x)) {
      xv <- timeoutCatch(x, timeout, ...)
    } else {
      xv <- 'Not a function!'
    }
    # Assign back to first values
    s <- sv
    x <- xv
  }

  # Collect timeouts and code failures
  m <- NA
  xtimeout <- identical(x, 'Timeout!')
  xcf <- identical(x, 'Code Failure!')
  if (identical(x, 'Not a function!')) {
    xcf <- NA
    m <- x
  }

  #### NOTE: SHOULD WE LET INSTRUCTOR KNOW 'Not a function!'?

  # If s is null throw a warning and output
  if (is.null(s)) warning('Solution script produces NULL as the answer')
  tests <- correctTests(x,s)
  return(list(score = tests$score,
              timeout = xtimeout,
              codefailure = xcf,
              message = tests$comments))
}

#' Eval an R File
#' @param filename path to a R script (R and not Rmd at the moment)
#' @param timeout timeout threshold.
#' @details We will need to deduce the name of the object from the
#' last line of code (non-empty, non-comment line) in the solution file
#' and this will be an important RULE to document/explain...
#' @examples
#' \dontrun{
#' eval_file('Solution.R', timeout = 10)
#' }
eval_file <- function(filename, timeout = 10) {
  # Create a function wrapper around a script
  mytxt <- c("function() {", "set.seed(1)", readLines(filename, warn = FALSE), 
             "return(ans) }")
  # Set all set.seeds at set.seed(1)
  mytxt <- gsub("set.seed\\(*\\d*\\)", "set.seed\\(1\\)", mytxt)  # REVISIT
  # Create a function
  myfunc <- eval(parse(text = mytxt))
  # Returns the results from the script
  output <- timeoutCatch(myfunc, timeout)
  return(output)
}


#' Try Catch for internal use
#' @param func the function to be evaluated.
#' @param timeout timeout threshold.
#' @param ... optional arguments to FUN.
#' @importFrom R.utils withTimeout
#' @examples
#' \dontrun{
#' timeoutCatch(myfunction, timeout = 10, iris)
#' }
timeoutCatch <- function(func, timeout = 10, ...) {
  val <- tryCatch({
    withTimeout({
      func(...)
    }, timeout = timeout)
  }, TimeoutException = function(ex) {
    # If evaluation of the function reaches threshold, return 'Timeout!'
    return('Timeout!')
  }, error = function(e){
    # If evaluation of the function results in an error, return 'Code Failure!'
    return('Code Failure!')
  })
  # If evaluation of the function has no problems we return the evaluation
  return(val)
}

# Notes:

# Consider: "<<-" and " assign(" [FIXED = TRUE] or "^assign\\(" ======> CRASH
# Consider adding an option to allow or disallow the use of packages
# The point here is to be careful of students possibly introducing code with
# side effects that might add problems.

# Nokkvi Notes:

# Append results to file? CREATE CSV?


