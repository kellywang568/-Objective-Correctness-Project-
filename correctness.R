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
    k[[i]] <- correctCheck_object(files[i], solution_file, 
                                  timeout = timeout, ...)
  }
  names(k) <- files
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
  x <- eval_file(student_file, timeout = timeout, ...)

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
correctCheck_object <- function(submission_file, solution_file, timeout = 10, ...){
  # Using wrapper function to get results of solution script
  inputs <- list(...)
  if (length(inputs) > 0){
    s <- lapply(inputs, 
                function(i) return(eval_file(solution_file, timeout,i )))
    x <- lapply(inputs, 
                function(i) return(eval_file(submission_file, timeout, i)))
    return(correctTests(x,s))
  } else {
    s <- eval_file(solution_file, timeout)
    x <- eval_file(submission_file, timeout)
    ae <- my_allEqual(x,s)
    return(list(score = as.numeric(ae == TRUE), comments = ae))
  }
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
eval_file <- function(filename, timeout = 10, ...) {
  # Create a function wrapper around a script
  mytxt <-  readLines(filename, warn = FALSE)
  mytxt <- gsub("set.seed\\(*\\d*\\)", "set.seed\\(1\\)", mytxt)
  mytxt <- gsub("install.packages*", "", mytxt)
  if (!is.function(eval(parse(text = mytxt)))){
    mytxt <- c("function() {", "set.seed(1)", mytxt,
             "return(ans) }")}
  # Set all set.seeds at set.seed(1)

  # Create a function
  myfunc <- eval(parse(text = mytxt))
  # Returns the results from the script
  inputs <- list(...)
  if (length(inputs) > 0){
    output <- sapply(inputs, 
                     function(i) return(timeoutCatch(myfunc, timeout, i)))
  } else {
    output <- timeoutCatch(myfunc, timeout)
  }
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


my_allEqual <- function(x,s) {
  if (identical(x, "Code Failure!")) {
    return("Code failure!")
  }
  else if (identical(x, "Timeout!")) {
    return("Timeout!")
  }
  else return(all.equal(x,s, check.attributes = FALSE))
}

correctTests <- function(x, s) {
  ae <- sapply(1:length(x) , function(i) return(my_allEqual(x[i], s[i])))
  return(list(score = as.numeric(ae == T), comments = ae))
}


