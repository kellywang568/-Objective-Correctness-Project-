library(R.utils)
# Very simple, incomplete version.
correctTests <- function(x, s){
  # need to translate this into a numeric score
  # But I think saving the comments somehow is helpful?
  ae <- (all.equal(x, s))
  return(list(score = as.numeric(isTRUE(ae)), comments = ae))
  
}

# Thought: If correctCheck_object passes all the timeout/error requirements, 
# then have it return the results from a new function (correctTests), that way 
# we can save the all.equal comments 

t <- correctCheck_directory('662/feedback/ExampleSubmissions-1/', 
                            '662/feedback/Example1_Solution1.R', timeout = 3)

feedbacks <- lapply(t, function(x) return(x['correctCheck']))
table(unlist(feedbacks))
