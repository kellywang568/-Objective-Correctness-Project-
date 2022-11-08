library(R.utils)
source('correctness.R')

# Example 1: Return a variable ans
t1 <- correctCheck_directory('662/feedback/ExampleSubmissions-1/', 
                            '662/feedback/Example1_Solution1.R', timeout = 3)

scores <- lapply(t1, function(x) return(x['score']))

feedbacks <- lapply(t1, function(x) return(x['comments']))
table(unlist(feedbacks))
table(unlist(scores))

######################################################################
# Example 2: Write a function, use test cases

# Toy dataframe for testing
df <- data.frame(letters = c('A', 'B', NA, 'X', NA),
                 numbers = c(NA, 31, 14, NA, 89))

t2 <- correctCheck_directory('662/example2_1', 
                            '662/example2_1/spearsabby_168704_6630827_Question2.R', 
                            timeout = 3, iris, df)


