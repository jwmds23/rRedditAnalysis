# Load the required packages
library(testthat)
library(httr)
library(jsonlite)
source("../../R/select_subreddit.R")



# original_readline <- readline

# Redefine the readline function to return a predefined value
# readline <- function(prompt) {
#   return(2)
# }

test_that("select_subreddit function should return a subreddit name", {
    result <- select_subreddit("cat",1)
    expect_is(result, "character")
    expect_equal(result, "cats")
})

# Restore the original readline function
# readline <- original_readline