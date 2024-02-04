# Load the required packages
library(testthat)
library(httr)
library(jsonlite)

test_that("select_subreddit function should return a subreddit name", {
    result <- select_subreddit("cat",1)
    expect_is(result, "character")
    expect_equal(result, "cats")
})
