# Load the required packages
library(testthat)
library(httr)
library(jsonlite)
source("R/get_requests.R")

test_that("get_requests function should return response abhout specific subreddits", {
    response <- get_subredit_titles("books")
    expect_is(response, "response")
    expect_true(response$status_code == 200, "Response status code is 200")
})

test_that("get_requests function should return response about related subreddit names", {
    response <- get_search_subreddit("books")
    expect_is(response, "response")
    expect_true(response$status_code == 200, "Response status code is 200")
})