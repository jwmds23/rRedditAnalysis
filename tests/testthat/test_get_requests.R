library(testthat)
library(httr)
source("R/get_requests.R")

test_that("get_search_threads_response returns a response object", {
  response <- get_search_threads_response("test")
  expect_is(response, "response")
  expect_true(response$status_code == 200, "Response status code is 200")
})