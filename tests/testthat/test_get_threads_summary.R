# Load the required packages
library(testthat)
library(httr)
library(jsonlite)

# Load the function
source("get_requests.R")

# Define the tests
test_that("get_threads_summary function works as expected", {
  # Define test cases
  test_keywords <- c("Mazzy Star", "UBCO")
  
  # Loop through test cases
  for (keyword in test_keywords) {
    # Call the function
    result <- get_threads_summary(keyword)
    
    # Check if the result is a list
    expect_is(result, "list")
    
    # Check if the result contains three elements
    expect_length(result, 3)
    
    # Check if the elements of the result are ggplot objects
    expect_s3_class(result$plot1, "gg")
    expect_s3_class(result$plot2, "gg")
    expect_s3_class(result$plot3, "gg")
  }
})
