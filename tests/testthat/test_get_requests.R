test_that("get_token returns a valid token string", {
  response <- get_token()
  expect_is(response, "character")
})


test_that("get_user_content_response returns a valid response object", {
  response <- get_user_content_response("Techno_superbowl", "comments")
  expect_is(response, "response")
  expect_true(response$status_code == 200, "Response status code is 200")
})


test_that("get_search_threads_response returns a response object", {
  response <- get_search_threads_response("test")
  expect_is(response, "response")
  expect_true(response$status_code == 200, "Response status code is 200")
})

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
