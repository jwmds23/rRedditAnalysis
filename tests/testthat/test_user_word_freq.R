library(testthat)

test_that("user_word_freq returns an htmlwidget", {
  wordcloud_object <- user_word_freq("Techno_superbowl", "comments")
  expect_true(inherits(wordcloud_object, "htmlwidget"))
})

test_that("Error on invalid content type", {
  expect_output(user_word_freq("Techno_superbowl", "likes"), "Content type has to be either 'comments' or 'submitted'.")
})

test_that("Error on invalid username", {
  expect_output(user_word_freq("kfasdjhfskdsdf", "comments"), paste0("Username 'kfasdjhfskdsdf' not found. Please enter a valid username."))
})
