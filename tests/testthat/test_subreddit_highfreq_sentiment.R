test_that("subreddit_highfreq_sentiment function should return a bar graph", {
    test_keywords <- c("cat", "book")
    for (keyword in test_keywords) {
        result <- subreddit_highfreq_sentiment(keyword,1)
        expect_s3_class(result, "gg")
    }
})