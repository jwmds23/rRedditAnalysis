source('get_request.r')

library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)


subreddit_highfreq_sentiment <- function(keyword){
    # select subreddit name
    subreddit <- select_subreddit(keyword)
    # request for titles
    response <- request_titles(subreddit)
    # word segment
    data <- fromJSON(rawToChar(response$content), flatten = TRUE)
    titles <- data$data$children[10]
    
    text_data <- data.frame(titles)
    colnames(text_data) <- "titles"
    tokens <- text_data %>%
    unnest_tokens(word, titles)
    data_clean <- tokens %>%
    anti_join(stop_words)
    word_counts <- data_clean %>%
    count(word, sort = TRUE)

    # select high frequency data
    word_counts_pic <- word_counts %>%
    arrange(desc(n)) %>%
    head(20)

    # Get sentiment lexicon
    bing_lexicon <- get_sentiments("bing")

    # Join with sentiment lexicon
    word_sentiment <- word_counts %>%
    inner_join(bing_lexicon, by = c("word" = "word"))
    word_sentiment$sentiment <- factor(word_sentiment$sentiment, levels = c("positive", "negative"))

    # Plot bar graph
    ggplot(word_sentiment, aes(x = reorder(word, n), y = n, fill = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Word Frequency by Sentiment", x = "Words", y = "Frequency") +
    theme_minimal() +
    scale_fill_gradient(low = "lightblue", high = "pink") +
    facet_wrap(~ sentiment, scales = "free_y")
}