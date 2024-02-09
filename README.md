# rRedditAnalysis
[![Build Status](https://app.travis-ci.com/jwmds23/rRedditAnalysis.svg?branch=main)](https://travis-ci.org/jwmds23/rRedditAnalysis)


## Introduction

`rRedditAnalysis` is an R package with API Wrapper functionality. This package provides tools for Reddit data analysis and visualization, focusing on retrieving user-specific content, threads summary, and sentiment analysis. It can be applied social media analytics and natural language processing.

## Installation

This package can be installed remotely from the github by using the `remotes` package:

```{r, eval=FALSE}
# Install the remotes Package (Skip this step if you have installed it before)
install.packages("remotes")
```

```{r, eval=FALSE}
# Install the Package from GitHub
remotes::install_github("jwmds23/rRedditAnalysis",upgrade = FALSE)
```

```{r}
# Load and Use the Package
library(rRedditAnalysis)
```

## Usage

### User Content Analysis

You can conduct user content analysis using `user_word_freq(username, content_type)`.

To call this function, you should input a valid Reddit username and select a content type out of two options: 'comments' and 'submitted' to retrieve the user's content through reddit API.

-   For option 'comments', the function will retrieve the most recent 100 comments of the specified user.

-   For option 'submitted', the function will retrieve the most recent 100 threads submitted from the specified user.

After API request, it will analyse the requested content and plot a Word Cloud for visualization.

Please refer the below examples.

```{r word-freq, warning=FALSE}
# Assuming `threads` is a data frame containing Reddit threads
user_word_freq("Techno_superbowl", "comments")
user_word_freq("Techno_superbowl", "submitted")
```

### Extracting Thread Summaries

You can extract threads visualization summary of any topic using `get_threads_summary(keyword)`.

To call this function, you should input a keyword that you would like to analyze in the format of character(put in quotes) to retrieve top 100 related threads through reddit API.

After API request, it will analyze the requested search results and plot three charts:

-   upvote_ratio_plot: A histogram showing the distribution of upvote ratios

-   top_subreddits_plot: A bar plot displaying the top subreddits by number of threads

-   top_nouns_plot: A bar plot showing the top 20 frequent nouns in thread titles and texts

Please refer the below examples.

```{r get-threads-summary, warning=FALSE}
threads <- get_threads_summary(keyword = "Nirvana")
threads$plot1
threads$plot2
threads$plot3
```

### Sentiment Analysis

You can conduct sentiment analysis on the high-frequency terms of a subreddit using the function `subreddit_highfreq_sentiment()`.

To use this function, begin by inputting a keyword to identify the subreddit you're interested in. Based on relevance and subscriber count, a list of 5 subreddits will be displayed.

From this list, select a subreddit by choosing an index number between 1 and 5. Following your selection, a bar chart showing the high-frequency words and their associated sentiments will be presented.

please refer the below example:

```{r sentiment-analysis, warning=FALSE}
# Perform sentiment analysis on a sample subreddit, please delete "test = 1" in application
subreddit_highfreq_sentiment(keyword = "UBC",test=1)

```

## Note

Please do not frequently call each function within one minute, otherwise Reddit API will prevent the user from requesting. If you see error messages like "too many requests" or "request unsuccessful", please wait for at least one minute and try it again.

## Further Details

For more detailed information on each function and its parameters, refer to the function help pages in R.

```{r get-help, eval=FALSE}
# Access help pages for the specific functions
?get_threads_summary
?user_word_freq
?subreddit_highfreq_sentiment
```

