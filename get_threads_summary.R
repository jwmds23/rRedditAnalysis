library(httr)
library(jsonlite)
library(text)
library(udpipe)

## configure parameters for connecting the Reddit API
client_id <- "JL7d1_eeiz7zeNLDwgEF5A"
client_secret <- "XUJEFa86M5-wWMXkhHRNFOhMODv0UQ"
user_agent <- "R:reddit_script:v1.0 (by /u/appleontree1990)"

response <- POST("https://www.reddit.com/api/v1/access_token",
                 authenticate(client_id, client_secret),
                 user_agent(user_agent),
                 body = list(grant_type = "client_credentials"),
                 encode = "form")

stop_for_status(response)
content <- httr::content(response)
access_token <- content$access_token

get_threads_summary <- function(keyword){
  
# Define the API endpoint URL
params <- list(
  sort = 'relevance',
  t = 'all',
  type ='link',
  q = keyword, # any keyword
  limit = 100,
  Authorization = access_token)

# Send the GET request
url <- paste0("https://www.reddit.com/search.json?q=",params$q)
response <- GET(url, query = params)

# Check the status of the response
if(status_code(response)=='200'){
  
      # Parse JSON content
      json_content <- httr::content(response, "text", encoding = "UTF-8")
      
      # Parse JSON into a data frame
      parsed_data <- fromJSON(json_content)
      
      # Accessing posts
      posts <- parsed_data$data$children
      
      # Extracting content from each post
      result_df <- data.frame(title = character(), text = character(),
                              subreddit = character(), upvote_ratio = numeric(),
                              created_utc = numeric(), num_comments = numeric(), 
                              ups = numeric())
      for (i in 1:100) {
        title <- posts[i,2]$title
        text <- posts[i,2]$selftext
        subreddit <- posts[i,2]$subreddit
        upvote_ratio <- posts[i,2]$upvote_ratio
        created_utc <- posts[i,2]$created_utc
        num_comments <- posts[i,2]$num_comments
        ups <- posts[i,2]$ups
        # Create a data frame for the current post
        df <- data.frame(
          title = title,
          text = text,
          subreddit = subreddit,
          upvote_ratio = upvote_ratio,
          created_utc = created_utc,
          num_comments = num_comments,
          ups = ups
        )
        
        # Append the current post data frame to the result data frame
        result_df <- rbind(result_df, df)
      }
      
      
      # visualization 1: Distribution of Upvote Ratio
      upvote_ratio_plot <- ggplot(result_df, aes(x = upvote_ratio)) +
        geom_histogram(binwidth = 0.1, fill = "darkgreen", color = "black", alpha = 0.7) +
        labs(title = "Distribution of Upvote Ratio", x = "Upvote Ratio", y = "Frequency") +
        theme_minimal()
      
      
      
      # visualization 2: Top Subreddits by Number of Threads
      subreddit_counts <- result_df %>%
        group_by(subreddit) %>%
        summarise(post_count = n()) %>%
        arrange(desc(post_count))  # Arrange in descending order
      
      top_15_subreddits <- head(subreddit_counts, 15)
      
      #Create a bar plot for top 15 Subreddits
      top_subreddits_plot <- ggplot(top_15_subreddits, aes(x = post_count, y = fct_reorder(subreddit, post_count))) +
        geom_bar(stat = "identity", fill = "darkgrey", color = "black") +
        labs(title = "Top Subreddits by Number of Threads", x = "Number of Threads", y = "Subreddit") +
        theme_minimal()
      
      
      
      # visualization 3: Top Frequent Nouns
      df <- data.frame(text = paste(result_df$title, result_df$text, sep = " "))
      
      # Load the English model for parts-of-speech tagging
      ud_model <- udpipe_download_model(language = "english", model_dir = getwd())
      ud_model <- udpipe_load_model(ud_model$file_model)
      
      # Tokenize and annotate the text with parts-of-speech
      annotated_text <- udpipe_annotate(ud_model, x = df$text)
      
      # Extract relevant information from the annotated text
      annotations <- as.data.frame(annotated_text)
      
      # Convert lemma column to lowercase
      annotations$lemma <- tolower(annotations$lemma)
      
      # Keep only the nouns
      nouns_df <- subset(annotations, upos == "NOUN")
      
      # Create a word frequency plot for the top 20 nouns
      top_nouns_plot <- ggplot(head(count(nouns_df, lemma, sort = TRUE), 20), aes(x = reorder(lemma, -n), y = n)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = paste0("Top 20 Frequent Nouns for keyword `",params$q,"`"), x = "Noun", y = "Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      # Return all three plots above in a list
      return(list(plot1 = upvote_ratio_plot, plot2 = top_subreddits_plot, plot3 = top_nouns_plot))
      }
else{
  return("Request failed")
}

}