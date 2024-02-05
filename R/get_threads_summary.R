#' Summarize Reddit Thread Search Results
#'
#' This function retrieves and summarizes search results of Reddit threads by a specific keyword.
#'
#' @param keyword A character string representing the keyword to search for on Reddit.
#'
#' @return A list containing three plots:
#'   \describe{
#'     \item{upvote_ratio_plot}{A histogram showing the distribution of upvote ratios.}
#'     \item{top_subreddits_plot}{A bar plot displaying the top subreddits by number of threads.}
#'     \item{top_nouns_plot}{A bar plot showing the top 20 frequent nouns in thread titles and texts.}
#'   }
#'
#' @import httr
#' @import jsonlite
#' @import text
#' @import udpipe
#' @import ggplot2
#' @import tidyverse
#'
#' @references The function uses Reddit's API for data retrieval.
#' @references The function utilizes the udpipe R package for natural language processing.
#'
#' @usage get_threads_summary(keyword)
#' @examples
#' \dontrun{
#'   get_threads_summary("Mazzy Star")
#'   get_threads_summary("UBCO")
#' }
#' @export


get_threads_summary <- function(keyword){
  # Check the format of argument `keyword`
  if (!is.character(keyword)) {
    tryCatch(
      {
        keyword <- as.character(keyword)
      },
      error = function(e) {
        message("Failed to cast the argument to a string.")
        return(NULL)
      }
    )
  }
  if (!is.null(keyword)) {
    
    response <- get_search_threads_response(keyword)
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
            labs(title = paste0("Distribution of Upvote Ratio for keyword `",keyword,"`"), x = "Upvote Ratio", y = "Frequency") +
            theme_minimal()
          
          
          
          # visualization 2: Top Subreddits by Number of Threads
          subreddit_counts <- result_df %>%
            group_by(subreddit) %>%
            summarise(post_count = n()) %>%
            arrange(desc(post_count))  # Arrange in descending order
          
          top_15_subreddits <- head(subreddit_counts, 15)
          
          #Create a bar plot for top 15 Subreddits
          top_subreddits_plot <- ggplot(top_15_subreddits, aes(x = post_count, y = reorder(subreddit, post_count))) +
            geom_bar(stat = "identity", fill = "darkgrey", color = "black") +
            labs(title = paste0("Top Subreddits by Number of Threads for keyword `",keyword,"`"), x = "Number of Threads", y = "Subreddit") +
            theme_minimal()
          
          
          
          # visualization 3: Top Frequent Nouns
          df <- data.frame(text = paste(result_df$title, result_df$text, sep = " "))
          
          # Load the English model for parts-of-speech tagging
          udpipe_model_path <- system.file("data", "english-ewt-ud-2.5-191206.udpipe", package = "rRedditAnalysis")
          ud_model <- udpipe_load_model(udpipe_model_path)
          
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
            labs(title = paste0("Top 20 Frequent Nouns for keyword `",keyword,"`"), x = "Noun", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          
          # Return all three plots above in a list
          return(list(plot1 = upvote_ratio_plot, plot2 = top_subreddits_plot, plot3 = top_nouns_plot))
          }
    else{
      return("Request failed")
    }
  }
  else {
    return("Please enter a valid keyword")
  }
}