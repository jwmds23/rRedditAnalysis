library(httr)
library(jsonlite)
library(here)

#' Select and display the top 5 subreddits related to a keyword.
#'
#' This function performs a search for subreddits related to a given keyword and
#' displays the top 5 subreddits with the highest number of subscribers, then 
#' prompts the user to select one of them.
#'
#' param (keyword) : The keyword used for subreddit search.
#' param (test) : A flag for unit test dealing with user input.
#'
#' return: The name of the selected subreddit based on user input.
#'
#' examples:
#' subreddit <- select_subreddit("cats")

select_subreddit <- function(keyword, test=0) {
  response <- get_search_subreddit(keyword)
  # handle the content to find 5 most related reddits
  content <- tryCatch({
    fromJSON(rawToChar(response$content), flatten = TRUE)
  }, error = function(e) {
    cat("Error in parsing JSON: ", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(content)) return(NULL)
  # Get subreddit name and subscribers number
  subreddits <- content$data$children[['data.display_name']]
  subscribers <- content$data$children[['data.subscribers']]
  
  subreddit_info <- data.frame(name = subreddits, subscribers = subscribers)
  names(subreddit_info) <- c("name", "subscribers")
  
  top_5_subreddits <- head(subreddit_info, 5)
  sorted_subreddits <- top_5_subreddits[order(-top_5_subreddits$subscribers), ]
  
  # Display the name and subscribers in descending order
  cat("Top 5 subreddits:\n")
  flush.console()
  for (i in 1:nrow(sorted_subreddits)) {
    cat(sprintf("%d: %s (%d subscribers)\n", i, sorted_subreddits$name[i], sorted_subreddits$subscribers[i]))
  }
  flush.console()
  
  # Prompt the user to enter the index number of the subreddit
  cat("Please enter the index number (1-5) of the subreddit you're interested in: ")
  flush.console()
  if (test == 0){
    index <- as.integer(readline(prompt = "Please enter a number: "))
  }
  else {
     index <- 2
  }  

  # Validate the input
  tryCatch({
    if (!is.numeric(index) || is.na(index) || index < 1 || index > 5) {
      selected_subreddit <- NA
      stop("Invalid index. Please enter a valid index number between 1 and 5.\n")
    } else {
      selected_subreddit <- sorted_subreddits$name[index]
      cat(sprintf("You selected: %s\n", selected_subreddit))
    }
  }, error = function(e) {
    cat(e$message)
  })
  
  return(invisible(selected_subreddit))
}
