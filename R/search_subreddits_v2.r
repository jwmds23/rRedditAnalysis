search_subreddits <- function(keyword, limit = 100) {
  # Construct the request URL
  url <- paste0("https://www.reddit.com/subreddits/search.json?q=", keyword, "&limit=", limit)
  
  # Make request
  headers <- add_headers(Authorization = paste("Bearer", access_token),
                         `User-Agent` = USER_AGENT)
  response <- tryCatch({
    GET(url, headers = headers)
  }, error = function(e) {
    cat("Error in GET request: ", e$message, "\n")
    return(NULL) # Return NULL to prevent further execution
  })
  
  # If the GET request failed, stop the function execution
  if (is.null(response)) return(NULL)
  
  if (status_code(response) != 200) {
    stop("API request unsuccessful. Status code: ", status_code(response))
  }
  
  # handle the content to find 5 most related reddits
  content <- tryCatch({
    fromJSON(rawToChar(response$content), flatten = TRUE)
  }, error = function(e) {
    cat("Error in parsing JSON: ", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(content)) return(NULL)
  
  subreddits <- content$data$children[['data.display_name']]
  subscribers <- content$data$children[['data.subscribers']]
  
  subreddit_info <- data.frame(name = subreddits, subscribers = subscribers)
  names(subreddit_info) <- c("name", "subscribers")
  
  top_5_subreddits <- head(subreddit_info, 5)
  sorted_subreddits <- top_5_subreddits[order(-top_5_subreddits$subscribers), ]
  
  # Display the name and subscribers in descending order
  cat("Top 5 subreddits:\n")
  for (i in 1:nrow(sorted_subreddits)) {
    cat(sprintf("%d: %s (%d subscribers)\n", i, sorted_subreddits$name[i], sorted_subreddits$subscribers[i]))
  }
  
  # Prompt the user to enter the index number of the subreddit
  cat("Please enter the index number (1-5) of the subreddit you're interested in: ")
  index <- as.integer(readline(prompt = "Please enter a number: "))
  
  # Validate the input
  tryCatch({
    if (is.na(index) || index < 1 || index > 5) {
      stop("Invalid index. Please enter a valid index number between 1 and 5.\n")
    } else {
      selected_subreddit <- sorted_subreddits$name[index]
      cat(sprintf("You selected: %s\n", selected_subreddit))
    }
  }, error = function(e) {
    cat(e$message)
  })
  
  return(selected_subreddit)
}
