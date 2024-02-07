library(httr)
library(jsonlite)
library(here)

# Request to get token
get_token <- function(client_id = CLIENT_ID, client_secret = CLIENT_SECRET, user_agent = USER_AGENT, redirect_uri = REDIRECT_URI) {
  result <- tryCatch({
    # Make the POST request
    response <- POST("https://www.reddit.com/api/v1/access_token",
                     authenticate(client_id, client_secret),
                     user_agent(user_agent),
                     body = list(grant_type = "client_credentials", redirect_uri = redirect_uri),
                     encode = "form")
    
    # Check the response status
    stop_for_status(response)
    
    
    content <- httr::content(response)
    content$access_token
  }, error = function(e) {
    
    cat("An error has occurred: ", e$message, "\n")
    NULL # Return NULL or an appropriate value indicating failure
  })
  return(result)
}

# Request to get user content
get_user_content_response <-function(username, content_type){
  access_token <- get_token()
  # Define the API endpoint URL
  params <- list(
    context = 2,
    show = 'given',
    sort = 'new',
    t = 'all',
    type = as.character(content_type),
    username = as.character(username),  
    count = 25,
    limit = 100,
    sr_detail = TRUE,
    Authorization = access_token
  )
  url <- paste0(DOMAIN_URL, "/user/", username, "/", content_type,".json")
  return(GET(url, query = params))
}


# Request to get reddit search result
get_search_threads_response <- function(keyword){
  access_token <- get_token()
  # Define the API endpoint URL
  params <- list(
    sort = 'relevance',
    t = 'all',
    type ='link',
    q = as.character(keyword),
    limit = 100,
    Authorization = access_token)
  # Send the GET request
  url <- paste0("https://www.reddit.com/search.json?q=",params$q)
  response <- GET(url, query = params)
  response
}


# Request to get titles from a specified subreddit.
get_subredit_titles <- function(subreddit){
  access_token <- get_token()
  url <- paste0("https://www.reddit.com/r/", subreddit, "/hot.json")
  params <- list(
    g = "GLOBAL",
    limit = 10,
    q = as.character(subreddit),
    limit = 100,
    Authorization = access_token)
  response <- GET(url, query = params)

  if (status_code(response) != 200) {
    stop("API request unsuccessful")
  }
  response
}

# Request to search for subreddits based on a keyword.
get_search_subreddit <- function(keyword){
  access_token <- get_token()
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
  response
}