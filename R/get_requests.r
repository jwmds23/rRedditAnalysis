source("constants.R")

library(httr)
library(jsonlite)
#' Get an access token for Reddit API authentication.
#'
#' This function retrieves an access token for authentication with the Reddit API
#'
#' @param client_id Reddit API client ID.
#' @param client_secret Reddit API client secret.
#' @param user_agent User agent string for API requests.
#' @return An access token for Reddit API authentication.
#'
#' @import httr jsonlite
#'
#' @examples
#' token <- get_token()
#' cat("Access token:", token, "\n")
#'
#' @export
get_token <- function(client_id = CLIENT_ID, client_secret = CLIENT_SECRET, user_agent = USER_AGENT) {
  response <- POST("https://www.reddit.com/api/v1/access_token",
    authenticate(client_id, client_secret),
    user_agent(user_agent),
    body = list(grant_type = "client_credentials"),
    encode = "form")
  stop_for_status(response)
  content <- httr::content(response)
  content$access_token
}

#' Get titles from a specified subreddit.
#'
#' This function retrieves titles from a specified subreddit.
#'
#' @param subreddit The name of the subreddit.
#' @return A response object containing subreddit titles.
#'
#' @import httr
#'
#' @examples
#' response <- get_subredit_titles("cats")
#' cat("Subreddit titles response:", response, "\n")
#'
#' @export
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

#' Search for subreddits based on a keyword.
#'
#' This function searches for subreddits on Reddit based on a specified keyword.
#'
#' @param keyword The keyword used for subreddit search.
#' @param limit The maximum number of results to retrieve (optional).
#' @return A response object containing subreddit search results.
#'
#' @import httr
#'
#' @examples
#' response <- get_search_subreddit("dogs")
#' cat("Subreddit search response:", response, "\n")
#'
#' @export
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
