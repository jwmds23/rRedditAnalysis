library(httr)
library(jsonlite)
library(here)

source(here("R", "constants.r"))

#' Get titles from a specified subreddit.
#'
#' This function retrieves hot posts data from specific subreddit.
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
