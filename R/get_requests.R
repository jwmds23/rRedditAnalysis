source("R/constants.R")

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