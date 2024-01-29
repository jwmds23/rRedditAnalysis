source("constants.R")

# Get access token of Reddit API
get_token <- function(client_id=CLIENT_ID,client_secret=CLIENT_SECRET,user_agent=USER_AGENT){
  response <- POST("https://www.reddit.com/api/v1/access_token",
                   authenticate(client_id, client_secret),
                   user_agent(user_agent),
                   body = list(grant_type = "client_credentials"),
                   encode = "form")
  stop_for_status(response)
  content <- httr::content(response)
  content$access_token
}

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