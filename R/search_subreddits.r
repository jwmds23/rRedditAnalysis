library(httr)
library(jsonlite)

access_token <- get_token()

search_subreddits <- function(keyword, limit = 5) {
  url <- paste0("https://www.reddit.com/subreddits/search.json?q=", keyword, "&limit=", limit)
  headers <- add_headers(Authorization = paste("Bearer", access_token),
                         `User-Agent` = USER_AGENT)
  
  response <- GET(url, headers = headers)
  if (status_code(response) != 200) {
    stop("API request unsuccessful. Status code: ", status_code(response))
  }
  
  content <- fromJSON(rawToChar(response$content), flatten = TRUE)
  subreddits <- content$data$children['data.display_name']
  
  return(subreddits)
}