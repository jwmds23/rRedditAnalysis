get_requests <- function(subreddit){
  url <- paste0("https://www.reddit.com/r/", subreddit, "/hot.json")
  params <- list(
    g = "GLOBAL",
    limit = 10,
    q = as.character(subreddit_name),
    limit = 100,
    Authorization = access_token)
  response <- GET(url, query = params)

  if (status_code(response) != 200) {
    stop("API request unsuccessful")
  }
  data <- fromJSON(rawToChar(response$content), flatten = TRUE)
  title <- data$data$children[10]
  return(title)
}
