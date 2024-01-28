library(httr)
library(jsonlite)

## configure parameters for connecting the Reddit API
client_id <- "JL7d1_eeiz7zeNLDwgEF5A"
client_secret <- "XUJEFa86M5-wWMXkhHRNFOhMODv0UQ"
user_agent <- "R:reddit_script:v1.0 (by /u/appleontree1990)"

response <- POST("https://www.reddit.com/api/v1/access_token",
                 authenticate(client_id, client_secret),
                 user_agent(user_agent),
                 body = list(grant_type = "client_credentials"),
                 encode = "form")

stop_for_status(response)
content <- httr::content(response)
access_token <- content$access_token

get_threads_summary <- function(keyword){
  
# Define the API endpoint URL
params <- list(
  sort = 'relevance',
  t = 'all',
  type ='link',
  q = keyword, # any keyword
  limit = 100,
  Authorization = access_token)

# Send the GET request
url <- paste0("https://www.reddit.com/search.json?q=",params$q)
response <- GET(url, query = params)
# Check the status of the response
print(status_code(response))

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

}