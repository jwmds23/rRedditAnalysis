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

}