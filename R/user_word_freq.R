library(httr)
library(jsonlite)
library(tidyverse)
library(tm)
library(wordcloud2)
CLIENT_ID <- "JL7d1_eeiz7zeNLDwgEF5A"
CLIENT_SECRET <- "XUJEFa86M5-wWMXkhHRNFOhMODv0UQ"
USER_AGENT <- "R:reddit_script:v1.0 (by /u/appleontree1990)"
DOMAIN_REDDIT <- "https://www.reddit.com/"

get_token <- function(client_id = CLIENT_ID, client_secret = CLIENT_SECRET, user_agent = USER_AGENT) {
  result <- tryCatch({
    # Make the POST request
    response <- POST("https://www.reddit.com/api/v1/access_token",
                     authenticate(client_id, client_secret),
                     user_agent(user_agent),
                     body = list(grant_type = "client_credentials"),
                     encode = "form")
    
    # Check the response status
    stop_for_status(response)
    
    
    content <- httr::content(response)w
    content$access_token
  }, error = function(e) {
    
    cat("An error has occurred: ", e$message, "\n")
    NULL # Return NULL or an appropriate value indicating failure
  })
  return(result)
}

# Send the GET request
get_user_content_response <-function(username, content_type){
  access_token <- get_token()
  
  # Define the API endpoint URL
  params <- list(
    context = 2,
    show = 'given',
    sort = 'new',
    t = 'all',
    type = as.character(content_type),
    username = as.character(username),  # Replace with the actual username
    count = 25,
    limit = 100,
    sr_detail = TRUE,
    Authorization = access_token
  )
  url <- paste0(DOMAIN_REDDIT, "user/", username, "/", content_type,".json")
  return(GET(url, query = params))
}


user_word_freq <- function(username, content_type){
  response = get_user_content_response(username, content_type)
  content_df <- httr::content(response, as = "text") %>% jsonlite::fromJSON(flatten = TRUE)
  content_df[2]$data$children$data.body
  vector <- content_df[2]$data$children$data.body
  print(vector)
  
  # Create a corpus from the vector
  #corpus <- Corpus(VectorSource(vector))
  
  # Preprocess the corpus: remove punctuation, numbers, lowercase, strip whitespace, remove stopwords
  #corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  # Create a document-term matrix
  #dtm <- TermDocumentMatrix(corpus)
  
  # Convert the document-term matrix to a matrix
  #m <- as.matrix(dtm)
  
  # Calculate word frequencies
  #word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  # Make a data frame for the word cloud
  #df_word_freqs <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  # Generate the word cloud
  #wordcloud2(df_word_freqs)
}
  
  

