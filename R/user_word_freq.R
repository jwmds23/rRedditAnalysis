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
    
    
    content <- httr::content(response)
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
    username = as.character(username),  
    count = 25,
    limit = 100,
    sr_detail = TRUE,
    Authorization = access_token
  )
  url <- paste0(DOMAIN_REDDIT, "user/", username, "/", content_type,".json")
  return(GET(url, query = params))
}


user_word_freq <- function(username, content_type){
  
  # Validate username as a string and strip spaces
  if(!is.character(username)) {
    print("Error: Username must be a string.")
    return(NULL)
  } else {
    username <- gsub(" ", "", username) # Remove all spaces
  }
  
  # Validate content_type as a string and make it lowercase
  if(!is.character(content_type)) {
    print("Error: Content type must be a string.")
    return(NULL)
  } else {
    content_type <- tolower(content_type)
  }
  
  # Initialize content to NULL
  content <- NULL
  
  # Use tryCatch to handle potential errors during API call
  result <- tryCatch({
    listing <- get_user_content_response(username, content_type)
    listing_df <- httr::content(listing, as = "text") %>% jsonlite::fromJSON(flatten = TRUE)
    
    if(content_type == "comments"){
      content <- listing_df[2]$data$children$data.body
    }else if(content_type == "submitted"){
      content <- listing_df[2]$data$children$data.selftext
    } else {
      stop("Content type has to be either 'comments' or 'submitted'.")
    }
    
    if(is.null(content)) {
      message <- tolower(listing_df$message)
      stop(paste0("Username '", username, "' ", message, ". Please enter a valid username."))
    }
    
    # Return the content if all goes well
    content
  }, error = function(e) {
    # Handle errors
    print(paste0("Error: ", e$message))
    NULL # Return NULL or appropriate error value
  })
  # Create a corpus from the content
  corpus <- Corpus(VectorSource(content))
  
  # Preprocess the corpus: remove punctuation, numbers, lowercase, strip whitespace, remove stopwords
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  # Create a document-term matrix
  dtm <- TermDocumentMatrix(corpus)
  
  # Convert the document-term matrix to a matrix
  m <- as.matrix(dtm)
  
  # Calculate word frequencies
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  # Make a data frame for the word cloud
  word_freqs_df <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  # Generate the word cloud
  wordcloud2(word_freqs_df)
}
  
  

