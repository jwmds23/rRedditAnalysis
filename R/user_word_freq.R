library(httr)
library(jsonlite)
library(tidyverse)
library(tm)
library(wordcloud2)
library(udpipe)

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
  
  #handle inputs for string formats
  if(!is.character(username)) {
    print("Error: Username must be a string.")
    return(NULL)
  } else {
    username <- gsub(" ", "", username) # Remove all spaces
  }
  
  if(!is.character(content_type)) {
    print("Error: Content type must be a string.")
    return(NULL)
  } else {
    content_type <- tolower(content_type)
  }
  
  content <- NULL
  
  # handle potential errors during API call
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
    print(paste0("Error: ", e$message))
    NULL # Return NULL or appropriate error value
  })
  
  # Load the udpipe model
  ud_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  # Annotate the content using the udpipe model
  annotated_content <- udpipe_annotate(ud_model, x = content)
  annotated_content_df <- as.data.frame(annotated_content)
  
  # Filter the annotations to keep only nouns and adjectives
  topical_words_df <- subset(annotated_content_df, upos %in% c('NOUN','PROPN'))
  
  corpus <- Corpus(VectorSource(topical_words_df$lemma))
  
  # Preprocess the corpus:
  # remove punctuation, numbers, lowercase, strip whitespace, remove stopwords
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm)
  
  # Calculate word frequencies and generate the word cloud
  topical_word_freqs <- sort(rowSums(m), decreasing = TRUE)
  topical_word_freqs_df <- data.frame(word=names(topical_word_freqs), freq=topical_word_freqs)
  wordcloud2(topical_word_freqs_df)
}



