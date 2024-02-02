library(httr)
library(jsonlite)
library(tidyverse)
library(tm)
library(wordcloud2)
library(udpipe)

#' Title
#'
#' @param username 
#' @param content_type 
#'
#' @return
#' @export
#'
#' @examples
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
      output <- "Content type has to be either 'comments' or 'submitted'."
      stop()
    }
    
    if(is.null(content)) {
      message <- tolower(listing_df$message)
      if(message == "not found"){
        output <- paste0("Username '", username, "' ", message, ". Please enter a valid username.")
        stop()
      }
      
      if(message == "too many requests"){
        output <- message
        stop()
      }
    }
    # Return the content if all goes well
    content
  }, error = function(e) {
    #print(paste0("Error: ", e$message))
    print(output)
    return(NULL)
  })
  
  if(is.null(result)){
    return()
  }
  
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
  wordcloud_object <- wordcloud2(topical_word_freqs_df)
  return(wordcloud_object)
}



