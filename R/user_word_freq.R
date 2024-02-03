source("R/get_requests.R")
#' Generate a Word Cloud from User Content on Reddit
#'
#' This function fetches content from Reddit based on the specified username and content type, 
#' preprocesses the text to filter nouns and adjectives, and generates a word cloud visualization.
#'
#' @param username A character string specifying the Reddit username to fetch content for.
#' @param content_type A character string specifying the type of content to fetch from the user's Reddit profile. 
#'                     Valid options are "comments" or "submitted" for user comments or submitted posts, respectively.
#'
#' @return An `htmlwidget` object that displays a word cloud visualization of the most frequent nouns and 
#'         adjectives found in the user's content. Returns `NULL` if there are errors in fetching or processing the content.
#' @import httr
#' @import jsonlite
#' @import tidyverse
#' @import tm
#' @import wordcloud2
#' @import udpipe
#' 
#' @examples
#' \dontrun{
#'  user_word_freq("Techno_superbowl", "comments")
#' }
#' 
#'  @export
#'


user_word_freq <- function(username, content_type){
  #initialize an output variable for error message storation
  output <- NULL
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
  ud_model <- udpipe_load_model("R/english-ewt-ud-2.5-191206.udpipe")
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



