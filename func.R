save_images_on_deck<- function() {
  plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
  plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
  file.copy(from=plots.png.paths, to="plots")
  
}

remove_links <- content_transformer(function (x) rm_url(x))

clean_corpus <- function(corpus) {
  
  # Remove url
  corpus <- tm_map(corpus, remove_links)
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "a", "the"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # remove Numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Replace number
  # corpus <- tm_map(corpus, content_transformer(replace_number))
  # # Replace abbreviation
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  # Replace contradiction
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  # Replace symbol
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  # Replace brackets
  corpus <- tm_map(corpus, content_transformer(bracketX))
  # remove more words
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "covid", "sarscov", "among", "persons"))
  return(corpus)
}


clean_v_corpus <- function(x) {
  clean_v <- tolower(x)
  clean_v <- removeWords(clean_v, stopwords("en"))
  clean_v <-removePunctuation(clean_v)
  clean_v <-removeNumbers(clean_v)
  clean_v <-stripWhitespace(clean_v)
  clean_v <-bracketX(clean_v)
  # Replace numbers with words
  clean_v <-replace_number(clean_v)
  clean_v <-replace_abbreviation(clean_v)
  
  clean_v <-replace_symbol(clean_v)
  new_stops <- c("covid", "sarscov", "among", "persons", stopwords("en"))
  clean_v <- removeWords(clean_v, new_stops)
  return(clean_v)
}