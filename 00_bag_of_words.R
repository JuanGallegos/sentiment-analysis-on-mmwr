library(tidyverse)
# tm is a tex mining package
library(tm)
library(tidyverse)
library(syuzhet)
library(plotly)
library(tm)
library(qdap)
library(qdapRegex)
library(wordcloud)
library(viridisLite)
library(plotrix)

source("func.R")


# Read scraped data ------------------------------------------------------------
scraped <- read_csv("article_info.csv")
scraped

save_images_on_deck<- function() {
  plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
  plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
  file.copy(from=plots.png.paths, to="plots")
  
}

# Creating Corpus using vector -------------------------------------------------
article_implications <- scraped$implications
# Make a vector source from article_implications
implications_source <- VectorSource(article_implications)
# Call the VCorpus() function on the implications_source object to create implications_corpus
implications_corpus <- VCorpus(implications_source)

# Print out implications_corpus
print(implications_corpus)

# Print the 15th tweet in implications_corpus
implications_corpus[[15]]
# Print the contents of the 15th tweet in implications_corpus
content(implications_corpus[[15]])

# Now use content to review the plain text of the 10th tweet
content(implications_corpus[[10]])

# Creating Corpus using Dataframe ----------------------------------------------
# You need to rename a column in dataframe to text
rm(df_source)
colnames(scraped) <- c("doc_id", "art_title","what_is_known","what_is_added","text","publication_date")
# Create a DataframeSource from scraped after labeling doc_id and text of interest
df_source <- DataframeSource(scraped)
class(df_source)
# Convert df_source to a volatile corpus
df_corpus <- VCorpus(df_source)
# Examine df_corups
df_corpus
class(df_corpus)

# Pre-process Text -------------------------------------------------------------
# Prior to this was the raw stage, the following will clean and preprocess text
library(qdap)
# Quantitative Discourse Analysis Package is an R package for computer assisted
# qualitative data analysis, particularly quantitative discourse analysis,
# transcript analysis and natural language processing.

# Package to remove links
library(qdapRegex)

# Apply preprocessing steps to a corpus
## Alter the function code to match the instructions
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

# Apply your customized function to the df_corpus: clean_corp
clean_corp <- clean_corpus(df_corpus)

# Print out a cleaned up article
content(clean_corp[[1]])

# Print out the same tweet in the original form
content(df_corpus[[1]])

# With your cleaned corpus, you need to change the data structure for analysis.
# Generate TDM
implications_tdm <- TermDocumentMatrix(clean_corp)
# Generate DTM
implications_dtm <- DocumentTermMatrix(clean_corp)

# Print the subset of coffee_m containing documents (rows) 25 through 35 and 
# terms (columns) "eligible" and "vaccinated".
implications_m <- as.matrix(implications_dtm)
implications_dtm[1:5, c("eligible", "vaccine")]
implications_m[1:5, c("eligible", "vaccine")]

# Word Frequency Matrix
# is less popular and can be made from a term document matrix
implications_wfm <- wfm(scraped$text)
implications_wfm

# Common Text Mining Visuals ---------------------------------------------------
## Term frequency plots with tm ------------------------------------------------
# To make a frequency plot with the tm package, you change your TDM into a Matrix
# Convert TDM to matrix
implications_m <- as.matrix(implications_tdm)

# Sum rows and sort by frequency
term_frequency <- rowSums(implications_m) 
term_frequency <- sort(term_frequency, decreasing = TRUE)
# Create a barplot
# dev.new()
barplot(term_frequency[1:10], col = "blue", las = 2)

## Term frequency plots with qdap ----------------------------------------------
scraped <- read_csv("article_info.csv")

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

clean_v_implications <- clean_v_corpus(scraped$implications)

class(clean_v_implications)
# Find term frequencies 
frequency <- freq_terms(
  clean_v_implications,
  top = 10,
  stopwords = "Top200Words"
)

# Plot term frequencies
plot(frequency) + labs(title="Top 10 words with a \"at.least\" 0 letters")

# Find term frequencies 
frequency <- freq_terms(
  clean_v_implications,
  top = 10,
  at.least = 8,
  stopwords = "Top200Words"
)
# Plot term frequencies
plot(frequency) + labs(title="Top 10 words with a \"at.least\" 8 letters")

frequency <- freq_terms(
  clean_v_implications,
  top = 10,
  at.least = 10,
  stopwords = "Top200Words"
)
# Plot term frequencies
plot(frequency) + labs(title="Top 10 words with a \"at.least\" 10 letters")
save_images_on_deck()

# Word Clouds ------------------------------------------------------------------
library(wordcloud)
# Size is relative to word frequency
scraped <- read_csv("article_info.csv")
scraped
rm(df_source)
colnames(scraped) <- c("doc_id", "art_title","what_is_known","what_is_added","text","publication_date")
# Create a DataframeSource from scraped after labeling doc_id and text of interest
df_source <- DataframeSource(scraped)
class(df_source)
# Convert df_source to a volatile corpus
df_corpus <- VCorpus(df_source)
# Examine df_corups
df_corpus
class(df_corpus)


# Convert TDM to matrix
# from earlier
implications_m <- as.matrix(implications_tdm)
# Sum rows and sort by frequency
term_frequency <- rowSums(implications_m)
word_freqs <- data.frame(term = names(term_frequency),
                         num = term_frequency)
# Make word cloud
# Selecting 3 colors will naturally divide the term frequency into "low", 
# "medium", and "high" for easier viewing.

library(viridisLite)
# select 5 colors
color_pal <- magma(n = 3)
png("plots/tm_implications_wordcloud.png", width=12,height=8, units="in", res=300)
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = c("grey80", "darkgoldenrod1",  "tomato"))
dev.off()

frequency <- freq_terms(
  clean_v_implications,
  top = 100,
  at.least = 0,
  stopwords = "Top200Words"
)

png("plots/qdap_v0_implications_wordcloud.png", width=12,height=8, units="in", res=300)
wordcloud(frequency$WORD, frequency$FREQ, max.words = 100, colors = c("grey80", "darkgoldenrod1",  "tomato"))
dev.off()

frequency <- freq_terms(
  clean_v_implications,
  top = 100,
  at.least = 10,
  stopwords = "Top200Words"
)

png("plots/qdap_v10_implications_wordcloud.png", width=12,height=8, units="in", res=300)
wordcloud(frequency$WORD, frequency$FREQ, max.words = 100, colors = c("grey80", "darkgoldenrod1",  "tomato"))
dev.off()

# Other word clouds and word networks
# The wordcloud library provides multiple functions for creating word clouds
# from one or more corpora.
# If you want to make a word cloud from a single corpus, then you use the wordcloud
# funtion with rowSums like before.

## Commonality -----------------------------------------------------------------
# Plot a cloud of words shared across documents
clean_v_known <- clean_v_corpus(scraped$what_is_known)
class(clean_v_known)

clean_v_added <- clean_v_corpus(scraped$what_is_added)
class(clean_v_added)

# Combine both corpora: all_sections
all_known <- paste(clean_v_added, collapse = "")
all_added <- paste(clean_v_known, collapse = "")
all_implications <- paste(clean_v_implications, collapse = "")
all_sections <- c(all_known, all_added, all_implications)

# Re-Clean all_tweets
all_sections <- VectorSource(all_sections) 
all_corpus <- VCorpus(all_sections)
all_clean <- clean_corpus(all_corpus)
all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)
# Make commonality cloud

png("plots/commonality_cloud.png", width=12,height=8, units="in", res=300)
commonality.cloud(all_m, max.words = 100, colors = c("grey80", "darkgoldenrod1",  "tomato"))
dev.off()


## Comparison clouds -----------------------------------------------------------
# Plot a cloud comparing the frequencies of words across documents.
# A comparison cloud allows us to study the differences or similarities between
# two or more individuals' speeches or literature by simply plotting the word
# cloud of each against the other. In this recipe, we will study the inaugural
# speeches given by President Obama and former president George Bush.
# Comparing the reporting of News Agencies
all_comparison_tdm <- all_tdm
colnames(all_comparison_tdm) <- c("known", "added", "implications")
all_cm <- as.matrix(all_comparison_tdm)
# head(all_cm, 50)
# Make comparison cloud
png("plots/comparison_cloud.png", width=12,height=8, units="in", res=300)
comparison.cloud(all_cm, colors = c("orange", "blue", "red"), max.words = 50)
dev.off()

# Another way to visualize the conjunction of two corpora is with a polarized tag plot
# Pyramid plots ------
library(plotrix)
# Identify terms shared by both documents
## Known vs. Implications ------
common_words <- subset(
  all_cm,
  all_cm[, 1] > 0 & all_cm[, 3] > 0
)
# Find most commonly shared words
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = TRUE), ]
top50_df <- data.frame(x = common_words[1:50, 1], 
                       y = common_words[1:50, 2],
                       labels = rownames(common_words[1:50, ]))

# Make pyramid plot
png("plots/known_implications_pyramid.png", width=12,height=8, units="in", res=300)
pyramid.plot(top50_df$x, top50_df$y, 
             labels = top50_df$labels, 
             main = "Words in Common",
             gap = 22,
             # laxly = NULL, 
             raxlab = NULL, 
             unit = NULL, 
             top.labels = c("known", 
                            "Words", 
                            "implications"))
dev.off()

### Added vs. Implications -----
common_words <- subset(
  all_cm,
  all_cm[, 2] > 0 & all_cm[, 3] > 0
)
# Find most commonly shared words
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = TRUE), ]
top50_df <- data.frame(x = common_words[1:50, 1], 
                       y = common_words[1:50, 2],
                       labels = rownames(common_words[1:50, ]))

# Make pyramid plot
pyramid.plot(top50_df$x, top50_df$y, 
             labels = top50_df$labels, 
             main = "Words in Common",
             gap = 22,
             # laxly = NULL, 
             raxlab = NULL, 
             unit = NULL, 
             top.labels = c("added", 
                            "Words", 
                            "implications"))


# Word networks ------
# Create word network

png("plots/known_children_word_network.png", width=12,height=8, units="in", res=300)
word_associate(clean_v_known,
               match.string = c("children"),
               stopwords = c(Top200Words), #, "coffee", "amp"),
               network.plot = TRUE,
               cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Children Word Associations from what_is_known section")
dev.off()

png("plots/implications_children_word_network.png", width=12,height=8, units="in", res=300)
word_associate(clean_v_implications,
               match.string = c("school"),
               stopwords = c(Top200Words), #, "coffee", "amp"),
               network.plot = TRUE,
               cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Children Word Associations from implications section")
dev.off()

png("plots/added_children_word_network.png", width=12,height=8, units="in", res=300)
word_associate(clean_v_added,
               match.string = c("school"),
               stopwords = c(Top200Words), #, "coffee", "amp"),
               network.plot = TRUE,
               cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Children Word Associations from what_is_added section")
dev.off()


# Improvements -----------------------------------------------------------------
# Dealing with TDM/DTM sparsity

