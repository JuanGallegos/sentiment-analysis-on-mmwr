library(tidyverse)
library(syuzhet)
library(plotly)
library(tm)
library(qdap)
library(qdapRegex)
library(wordcloud)
library(viridisLite)
library(plotrix)
library(tidyft)
source("func.R")


scraped <- read_csv("article_info.csv")
scraped$implications <- clean_v_corpus(scraped$implications)
scraped$what_is_known <- clean_v_corpus(scraped$what_is_known)
scraped$what_is_added <- clean_v_corpus(scraped$what_is_added)

# Let’s compare the four lexicons and put them into a data frame:
syuzhet <- get_sentiment(scraped$implications, method="syuzhet")
bing <- get_sentiment(scraped$implications, method="bing")
afinn <- get_sentiment(scraped$implications, method="afinn")
nrc <- get_sentiment(scraped$implications, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc, publication_date=scraped$publication_date)
sentiments$publication_date <- as.Date(sentiments$publication_date, format="%B %d, %Y")

sentiments %>% arrange(publication_date)

# Emotion analysis can be done with the NRC Emotion lexicon:
# get the emotions using the NRC dictionary
j <- get_nrc_sentiment(scraped$implications)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Now we are ready to visualize the results, let’s start by comparing the sentiment scores across the four methods:
# plot the different sentiments from different methods
plot_ly(sentiments, x=~publication_date, y=~syuzhet, type="scatter", mode="jitter", name="syuzhet") %>%
  add_trace(y=~bing, mode="lines", name="bing") %>%
  add_trace(y=~afinn, mode="lines", name="afinn") %>%
  add_trace(y=~nrc, mode="lines", name="nrc") %>%
  layout(title="Sentiments of Implications section in Article",
        yaxis=list(title="score"), xaxis=list(title="publication_date"))

# They look pretty consistent/correlated! Then we can see what sorts of 
# emotions are dominant in the Implications section:

# Visualize the emotions from NRC sentiments
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for Implications")

# Visualize the emotions from NRC sentiments
emo_sum %>% filter(emotion != "negative" & emotion != "positive") %>%
plot_ly(., x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for Implications section")

# Mainly positive! Again this is good news for HDB and this results agree with our previous analysis with the old sentiment package.
# Finally, let’s see which word contributes to which emotion:
# head(implications.sentiment,2)
save_images_on_deck()

# Comparison word cloud
all = c(
  paste(scraped$implications[emotions$anger > 0], collapse=" "),
  paste(scraped$implications[emotions$anticipation > 0], collapse=" "),
  paste(scraped$implications[emotions$disgust > 0], collapse=" "),
  paste(scraped$implications[emotions$fear > 0], collapse=" "),
  paste(scraped$implications[emotions$joy > 0], collapse=" "),
  paste(scraped$implications[emotions$sadness > 0], collapse=" "),
  paste(scraped$implications[emotions$surprise > 0], collapse=" "),
  paste(scraped$implications[emotions$trust > 0], collapse=" ")
)

all <- removeWords(all, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
class(tdm)
class(tdm1)
# add column names
colnames(tdm) <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
class(tdm)
class(tdm1)
tdm["positive",]
tdm["negative",]

comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

# test<-as.matrix(TermDocumentMatrix(Corpus(VectorSource(removeWords(paste(scraped$implications[emotions$anger > 0], collapse=" "), stopwords("english"))))))
# test["positive",]
# test["negative",]

png("plots/emotion_comparison_cloud.png", width=12,height=8, units="in", res=300)
# comparison.cloud(tdm1, colors = c("orange", "blue", "red"), max.words = 50)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=100, scale=c(2.5, 0.4),rot.per=0.4)
dev.off()

# As a note, I need to exclude words with more than 11 characters (< 7%) so that
# the words can fit nicely into the wordcloud. In practice we can also shorten these long words instead of removing them.

# https://rstudio-pubs-static.s3.amazonaws.com/283881_efbb666d653a4eb3b0c5e5672e3446c6.html

################################################################################

ps_neg = c(
  paste(scraped$implications[emotions$positive > 0], collapse=" "),
  paste(scraped$implications[emotions$negative > 0], collapse=" ")
)

ps_neg <- removeWords(ps_neg, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(ps_neg))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
class(tdm)
class(tdm1)
# add column names
colnames(tdm) <- c('positive', 'negative')
colnames(tdm1) <- colnames(tdm)
class(tdm)
class(tdm1)
tdm1["positive",]
tdm1["negative",]


# test<-as.matrix(TermDocumentMatrix(Corpus(VectorSource(removeWords(paste(scraped$implications[emotions$anger > 0], collapse=" "), stopwords("english"))))))
# test["positive",]
# test["negative",]

png("plots/positive_negative_comparison_cloud.png", width=12,height=8, units="in", res=300)
# comparison.cloud(tdm1, colors = c("orange", "blue", "red"), max.words = 50)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("black", "gray"),
                 title.size=1, max.words=100, scale=c(2.5, 0.4),rot.per=0.4)
################################################################################
nrc <- get_sentiment(scraped$implications, method="nrc")
nrc_sentiments <- data.frame(nrc, publication_date=scraped$publication_date)
nrc_sentiments$publication_date <- as.Date(nrc_sentiments$publication_date, format="%B %d, %Y")

nrc_sentiments %>% arrange(publication_date)

plot_ly(nrc_sentiments, x=~publication_date, y=~nrc, type="scatter", mode="jitter", name="nrc") %>%
  layout(title="NRC Sentiments of Implications section",
         yaxis=list(title="score"), xaxis=list(title="publication_date"))

nrc_sentiments %>%
  ggplot(aes(publication_date,nrc)) +
  geom_line() +
  labs(title="NRC Sentiments of Implications section") +
  # geom_point()+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

nrc_sentiments %>% 
  ggplot(aes(publication_date,nrc)) +
  geom_smooth()+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

get_sentiment_dictionary(dictionary = "nrc", language = "english") %>% filter(word=="health")
tdm["health",]
sentiments[sentiments$term == "pregnant",]


################################################################################
# Emotions correction
clean_v_implications <- clean_v_corpus(scraped$implications)
class(clean_v_implications)

clean_v_added <- clean_v_corpus(scraped$what_is_added)
class(clean_v_added)
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

all_comparison_tdm <- all_tdm
colnames(all_comparison_tdm) <- c("known", "added", "implications")
all_cm <- as.matrix(all_comparison_tdm)

# Attempting to plot Implications Emotion Cateogry Associations
class(all_cm)
all_cm[,3]
wwordcontribution<-as.data.frame(cbind(rownames(all_cm), all_cm[,3]))
wwordcontribution$V1
wwordcontribution$V2
wordcontributionnrc <- get_nrc_sentiment(wcontribution$V1)
length(wwordcontribution$V1)
length(wordcontributionnrc)
sentiments <- data.frame(term=wwordcontribution$V1, freq=wwordcontribution$V2, wordcontributionnrc)
sentiments

sentiment_terms<-clean_v_corpus(sentiments$term)

# Comparison word cloud
all = c(
  paste(sentiment_terms[sentiments$anger > 0], collapse=" "),
  paste(sentiment_terms[sentiments$anticipation > 0], collapse=" "),
  paste(sentiment_terms[sentiments$disgust > 0], collapse=" "),
  paste(sentiment_terms[sentiments$fear > 0], collapse=" "),
  paste(sentiment_terms[sentiments$joy > 0], collapse=" "),
  paste(sentiment_terms[sentiments$sadness > 0], collapse=" "),
  paste(sentiment_terms[sentiments$surprise > 0], collapse=" "),
  paste(sentiment_terms[sentiments$trust > 0], collapse=" ")
)

all <- removeWords(all, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)



for(j in 1:length(tdm[,1])) {
  get_value_where_word <- as.numeric(sentiments[sentiments$term == rownames(tdm)[j],]$freq)
  for(i in 1:8) {
    
    if(tdm[j, i] == 1) {
      tdm[j, i] <- get_value_where_word
    }
  }
}
 
sentiments
sentiments[sentiments$term == "sex",]

class(tdm)
# add column names
colnames(tdm) <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')

class(tdm)

tdm["sex",]
tdm["negative",]
sentiments
sentiments[sentiments$term == "adherence",]



comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
tdm["sex",]

#############
### Solving how to make freq of emtions
for(j in 1:length(tdm[,1])) {
  get_value_where_word <- as.numeric(sentiments[sentiments$term == rownames(tdm)[j],]$freq)
  for(i in 1:8) {
    
    if(tdm[j, i] == 1) {
      tdm[j, i] <- get_value_where_word
    }
  }
}

get_value_where_word <- as.numeric(sentiments[sentiments$term == "abuse",]$freq)
for(i in 1:8) {
  
  if(tdm["abuse", i] == 1) {
    tdm["abuse", i] <- get_value_where_word
  }
}

tdm["action", ]
tdm["abuse", ]

tdm[1, ]
rownames(tdm)[1]

#############3


# Positive Negative Correction
# Comparison word cloud
all = c(
  paste(sentiment_terms[sentiments$positive > 0], collapse=" "),
  paste(sentiment_terms[sentiments$negative > 0], collapse=" ")
)

all <- removeWords(all, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)



for(j in 1:length(tdm[,1])) {
  get_value_where_word <- as.numeric(sentiments[sentiments$term == rownames(tdm)[j],]$freq)
  for(i in 1:2) {
    
    if(tdm[j, i] == 1) {
      tdm[j, i] <- get_value_where_word
    }
  }
}

sentiments
sentiments[sentiments$term == "sex",]

class(tdm)
# add column names
colnames(tdm) <- c('positive', 'negative')

class(tdm)

tdm["sex",]
tdm["negative",]
sentiments
sentiments[sentiments$term == "adherence",]
tdm


comparison.cloud(tdm, random.order=FALSE,
                 colors = c("black", "gray"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
tdm["women",] 

