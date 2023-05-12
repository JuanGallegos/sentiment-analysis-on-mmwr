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
library(rvest)

scraped <- read_csv("article_info.csv")

scraped$implications <- clean_v_corpus(scraped$implications)
scraped$what_is_known <- clean_v_corpus(scraped$what_is_known)
scraped$what_is_added <- clean_v_corpus(scraped$what_is_added)

what_is_known.sentiment <- data.frame()
# What is known
for(i in 1:length(scraped$what_is_known)) {
  j <- get_nrc_sentiment(scraped$what_is_known[i])
  j$publication_date <-scraped$publication_date[i]
  j$art_title <-scraped$art_title[i]
  what_is_known.sentiment <- rbind(what_is_known.sentiment,j)
}

what_is_added.sentiment <- data.frame()
# What is added
for(i in 1:length(scraped$what_is_added)) {
  j <- get_nrc_sentiment(scraped$what_is_added[i])
  j$publication_date <-scraped$publication_date[i]
  j$art_title <-scraped$art_title[i]
  what_is_added.sentiment <- rbind(what_is_added.sentiment,j)
}

implications.sentiment <- data.frame()

# Implications
for(i in 1:length(scraped$implications)) {
  j <- get_nrc_sentiment(scraped$implications[i])
  j$publication_date <-scraped$publication_date[i]
  j$art_title <-scraped$art_title[i]
  implications.sentiment <- rbind(implications.sentiment,j)
}

article.sentiments.what_is_known <- what_is_known.sentiment %>% select(-art_title) %>%
  group_by(publication_date) %>%
  summarize(across(anger:positive,mean)) %>%
  drop_na() %>%
  gather("trait","avg",-c("publication_date")) %>%
  mutate(category="what_is_known")

article.sentiments.what_is_added <- what_is_added.sentiment %>% select(-art_title) %>%
  group_by(publication_date) %>%
  summarize(across(anger:positive,mean)) %>%
  drop_na() %>%
  gather("trait","avg",-c("publication_date")) %>%
  mutate(category="what_is_added")

implications.sentiment %>% arrange(publication_date)

article.sentiments.implications <- implications.sentiment %>% select(-art_title) %>%
  group_by(publication_date) %>%
  summarize(across(anger:positive,mean)) %>%
  drop_na() %>%
  gather("trait","avg",-c("publication_date")) %>%
  mutate(category="implications")

article.sentiments.what_is_known$publication_date <- as.Date(article.sentiments.what_is_known$publication_date, format="%B %d, %Y")
article.sentiments.what_is_added$publication_date <- as.Date(article.sentiments.what_is_added$publication_date, format="%B %d, %Y")
article.sentiments.implications$publication_date <- as.Date(article.sentiments.implications$publication_date, format="%B %d, %Y")
article.sentiments <- rbind(article.sentiments.what_is_known, article.sentiments.what_is_added, article.sentiments.implications)

implications.sentiment$publication_date <- as.Date(implications.sentiment$publication_date, format="%B %d, %Y")
implications.sentiment %>% arrange(publication_date)



# head(what_is_known.sentiment[1:11], 6)
# article.sentiments %>% filter(category=="what_is_known") %>% arrange(desc(publication_date))
(article.sentiments %>% group_by(trait)) %>% filter(avg>4)
summary(article.sentiments$avg)

what_is_known.valence <- (what_is_known.sentiment[,9]*-1) + what_is_known.sentiment[,10]
what_is_known.valence

# dev.new()

# Yes: All
article.sentiments %>% filter(trait == "positive" | trait == "negative" | trait == "fear") %>%
  ggplot(.,aes(publication_date,avg, color=trait))+
  geom_line() + 
  labs(title="Sentiment across time", subtitle="All sections in Bag of Words") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Yes: Just Implications
article.sentiments.implications %>% filter(trait == "positive" | trait == "negative" | trait == "fear") %>%
  ggplot(.,aes(publication_date,avg, color=trait))+
  geom_line() + 
  labs(title="Sentiment across time", subtitle="Implications section in Bag of Words") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# No: All
ggplot(article.sentiments,aes(trait,avg))+
  geom_bar(stat="identity") #,position="dodge")+
  theme_bw()+
  theme(legend.position = "top")+
  coord_flip()

# Yes  
  barplot(
    sort(colSums(prop.table(what_is_known.sentiment[, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions in Known Sections", xlab="Percentage"
  )

  barplot(
    sort(colSums(prop.table(implications.sentiment[, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions in Implications Sections", xlab="Percentage"
  )  
  
  barplot(
    sort(colSums(prop.table(what_is_added.sentiment[, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions in Added Sections", xlab="Percentage"
  )  
  
  
# All without Positive and Negative
article.sentiments %>% 
  filter(trait != "positive", trait != "negative") %>%
  ggplot(., aes(trait, avg)) +
  geom_bar(stat="identity",position="dodge") +
  theme_bw() +
  theme(legend.position = "top") +
  coord_flip()

# Yes: By category with Positive and Negative
ggplot(article.sentiments,aes(trait,avg,fill=category))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(legend.position = "top")+
  coord_flip()

# Yes: By category without Positive and Negative
article.sentiments %>% 
  filter(trait != "positive", trait != "negative") %>%
  ggplot(., aes(trait, avg, fill=category)) +
  geom_bar(stat="identity",position="dodge") +
  theme_bw() +
  theme(legend.position = "top") #+
  #coord_flip()
  
# Yes: Only line without Smooth
article.sentiments %>% filter(trait == "positive" | trait == "negative") %>%
  ggplot(aes(publication_date,avg)) +
  geom_line() +
  # geom_point()+
  theme_bw() + 
  facet_wrap(vars(trait),nrow=2,)

# Yes: With Smooth
article.sentiments %>% filter(trait == "positive" | trait == "negative") %>%
  ggplot(aes(publication_date,avg)) +
  geom_smooth()+
  theme_bw() + 
  facet_wrap(vars(trait),nrow=2,)

# Yes: All with Smooth
article.sentiments %>% filter(trait != "positive", trait != "negative") %>%
  ggplot(aes(publication_date,avg)) +
  geom_smooth()+
  facet_wrap(vars(trait),nrow=2,)

# Yes
article.sentiments %>% filter(trait == "positive" | trait == "negative" | trait == "fear") %>%
ggplot(.,aes(publication_date,avg))+
  geom_smooth(aes(col=trait))+
  facet_wrap(vars(category),nrow=1)


# Yes: All lines seperated
article.sentiments %>% # filter(trait != "positive", trait != "negative") %>%
  ggplot(aes(publication_date,avg))+
  geom_line(aes(col=category))+
  facet_grid(trait~category)

save_images_on_deck()

