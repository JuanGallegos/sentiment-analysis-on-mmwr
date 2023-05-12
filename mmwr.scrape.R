library(tidyverse)
library(rvest)

#website
url2 <- "https://www.cdc.gov/mmwr/Novel_Coronavirus_Reports.html?ACSTrackingID=USCDC_921-DM43690&ACSTrackingLabel=This%20Week%20in%20MMWR%20-%20Vol.%2069%2C%20December%204%2C%202020&deliveryName=USCDC_921-DM43690"

#get urls for articles
xxx.url <- url2 %>% read_html() %>%
  html_nodes("li, a") %>%
  html_attr("href")

#get article titles
xxx.title <- url2 %>% read_html() %>%
  html_nodes("li, a") %>%
  html_text()
#get article titles - titles start at 29 and end 751, and there are duplicates
url.list <- data.frame(art_title=xxx.title[29:751],art_url=xxx.url[29:751]) %>% drop_na() %>%
  mutate(art_title=trimws(art_title,"r"), art_url=paste0("https://www.cdc.gov",art_url))

#url <- url.list$art_url[1]

#function to get the article summary information
get_article_info <- function(url){
  xxx.text <- url %>% read_html() %>%
    html_nodes("p") %>%
    html_text  
  xxx <- url %>% read_html() %>%
    html_nodes("h1,.order-0") %>%
    html_text
  asummary <- gsub("\n","",xxx[2])
  asummary <- gsub("\\?","",asummary)
  asummary <- gsub("\r","",asummary)
  asummary <- gsub("What is already known about the topic","What is already known about this topic",asummary)
  asummary <- gsub("What is already known on this topic","What is already known about this topic",asummary)
  asummary <- gsub("this this","this",asummary)
  asummary <- gsub("What is added by the report","What is added by this report",asummary)
  asummary <- gsub("What are the implications for public health","What are the implications for public health practice",asummary)
  asummary <- gsub("practice practice","practice",asummary)
  xxx.summary <- data.frame(
    title=xxx[1],
    what_is_known = str_extract_all(asummary,"(?<=about this topic).+(?=What is added by this report)"),
    what_is_added = str_extract_all(asummary,"(?<=What is added by this report).+(?=What are the implications)"),
    implications = str_extract_all(asummary,"(?<=What are the implications for public health practice).+(?=)"),
    publication_date = str_split(xxx.text[1]," / ")[[1]][2]
  ) %>% "colnames<-"(c("art_title","what_is_known","what_is_added","implications","publication_date"))
  return(xxx.summary)
}

#create the final data set
art_info = NULL
for(i in 354:nrow(url.list)) {
  art_info=rbind(art_info,get_article_info(url.list$art_url[i]))
}



