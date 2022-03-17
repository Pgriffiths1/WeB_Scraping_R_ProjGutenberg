library(dplyr)
library(plyr)
library(quanteda)
library(readtext)
library(rvest)
library(tm)
library(wordcloud)
library(tidytext)
library(ggplot2)


wait<- function(x){
  date_time<-Sys.time()
  while((as.numeric(Sys.time())-as.numeric(date_time))<x){}
  
}

# list of book numbers as assigned by the project gutenberg site i.e 4300 is Ulysses by James Joyce

books<-c("4300", "64317", "521", "829", "6593", "1079", "158", "76", "84", "2701",
         "11", "514", "2833", "153", "76", "145", "2600", "2554", "100", "67138",
         "219", "1260", "63107", "1184", "768", "174", "345", "135", "996", "7849",
         "2413", "19942", "43", "140", "26", "61221", "8387", "4517", "47935", "63203")

allbooks <- data.frame(Book_Name=character(), Author=character()) #empty data set

for(i in 1:length(books)){
  
  theurl<- paste0("https://www.gutenberg.org/ebooks/",books[i])
  html<- read_html(theurl)
  
  #get the desired meta data, title and author
  get_meta<-html_nodes(html, ".bibrec" )
  table_m<-html_table(get_meta)
  meta<-data.frame(table_m[[1]])
  
  #get title and author (feels like a dumb way to do this but it works)
  indices<-which(meta == "Title", arr.ind=TRUE)
  title<-meta[indices[1,1],indices[1,2]+1]
  indices<-which(meta == "Author", arr.ind=TRUE)
  author<-meta[indices[1,1],indices[1,2]+1]
  
  meta_data<-data.frame(title, author)
  
  #get the html link so we can "read" the book
  get_link<-html_nodes(html,".files")
  table_l<-html_table(get_link)
  linktable<-data.frame(table_l)
  
  #making sure we get the .txt
  indices<-which(linktable == "Plain Text UTF-8", arr.ind=TRUE)
  txt_link<-linktable[indices[1,1],indices[1,2]+1]
  
  meta_data<-cbind(meta_data, txt_link)
  
  allbooks<-rbind(allbooks,meta_data)
  
  #2 second wait time to be safe so the website doesn't get mad and block my ip address
  wait(2)
}

str1<-""



for(i in 1:length(books)){
  
  url<-allbooks[i,3]
  rt<- readLines(url)
  combined_text<-paste(str1,rt)
  str1<-combined_text
  wait(2)
}
  
  
text<-Corpus(VectorSource(combined_text))
  
clean_text<-tm_map(text,removePunctuation)
clean_text<- tm_map(clean_text, content_transformer(tolower))
clean_text<- tm_map(clean_text, removeNumbers)
clean_text<- tm_map(clean_text, stripWhitespace)
clean_text<- tm_map(clean_text, removeWords, stopwords('english'))
clean_text<- tm_map(clean_text, removeWords, c("said","project","gutenberg"))
  
wordcloud(clean_text, scale = c(.4,1), min.freq = 50000, colors = rainbow(50))

url<-allbooks[10,3]
rt<- readLines(url)
text<-Corpus(VectorSource(rt))

clean_text<-tm_map(text,removePunctuation)
clean_text<- tm_map(clean_text, content_transformer(tolower))
clean_text<- tm_map(clean_text, removeNumbers)
clean_text<- tm_map(clean_text, stripWhitespace)
clean_text<- tm_map(clean_text, removeWords, stopwords('english'))
clean_text<- tm_map(clean_text, removeWords, c("said","project","gutenberg"))
wordcloud(clean_text, scale = c(.4,1), min.freq = 200, colors = rainbow(50))
