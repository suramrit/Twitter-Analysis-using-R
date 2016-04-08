library(tm)
library(wordcloud)
library(SnowballC)
library(jsonlite)
#read all the property jsons into single data frame 


temp <- list.files(path ="C:\\Users\\suramrit\\Documents\\R\\Property Data",pattern="*.json",full.names=TRUE)
property_data<- data.frame()

day_mentions <- data.frame()

for (i in 1:length(temp)){
  file_name = paste0(c('property','_',i),collapse = "")
  readlines <- readLines(temp[i], warn = FALSE)
  property_data <- rbind(property_data, fromJSON(paste(readlines, collapse = ''),simplifyDataFrame = TRUE))
}

property_data<-unique(property_data)

# have tweets related to property 
# method learned from : https://mkmanu.wordpress.com/2014/08/05/sentiment-analysis-on-twitter-data-text-analytics-tutorial/
# and http://www.r-bloggers.com/building-wordclouds-in-r/


property_data$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", property_data$text)
# Then remove all "@people"
property_data$text <- gsub("@\\w+", "", property_data$text)
# Then remove all the punctuation
property_data$text <- gsub("[[:punct:]]", "", property_data$text)
# Then remove numbers, we need only text for analytics
 property_data$text <- gsub("[[:digit:]]", "", property_data$text)
# the remove html links, which are not required for sentiment analysis
 property_data$text <- gsub("http\\w+", "", property_data$text)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
 property_data$text <- gsub("[ \t]{2,}", "", property_data$text)
 property_data$text <- gsub("^\\s+|\\s+$", "", property_data$text)
 
 property_txt <- property_data$text
 
 property_txt <- property_txt[!is.na(property_txt)]
 
 names(property_txt) = NULL
 
 #converting to corpus and performing stemming, stop word removal and punctuation removal 
 
 propCorpus <- Corpus(VectorSource(property_txt))
 
 propCorpus <- tm_map(propCorpus, PlainTextDocument)
 
 propCorpus <- tm_map(propCorpus, removePunctuation)
 propCorpus <- tm_map(propCorpus, removeWords, stopwords('english'))
 
 propCorpus <- tm_map(propCorpus, stemDocument)
 pal <- brewer.pal(8,"Dark2")
 wordcloud(propCorpus, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
 
 # forecast result from Zillow Data...
 
 forecast<-read.csv("C:\\Users\\suramrit\\Desktop\\DataIntensiveComputing\\Zillow Data\\ZriForecast_Public.csv")
 
 forecast<- forecast[order(forecast$SizeRank),]
 
 forecast<- forecast [1:20,]
 
 forecast$fill <- with(forecast, ifelse(forecast$RegionName == "New York, NY", 1,0))
 
 ggplot(forecast, aes(forecast$RegionName, forecast$Current_YoY_Change, fill = factor(fill))) + 
   geom_bar(stat = "identity") + scale_fill_manual(values = c("1" = "blue", "0" = "white")) + xlab("Region Name") +ylab("year on year growth rate")
 # price to rental ratio ---- 
 prr<-data.frame()
 prr <-  read.csv("C:\\Users\\suramrit\\Desktop\\DataIntensiveComputing\\Zillow Data\\Metro_PriceToRentRatio_AllHomes.csv")
 prr<- prr[order(prr$SizeRank),]
 prr <- prr[1:10,]
 ggplot(prr, aes(prr$RegionName, prr$X2015.12, fill= prr$RegionName))+ geom_bar(stat="identity") + xlab("Metorpolitan")+ylab("Price to rent ratio")
 
 #show low sales from NY region.. 
 
 # market health analysis of zillow... 
 health<-read.csv("C:\\Users\\suramrit\\Desktop\\DataIntensiveComputing\\Zillow Data\\MarketHealthIndex_City.csv")
 
 health<- health[order(health$SizeRank),]
 
 health<- health [1:10,]
 
 ggplot(health, aes(health$City, health$MarketHealthIndex)) + geom_bar(stat = "identity")+ xlab("Metorpolitan")+ylab("Market Health Index")
 
 
 
 