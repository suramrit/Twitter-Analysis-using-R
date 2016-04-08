require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(SnowballC)

temp <- list.files(path ="C:\\Users\\suramrit\\Documents\\R\\Primary Data",pattern="*.json",full.names=TRUE)
primary_data<- data.frame()
candidates <- c("donald trump|trump|donald","hilary clinton|clinton|hilary",
                "bernie|sanders|bernie sanders","marco rubio|rubio","ben carson|carson", "ted cruz|cruz")
day_mentions <- data.frame()

for (i in 1:length(temp)){
  file_name = paste0(c('primary','_',i),collapse = "")
  day_name = paste0(c('day','_',i,'_mentions'),collapse = "")
  #assign(file_name, read.xls(temp[i],quote = NULL,header=TRUE ))
  assign(file_name, parseTweets(temp[i], simplify = FALSE))
  assign(day_name, data.frame(candidate= factor(), num = factor()))
  temp2 <- get(file_name) nrow()
  temp3 <- get(day_name)
  for(j in candidates){
    sub <- temp2[grep(j, temp2$text), ]
    temp3<- rbind(temp3, data.frame(day=i,candidate = j,num=nrow(sub)))
  }
  day_mentions <- rbind(day_mentions,temp3)
  primary_data <- rbind(primary_data, temp2)
 }

#have all tweet data at this point.. 

weekly_mentions <- data.frame(candidate = factor(), num = factor())
# tweet mentions for each popular candidate
for(i in candidates){
  sub2 <- primary_data[grep(i, primary_data$text), ]
  weekly_mentions<- rbind(weekly_mentions, data.frame(candidate = i,num=nrow(sub2)))
}

#doing location analysis-- weekly
visited <- subset(primary_data,  !(is.na(primary_data$location)))$location[1:200]
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
ggplot() +   mapWorld  +geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=1.5) 


primary_1 <- primary_1[order(-(primary_1$retweet_count)),]
primary_2 <- primary_2[order(-(primary_2$retweet_count)),] 
primary_3 <- primary_3[order(-(primary_3$retweet_count)),] 
primary_4 <- primary_4[order(-(primary_4$retweet_count)),] 
primary_5 <- primary_5[order(-(primary_5$retweet_count)),] 
primary_6 <- primary_6[order(-(primary_6$retweet_count)),] 
primary_7 <- primary_7[order(-(primary_7$retweet_count)),] 
#doing location analysis daily
visited_1 <- subset(primary_1,  !(is.na(primary_1$location)))$location[1:20]
ll.visited_1 <- geocode(visited_1) 
visit_1.x <- ll.visited_1$lon  
visit_1.y <- ll.visited_1$lat 

visited_2 <- subset(primary_2,  !(is.na(primary_2$location)))$location[1:20]
ll.visited_2 <- geocode(visited_2)
visit_2.x <- ll.visited_2$lon
visit_2.y <- ll.visited_2$lat

visited_3 <- subset(primary_3,  !(is.na(primary_3$location)))$location[1:20]
ll.visited_3 <- geocode(visited_3)
visit_3.x <- ll.visited_3$lon
visit_3.y <- ll.visited_3$lat

visited_4 <- subset(primary_4,  !(is.na(primary_4$location)))$location[1:20]
ll.visited_4 <- geocode(visited_4)
visit_4.x <- ll.visited_4$lon
visit_4.y <- ll.visited_4$lat

visited_5 <- subset(primary_5,  !(is.na(primary_5$location)))$location[1:20]
ll.visited_5 <- geocode(visited_5)
visit_5.x <- ll.visited_5$lon
visit_5.y <- ll.visited_5$lat

visited_6 <- subset(primary_6,  !(is.na(primary_6$location)))$location[1:20]
ll.visited_6 <- geocode(visited_6)
visit_6.x <- ll.visited_6$lon
visit_6.y <- ll.visited_6$lat


ggplot() +   mapWorld  +geom_point(aes(x=visit_1.x, y=visit_1.y) ,color="blue", size=1.5)+
  geom_point(aes(x=visit_2.x, y=visit_2.y) ,color="red", size=1.5) +
  geom_point(aes(x=visit_3.x, y=visit_3.y) ,color="green", size=1.5) +
  geom_point(aes(x=visit_4.x, y=visit_4.y) ,color="orange", size=1.5) +
  geom_point(aes(x=visit_5.x, y=visit_5.y) ,color="black", size=1.5) +
  geom_point(aes(x=visit_6.x, y=visit_6.y) ,color="yellow", size=1.5) 

new_tweets <- data.frame()
while(TRUE){
filterStream(file.name = "new_tweets.json", # Save tweets in a json file
             track = c("primary election","us election","trump","donald trump",
                       "hilary clinton", "clinton","democrats",
                       "republicans","sanders","bernie sanders",
                       "caucuses result","nevada primary",
                       "south carolina primary","iowa primary","new hampshire result",
                       "presidential election us","republican result",
                       "democrat result","primaries delegate result",
                       "new york times bernie sanders","new york times hilary clinton","new york times donald trump",
                       "fox news bernie sabders","fox news hilary","fox news donald trump",
                       "huffington post bernie sanders","huffington post hilary clinton","huffington post donald trump",
                       "cnbc bernie sanders","cnbc donald trump","cnbc hilary clinton",
                       "bloomberg bernie sanders","bloomberg hilary clinton","bloomberg donald trump",
                       "bbc bernie sanders","bbc hsilary clinton","bbc donald trump",
                       "new york times primary elections","fox news primary elections", "cnbc primary elections","bloomberg primary elections"),
             language = "en",
             timeout = 1,
             oauth = my_oauth) 
Sys.sleep(5)
}


