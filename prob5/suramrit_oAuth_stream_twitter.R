library(streamR)
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "okSjMmkMA1JafXfilBQ0jG7DR" # From dev.twitter.com
consumerSecret <- "8Zs3VKH9qQZT4z2cBLaNryGZmZ8c5eGJfI4WWkPVZ4TdE2IJUM" # From dev.twitter.com

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

live<- "new_tweets.json"

library(streamR)
library(ROAuth)
filterStream(file.name = "primary_day_7.json", # Save tweets in a json file
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
                       "bbc bernie sanders","bbc hilary clinton","bbc donald trump",
                       "new york times primary elections","fox news primary elections", "cnbc primary elections","bloomberg primary elections"),
             language = "en",
             timeout = 100,
             oauth = my_oauth)  #Use my_oauth file as the OAuth credentials




tweets_primary.df <- parseTweets("primary_day_7.json", simplify = FALSE)
#tweets_property.df<- parseTweets("property_day_1.json", simplify = FALSE)

colnames(tweets_primary.df)

drop_cols <- c("favorited","favoriteCount","replyToSN","truncated","replyToSID","statusSource","retweetCount","isRetweet","retweeted","in_reply_to_screen_name",  "source"       ,             "retweeted"                ,    "in_reply_to_status_id_str" 
               , "in_reply_to_user_id_str" ,                        "listed_count"        ,                                      
                "user_id_str"          ,     "description"    ,           "geo_enabled"       ,        "user_created_at"      ,     "statuses_count" ,          
                "followers_count"   ,            "protected"       ,          "user_url"     ,                                
                "time_zone"   ,              "user_lang"    ,             "utc_offset"     ,   "country_code"     ,                   "place_type"     ,           "full_name"       ,          "place_name" ,              
                "place_id"   ,              "expanded_url"         ,     "url")

clean_tweets_primary.df <- tweets_primary.df[,!(names(tweets_primary.df) %in% drop_cols)]
#clean_tweets_property.df <- tweets_property.df[,!(names(tweets_property.df) %in% drop_cols)]

clean_tweets_primary.df <- unique(clean_tweets_primary.df)
#clean_tweets_property.df <- unique(clean_tweets_property.df)

nrow(clean_tweets_primary.df)
#nrow(clean_tweets_property.df)

library(xlsx)
write.xlsx(clean_tweets_primary.df,"primary_day_7.xls")
#write.xlsx(clean_tweets_property.df,"property_day_1.xls")



