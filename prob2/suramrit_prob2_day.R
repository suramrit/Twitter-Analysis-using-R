
library("doBy")
library("ggplot2")

nyt_data<- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

nyt_data$age_cat<- cut(nyt_data$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

nyt_data$imp_bool<- cut(nyt_data$Impressions,c(-Inf,0,2,4,6,8,10,Inf))

attach(nyt_data)

range <- function(x){c(length(x), min(x), mean(x), max(x))}
len <- function(x){c(length(x))}
sum <- function(x){c(sum(x))}

#age cat vs no of clicks
sum_age_cat<-summaryBy(Clicks~age_cat, data =nyt_data, FUN=range)
#shows that age between 34-64 has max users
ggplot(sum_age_cat , aes(sum_age_cat$age_cat,sum_age_cat$Clicks.FUN4)) +geom_bar(stat = "identity") + xlab("Category of Age") + ylab("Total no of Users")
#which age cat has highest clicks

sum_age_cat2<-summaryBy(Clicks~age_cat, data =nyt_data, FUN=base::sum)
#shows that age between 34-64 have maximum clicks.
ggplot(sum_age_cat2 , aes(sum_age_cat2$age_cat,sum_age_cat2$'Clicks.base::sum')) +geom_bar(stat = "identity") + xlab("Category of Age") + ylab("Total Clicks")

ggplot(subset(nyt_data,Clicks>0) , aes(x=Clicks, color = age_cat)) + geom_density()
#clicks for males vs clicks females... 

ggplot(subset(nyt_data,Clicks>0&Gender == 0) , aes(x=Clicks, color = age_cat)) + geom_density()
#age distributions for each age category
#also calculate median ages....
ggplot(subset(nyt_data,Clicks>0&Gender == 1) , aes(x=Age, color = age_cat)) + geom_density()
ggplot(subset(nyt_data,Clicks>0&Gender == 0) , aes(x=Age, color = age_cat)) + geom_density()

ggplot(subset(nyt_data,Clicks>0&Age>0&Gender == 1) , aes(age_cat, Age, color=age_cat)) + geom_jitter()+ geom_boxplot()

#male vs female clicks/Impressions...
c_females<-base::sum((subset(nyt_data,Gender>0)$Clicks))
c_males<-base::sum((subset(nyt_data,Gender<1)$Clicks))
c_summary <- data.frame(Gen = c('Male','Female'), Clicks = c(c_males,c_females))
ggplot(c_summary, aes(c_summary$Gen,c_summary$Clicks))+geom_bar(stat ="identity")+xlab("Gender") + ylab("Total Clicks")

# proportions signed in users...

sign_in<-subset(nyt_data, Signed_In == 1)  
singed_V_non<- nrow(sign_in)/nrow(nyt_data)  
#70 pc users are signed in.. pretty high  
  
#categorizing on basis of age groupmax <- function(x){c(length(x))}
summaryBy(Clicks~age_cat, data =age_click_summary, FUN=max)
age_click<-summaryBy(Clicks~age_cat, data =age_click_summary, FUN=max)
ggplot(age_click, aes(x=age_click$age_cat, y=age_click$Clicks.max))+ geom_count()

summaryBy(Gender+Signed_In+Impressions+Clicks~age_cat,data = nyt_data)

plot(Age,Clicks)

summaryBy(Clicks~imp_bool, data = nyt_data)

nyt_data$scode[nyt_data$Impressions==0] <- "NoImps"
nyt_data$scode[nyt_data$Impressions >0] <- "Imps"
nyt_data$scode[nyt_data$Clicks >0] <- "Clicks"

nyt_data$scode <- factor(nyt_data$scode)
head(nyt_data)

ggplot(nyt_data, aes(x=Impressions, fill=age_cat))+geom_histogram(binwidth=1)
ggplot(nyt-data, aes(x=agecat, y=Impressions, fill=agecat))+geom_boxplot()
ggplot(nyt_data, aes(x=Clicks, fill=imp_bool))+geom_bar()


#Impression vs age category...
ggplot(subset(nyt_data, Impressions>0), aes(Clicks/Impressions,color = age_cat)) + geom_density()

nyt_data$ctr[nyt_data$Impressions == 0] <- 0
nyt_data$ctr[nyt_data$Impressions > 0] <- (nyt_data$Clicks / nyt_data$Impressions)

nyt_data$ctr <- factor(nyt_data$ctr)

summaryBy(nyt_data$ctr~age_cat, data = nyt_data)

