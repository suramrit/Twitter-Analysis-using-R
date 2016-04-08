library("doBy")
library(ggplot2)
library(plotrix)
library(stringr)
temp <- list.files(path ="C:\\Users\\suramrit\\Desktop\\DataIntensiveComputing\\dds_datasets\\dds_ch2_nyt",pattern="*.csv",full.names=TRUE)


max <- function(x){c(length(x))}
range <- function(x){c(length(x), min(x), mean(x), max(x))}
len <- function(x){c(length(x))}
sum <- function(x){c(sum(x))}


imp_summary <- list() #impression summary
month_data<-data.frame()
click_day<-data.frame(day = factor(),sum_c=factor())
click_sum <-vector()
day_frame<-list()
days <- vector()
imp_summary<- data.frame(day = numeric(), min_Imp = numeric())



#save each days csv in a single data frame
for (i in 1:length(temp)){
  
  file_name = paste0(c('day','_',i),collapse = "")
  assign(file_name, read.csv(temp[i]))
  
  temp2 <- get(file_name)
  temp2$age_cat<- cut(temp2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
  
  temp2$imp_bool<- cut(temp2$Impressions,c(-Inf,0,2,4,6,8,10,Inf))
  month_data <- rbind(month_data,temp2)
  day_frame <- data.frame(day  = file_name, sum_c = base::sum(temp2$Clicks))
  click_day <- rbind(click_day,day_frame)
  days <- append (days,file_name, after  = length(days))
  #imp_summary<-rbind(imp_summary, data.frame(day = i, min_Imp = min(subset(temp2,temp2$Clicks>0)$Clicks)))
  
}
attach(month_data)
sum_age_cat2<-summaryBy(Clicks~age_cat, data =month_data, FUN=base::sum)
#shows that age between 34-64 have maximum clicks.
ggplot(sum_age_cat2 , aes(sum_age_cat2$age_cat,sum_age_cat2$`Clicks.base::sum`)) +geom_bar(stat = "identity") + xlab("Category of Age") + ylab("Total Clicks")

#age cat vs no of clicks
sum_age_cat<-summaryBy(Clicks~age_cat, data =month_data, FUN=range)

#shows that age between 34-64 has max users
ggplot(sum_age_cat , aes(sum_age_cat$age_cat,sum_age_cat$Clicks.FUN4)) +geom_bar(stat = "identity") + xlab("Category of Age") + ylab("Total no of Users")
pie3D(sum_age_cat$Clicks.FUN4,labels=sum_age_cat$age_cat, main="Pie chart of age distribution", explode = 0.1)

#our observation for daily data is carried on to monthly data... this means that ads 
#most effective for middle aged ppl.. least affective for young users

#no male vs female users over the month

c_females<-base::sum((subset(month_data,Gender>0)$Clicks))
c_males<-base::sum((subset(month_data,Gender<1)$Clicks))

c_summary <- data.frame(gender=c('male','female'),Users = c(c_males,c_females))

ggplot(c_summary, aes(c_summary$gender,c_summary$Users))+geom_bar(stat ="identity")

# clicks distribution for each age category
ggplot(subset(month_data,Clicks>0) , aes(x=Clicks, color = age_cat)) + geom_density()
ggplot(subset(month_data,Clicks>0&Gender == 1) , aes(x=Age, color = age_cat)) + geom_density()
ggplot(subset(month_data,Clicks>0&Age>0) , aes(age_cat, Age, color=age_cat)) + geom_jitter()+ geom_boxplot()

# click per day for the month
ggplot(click_day, aes(click_day$day, click_day$sum_c)) + geom_point()+ geom_smooth(method = "lm", se = FALSE)+scale_x_discrete(labels = function(x) str_wrap(x, width = 3))+xlab("Day") + ylab("Clicks")
