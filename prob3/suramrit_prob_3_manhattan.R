library(stringr)
library(gdata)
library("doBy")
library(ggplot2)
library(plotrix)
#reading
bk <- read.xls("C:\\Users\\suramrit\\Desktop\\DataIntensiveComputing\\dds_datasets\\dds_ch2_rollingsales\\rollingsales_bronx.xls")
head(bk)

max <- function(x){c(length(x))}
range <- function(x){c(length(x), min(x), mean(x), max(x))}
len <- function(x){c(length(x))}
sum <- function(x){c(sum(x))}

#cleaningsale_v_class_cat<-summaryBy(SALE.PRICE_num~BUILDING.CLASS.CATEGORY, data = bk, FUN= range )


bk$SALE.PRICE_num <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
bk$GROSS.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$GROSS.SQUARE.FEET))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$LAND.SQUARE.FEET))
bk$SALE.DATE <- as.Date(bk$SALE.DATE)

bk$year_cat<- cut(bk$YEAR.BUILT,c(-Inf,1800,1850,1900,1950,2000,2010,Inf))
bk$year_sold<- as.numeric(format(bk$SALE.DATE,'%Y'))
bk$year_sold_cat<- cut(bk$year_sold,c(-Inf,2010,2011,2012,2013,Inf))
bk$class_cat<- gsub("[^[[:alpha:]|[:space:]]","",bk$BUILDING.CLASS.CATEGORY)

#Analysis....
require(doBy)
attach(bk)
#No of homes in each category.. 
range <- function(x){c(length(x), min(x), mean(x), max(x))}
sale_v_class_cat<-summaryBy(SALE.PRICE_num~class_cat, data = bk, FUN= range )
ggplot(sale_v_class_cat, aes(sale_v_class_cat$class_cat, sale_v_class_cat$SALE.PRICE_num.FUN4)) + geom_bar(stat="identity")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+ xlab("Category of Unit") + ylab("Total Units Sold")

#price frequency distribution according to the age category...
ggplot(bk, aes(bk$SALE.PRICE_num, colour = bk$year_cat, fill = bk$year_cat)) +geom_density(alpha=0.5) + xlim(10000,7500000) + xlab("Sale Price")

price_v_year <- summaryBy(bk$SALE.PRICE_num~year_sold_cat, data =bk , FUN = base::sum)
# --- compare these two ------- ^ v ------ 
ggplot(bk, aes(bk$SALE.PRICE_num, colour = bk$year_sold_cat, fill = bk$year_sold_cat)) +geom_density(alpha=0.3) + 
  xlim(10000,7500000)+ xlab("Sale Price")

# analysis of price vs land area
summary(bk$GROSS.sqft)
bk$area_cat<- cut(bk$GROSS.sqft,c(-Inf,500,1000,1500,2000,2500,3000,3050,4000,4050,5000,5500,6000,6500,7000,7500,8000,Inf))

#mean prices per area.. 
num_v_area <- summaryBy(bk$SALE.PRICE_num~area_cat, data =bk , FUN = range)
ggplot(num_v_area, aes(num_v_area$area_cat, num_v_area$`bk$SALE.PRICE_num.FUN1`))+
  geom_bar(stat = "identity")+xlab("Area Category")+ ylab("No of Units")

#price distribution for each area category...
ggplot(bk, aes(SALE.PRICE_num,color = bk$area_cat, fill = bk$area_cat)) +
  geom_density(alpha=0.5) + xlim(10000,7500000) +xlab("Sale price disribution")

#prices by year sold 
ggplot(bk , aes(bk$year_sold,bk$SALE.PRICE_num))  + geom_bar(stat ="identity")

#linear model between price and area
lm_class<-lm(land.sqft~SALE.PRICE_num, data=bk)

ggplot(lm_class, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)+xlab("Sale Price")+ylab("Area")

par(cex=.8)
plot(SALE.PRICE_num,TOTAL.UNIT)
abline(lm_unit)






#linear model between no of units and area
lm_unit<-lm(SALE.PRICE_num~TOTAL.UNITS, data=bk)
ggplot(lm_unit, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)
#linear model of price and year built
lm_year<-lm(YEAR.BUILT~SALE.PRICE_num, data=bk)
ggplot(lm_year, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) + geom_point(aes(colour = factor(year_cat)))




