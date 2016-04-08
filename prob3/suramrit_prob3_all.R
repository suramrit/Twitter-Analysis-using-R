library("doBy")
library(ggplot2)
library(plotrix)

temp <- list.files(path ="C:\\Users\\suramrit\\Desktop\\DataIntensiveComputing\\dds_datasets\\dds_ch2_rollingsales",pattern="*.xls",full.names=TRUE)
borough_data<-data.frame()

total_sale <- data.frame(borough = factor(), total_sales = factor())
max_sale <- data.frame(borough = factor(), max_sales = factor())
median_year<- data.frame(borough = factor(), year_sold = factor())
median_price<- data.frame(borough = factor(), price = factor())
price_v_area <- data.frame(borough=factor(), price = factor(), area= factor())
num_sale <- price_v_area <- data.frame(borough=factor(), no_sales = factor())

max <- function(x){c(length(x))}
range <- function(x){c(length(x), median(x), mean(x), max(x))}
len <- function(x){c(length(x))}
sum <- function(x){c(sum(x))}

#storing as separate data frames as well as a combined data frame
for (i in 1:length(temp)){
  
  file_name = paste0(c('borough','_',i),collapse = "")
  #assign(file_name, read.xls(temp[i]))
  
  temp2 <- get(file_name)
  
  temp2$SALE.PRICE_num <- as.numeric(gsub("[^[:digit:]]","",temp2$SALE.PRICE))
  temp2$GROSS.sqft <- as.numeric(gsub("[^[:digit:]]","",temp2$GROSS.SQUARE.FEET))
  temp2$land.sqft <- as.numeric(gsub("[^[:digit:]]","",temp2$LAND.SQUARE.FEET))
  temp2$SALE.DATE <- as.Date(temp2$SALE.DATE)
  
  temp2$year_cat<- cut(temp2$YEAR.BUILT,c(-Inf,1800,1850,1900,1950,2000,2010,Inf))
  temp2$year_sold<- as.numeric(format(temp2$SALE.DATE,'%Y'))
  temp2$year_sold_cat<- cut(temp2$year_sold,c(-Inf,2010,2011,2012,2013,Inf))
  temp2$class_cat<- gsub("[^[[:alpha:]|[:space:]]","",temp2$BUILDING.CLASS.CATEGORY)
  
  #borough_data <- rbind(borough_data,temp2)
  num_sale <- rbind(num_sale, data.frame(borough = file_name, no_sales=base::nrow(subset(temp2, temp2$SALE.PRICE_num>0))))
  total_sale<- rbind (total_sale, data.frame(borough = file_name,total_sales=base::sum(temp2$SALE.PRICE_num)))
  max_sale<- rbind (max_sale, data.frame(borough = file_name,max_sale=base::max(temp2$SALE.PRICE_num)))
  median_year<- rbind(median_year, data.frame(borough= file_name , year_built = median(temp2$YEAR.BUILT)))
  price_v_area <- rbind(price_v_area, data.frame(borough= file_name, price = mean(temp2$SALE.PRICE_num), area = base::
                                                   mean(temp2$GROSS.sqft)))
  median_price <- rbind(median_price, data.frame(borough= file_name, price = stats::median(temp2$SALE.PRICE_num)))

}
attach(borough_data)
#median prices for borough 
ggplot(median_price, aes(median_price$borough, median_price$price))+geom_bar(stat = "identity")+xlab("Borough")+ylab("Median Price")

#total sales for borough
ggplot(total_sale, aes(total_sale$borough, total_sale$total_sales))+geom_bar(stat = "identity")+xlab("Borough")+ylab("Total Sales")

#max price per borough
max_sale2 <- max_sale
ggplot(max_sale2, aes(max_sale2$borough, max_sale2$max_sale))+geom_bar(stat = "identity")+xlab("Borough")+ylab("Max Sale Price")

#median year of property for each borough
ggplot(median_year, aes(median_year$borough, median_year$year_built, color = median_year$year_built))+
  geom_point() + geom_smooth()+xlab("Borough")+ylab("Median Built Year")

#total area vs price for each borough 

ggplot(price_v_area, aes(price_v_area$price,price_v_area$area, color= price_v_area$borough , fill =
                           price_v_area$borough)) +geom_point() + geom_smooth(lm)
ggplot(price_v_area, aes(price_v_area$price,price_v_area$area)) +geom_point() + geom_smooth(method = "lm")

#try finding relation between price and 



