## Forecast
## Forecast using different machine learning method on Scotts dataset
## zihao.zhang@analyticpartners.com
## have fun



setwd("C:/Users/zihao.zhang/Desktop")


target_name = "Lawn.Fertilizer.Sales.Units"
SampleSize = 1


#########################################
#######    Data Processing     #########
#########################################


scotts = read.csv("Modeling Reference File_V2.csv")

scotts$target = scotts[,target_name]
scotts[,target_name]= NULL
names(scotts)



names(scotts)
library(lubridate)
library(dplyr)
library(ggplot2)
for(i in 4:dim(scotts)[2]){scotts[,i] = as.numeric(scotts[,i])}

names(scotts)
summary(scotts$target)

scotts$Week.Ending.Sunday <- as.factor(scotts$Week.Ending.Sunday)
a = scotts %>% group_by(Week.Ending.Sunday) %>% summarise(sales = sum(target)) 
a$Date = as.POSIXct(a$Week.Ending.Sunday, format = "%m/%d/%y")
a$data = "training"
a$data[a$Date>"2016-06-25"] = "test" 
a %>% ggplot(aes(Date,sales,colour = data)) + geom_line()


scotts$Week.Ending.Sunday <- as.POSIXct(scotts$Week.Ending.Sunday, format = "%m/%d/%y")
scotts$month = month(scotts$Week.Ending.Sunday ) 
scotts$day = day(scotts$Week.Ending.Sunday ) 
scotts$year = year(scotts$Week.Ending.Sunday ) 
names(scotts)


scotts_dimension = scotts[,c('Week.Ending.Sunday','DMA','Retailer')]

library(ade4)
library(data.table)
ohe_feats = c('month','day','DMA','Retailer')


for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(scotts[f])
  scotts[f] = NULL
  scotts = cbind(scotts, df_all_dummy)
}

scotts$Date=NULL
dim(scotts)


names(scotts) <- gsub("-", "", names(scotts), fixed = TRUE)
names(scotts) <- gsub(",", "", names(scotts), fixed = TRUE)
names(scotts) <- gsub(" ", "", names(scotts), fixed = TRUE)
names(scotts) <- gsub("(", "", names(scotts), fixed = TRUE)
names(scotts) <- gsub(")", "", names(scotts), fixed = TRUE)
names(scotts) <- gsub("&", "", names(scotts), fixed = TRUE)
