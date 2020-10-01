library("ggplot2")
library("scales")
library("forecast")
library("TSA")
library("ggplot2")
library("dplyr")
library("reshape2")
library("magrittr")
library("lubridate")
library("tseries")


rawdata_NA <- na.omit(rawdata)

rawdata_NA_cor <-  cor(rawdata_NA)

rawdata_NA_cor_melt <- melt(rawdata_NA_cor)
rawdata_NA_cor_melt <- rawdata_NA_cor_melt[rawdata_NA_cor_melt$value != 1,]

temp <- rawdata_NA_cor_melt


for(i in seq(0,length(colnames(rawdata_NA)))){
  rawdata_NA_cor <- cor(rawdata_NA)
  temp <- melt(rawdata_NA_cor)
  temp <- temp[temp$value != 1,]
  temp <- temp[order(abs(temp$value),decreasing=T),]
  if(abs(temp[1,c("value")]) > 0.5){
    key1 = as.vector(temp[1,c("Var1")])
    key2 = as.vector(temp[1,c("Var2")])
    test_a <- mean(abs(rawdata_NA_cor_melt[rawdata_NA_cor_melt$Var1 == key1,c("value")]))
    test_b <- mean(abs(rawdata_NA_cor_melt[rawdata_NA_cor_melt$Var1 == key2,c("value")]))
    if(test_a > test_b){
      rawdata_NA <- rawdata_NA %>% select(-key1)
    }
    else{
      rawdata_NA <- rawdata_NA %>% select(-key2)
    }
  }  
  else{
    stop()
  }
}

