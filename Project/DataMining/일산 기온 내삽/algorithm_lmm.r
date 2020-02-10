library("ggplot2")
library("dplyr")
library("reshape2")
library("magrittr")
library("lubridate")
library("TSA")

tol <- 0.01

temp_name <- colnames(rawdata_all %>% select(-지점,-일시,-one))

iter_name <- c(temp_name,paste(temp_name,paste("_ 1")))

for(i in seq(2,20)){
  iter_name <- c(iter_name,paste(temp_name,paste(c("_"),as.character(i))))
}

iter_name <- c("지점","일시",iter_name)

temp_final <- setNames(data.frame(matrix(ncol = length(iter_name), nrow = 0)), iter_name)

k = 20 + 1

for(j in c(108,119,112)){
  print(j)
  site_time <- rawdata_all[rawdata_all$지점 == j,c("지점","일시")]
  temp <- rawdata_all[rawdata_all$지점== j,] %>% select(-지점,-일시,-one)
  temp_2 <- cbind(temp[2 : length(rownames(temp)), ] , temp[1 : length(rownames(temp)) - 1 ,])
  for(i in seq(3,k)){
    temp_2 <- cbind(temp[i : length(rownames(temp)) ,], temp_2[1 : (length(rownames(temp_2))) - 1 , ])
  }
  print("ok")
  temp <- cbind(site_time[k : length(rownames(temp)) , ] , temp_2)
  colnames(temp) <- iter_name
  temp_final <- rbind(temp_final,temp)
}  

temp_data <- temp_final[month(temp_final$일시) == 1 & day(temp_final$일시) == i & hour(temp_final$일시) == j, ]

temp_for_day <- function(x){
  temp_data <- temp_final[month(temp_final$일시) == 1 & day(temp_final$일시) == x , ]
  return(temp_data)
}

temp_for_day <- function(x){
  hour <- unique(hour(rawdata_all$일시))
  temp_data <- lapply(hour,temp_for_hour)
}

for(k in unique(temp_final$지점)) {
  temp <- temp_final[temp_final$지점  == k & month(temp_final$일시) == 1, ]
  for (i in c(1, 2)) {
    temp_2 <- temp[day(temp$일시) == i, ]
    for (j in seq(0:23)) {
      temp_3 <- temp_2[]
    }
  }
}

temp_for_test <- temp_final[temp_final$지점 == 108 & month(temp_final$일시) == 1 & day(temp_final$일시) == 1,]

lapply(seq(0:23),temp_for_test[hour(temp_for_test == x)])
