library(dplyr)
library(ggplot2)


options(scipen = 100)



rowdata <- read.csv("C://Users//23//Desktop//조류 예측//데이터//data_mart_final_V1.2.csv") 
rowdata <- rowdata %>% arrange(year.iso, week.iso)
################################################### 정규화 
rowdata_sc <- as.data.frame(scale(rowdata[,3:82]))
rowdata_sc <- bind_cols(rowdata[,1:2], rowdata_sc)

################################################### 종속변수와 각각의 독립변수와의 상관분석
### p.value 보기
cor_fun_p <- function(x){
  cor.test(rowdata_sc$cells_mean, x)$p.value
}
for(i in 1:82){
  print(cor_fun_p(rowdata_sc[,i]))
}

### 상관계수 보기
cor_fun_pearson <- function(x){
  cor.test(rowdata_sc$cells_mean, x)$estimate
}
for(i in 1:82){
  print(cor_fun_pearson(rowdata_sc[,i]))
}

################################################### 두개 이상의 변수 한번에 그리기(정규화)
library(ggplot2)
test <- rowdata_sc
test$week.iso <- ifelse(nchar(test$week.iso) == 2, test$week.iso, paste(0 ,test$week.iso, sep=""))
str(test)
test$time <- paste0(test$year.iso, test$week.iso)
ggplot(data = test, aes(x = time, y = level_lower_max)) + geom_line() + 
  geom_line(aes(x = time, y = rain_max), colour = "blue")

### 양의 상관계수가 높은 상위 4개
### 1번째, wt_temp_min 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = wt_temp_min, group = 1), colour = "springgreen", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("wt_temp_min 양1")

### 2번째, wt_temp_mean 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = wt_temp_mean, group = 1), colour = "green", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("wt_temp_mean 양2")

### 3번째, wt_temp_max 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = wt_temp_max, group = 1), colour = "greenyellow", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("wt_temp_max 양3")

### 4번째, temp_min 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = temp_min, group = 1), colour = "orange", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("temp_min 양4")


### 음의 상관계수가 높은 상위 3개
### 1번째, hPa_max 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = hPa_max, group = 1), colour = "hotpink", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("hPa_max 음1")

### 2번째, tn_min 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = tn_min, group = 1), colour = "purple", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("tn_min 음2")

### 3번째, tn_mean
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = tn_mean, group = 1), colour = "red", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("tn_mean 음3")


### 상관없는 
### 1번째, wt_temp_sd
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = wt_temp_sd, group = 1), colour = "royalblue", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("wt_temp_sd 상관없는1")


### 2번째, rain_min 
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = rain_min, group = 1), colour = "skyblue", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("rain_min 상관없는2")

### 3번째, ntu_sd
ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = ntu_sd, group = 1), colour = "deepskyblue", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("ntu_sd 상관없는3")

