library("ggplot2")
library("dplyr")
library("reshape2")
library("magrittr")
library("lubridate")
library("TSA")

rawdata <- read.csv("C://Users//23//Documents//GitHub//MainProject//Project//DataMining//일산 기온 내삽//result_hour.csv",sep=",",encoding="UTF-8",row.names=NULL)
rawdata_2 <- read.csv("C://Users//23//Documents//GitHub//MainProject//Project//DataMining//일산 기온 내삽//result_final.csv",sep=",",encoding="UTF-8",row.names=NULL)
rawdata_2_all <- rawdata_2
rawdata_all <- rawdata

colnames(rawdata_all) <- c("지점","일시","온도","강수량","풍속","풍향","습도","증기압","이슬점온도","현지기압","해면기압","일조","일사","적설","신적설","전운량","층하층운량","운형","최저운고","시정","지면상태","현상번호","지면온도","지중온도1",'지중온도2',"지중온도3","지중온도4")
colnames(rawdata_2_all) <- c("지점","일시","온도","강수량","풍향","풍속","현지기압","해면기압","습도","일사","일조")

rawdata_all <- rawdata_all %>% select(지점, 일시, 온도, 강수량, 풍속, 풍향, 습도, 현지기압, 해면기압, 일조, 일사)

#colnames(rawdata) <- c("site","date","temp","rainfall","windspeed","winddirection","humdity","vapor pressure","dew point","land pressure","ocean pressure","sunlight time","sunshine amount","cum snow","cum snow by 3hour","total cloud amount","cloud amount below middle","shape of cloud","minimum height of cloud","visiability","condition of land","num of phenomenom","temp of surface","temp amid land by 5cm","temp amid land by 10cm","temp amid land by 20cm","temp amid land by 30cm","dates")

colnames(rawdata_all) <- c("지점","일시","온도","강수량","풍속","풍향","습도","현지기압","해면기압","일조","일사")
colnames(rawdata_2_all) <- c("지점","일시","온도","강수량","풍향","풍속","현지기압","해면기압","습도","일사","일조")


rawdata_all$일시 <- as.POSIXct(rawdata_all$일시,format="%Y-%m-%d %H:%M")
rawdata_2_all$일시 <- as.POSIXct(rawdata_2_all$일시,format="%Y-%m-%d %H:%M")

rawdata_all <- rbind(rawdata_all,rawdata_2_all)

# 숫자를 세기 위해 1로만 가득차있는 변수를 생성
rawdata_all$one <- rep(1,length(rawdata_all[1]))

#### 시간, 장소 차원에서 샘플 수의 동일성을 검증


#데이터 구조

head(rawdata)
str(rawdata)

#4분위수 확인
summary(rawdata)

#결측값

## 비중확인
ratio <- colSums(is.na(rawdata_all)) / length(row(rawdata_all[1]))
colSums(is.na(rawdata)) / length(row(rawdata[1])) < 0.6
rawdata[c(as.data.frame((colSums(is.na(rawdata))/length(row(rawdata[1])) < 0.6))[, 1])]

## 타 변수와 상관관계 확인

rawdata_int <- rawdata[as.data.frame(sapply(rawdata,class)=="numeric")[,1]]
rawdata_int <- rawdata_int %>% select(-풍향)

rawdata_int_NA <- apply(sapply(rawdata_int,is.na),2,as.integer)
ggplot(melt(cor(rawdata_int_NA)), aes(x = Var1, y = Var2)) + geom_tile(aes(fill = value)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "white", high = "red")

rawdata_int_NA <- rawdata_int[c(as.data.frame((colSums(is.na(rawdata_int))/length(row(rawdata_int[1])) < 0.6))[,1])]
rawdata_int_NA <- apply(sapply(rawdata_int_NA,is.na),2,as.integer)
ggplot(melt(cor(rawdata_int_NA)), aes(x  = Var1, y = Var2, fill = value)) + geom_tile() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "green", high = "purple")

rawdata_all_NA <- apply(sapply(rawdata_all, is.na), 2, as.integer)
ggplot(melt(cor(rawdata_all_NA)), aes(x = Var1, y = Var2)) + geom_tile(aes(fill = value)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "white", high = "red")


### 1.  결측값의 카이제곱 검정

### 습도,중기압,이슬점온도
### 현지기압,해면기압의 결측 상관성이 존재하는 것으로 보인다(>0.5)
### 과연 결측값은 관측소에 의존하는가, 아니면 시간에 의존하는가? -> 카이스퀘어 검정 시행
#### H0 : 결측값은 시간/장소라는 집단에 대하여 독립적이다.
#### H1 : 결측값은 시간/장소라는 집단에 대하여 독립적이지 않다.(변화한다.)


## 강수량 결측 패턴 확인

# 결측값을 자동으로 시간으로 묶어주는 함수 작성

groupby_time <- function(x){
  rawdata_all_NA <- apply(sapply(x, is.na), 2, as.integer)
  rawdata_all_NA <- as.data.frame(rawdata_all_NA) %>% select(-지점,-일시,-one)
  rawdata_all_NA <- as.data.frame(cbind("지점" = rawdata_all$지점,rawdata_all_NA))
  rawdata_all_NA <- as.data.frame(cbind("일시" = rawdata_all$일시,rawdata_all_NA))
  rawdata_all_NA$일시 <- as.POSIXct(rawdata_all$일시,format="%Y-%m-%d %H:%M")
  
  rawdata_all_NA_groupby_time <- rowsum(rawdata_all_NA, hour(rawdata_all_NA$"일시"))
  
  return(rawdata_all_NA_groupby_time)
  
  # 결측값을 자동으로 지점으로 묶어주는 함수
}
groupby_site <- function(x){
  rawdata_all_NA <- apply(sapply(x, is.na), 2, as.integer)
  rawdata_all_NA <- as.data.frame(rawdata_all_NA) %>% select(-지점,-일시,-one)
  rawdata_all_NA <- as.data.frame(cbind("지점" = rawdata_all$지점,rawdata_all_NA))
  rawdata_all_NA <- as.data.frame(cbind("일시" = rawdata_all$일시,rawdata_all_NA))
  rawdata_all_NA$일시 <- as.POSIXct(rawdata_all$일시,format="%Y-%m-%d %H:%M")
  
  rawdata_all_NA_groupby_site <- rowsum(rawdata_all_NA, rawdata_all_NA$"지점")
  
  return(rawdata_all_NA_groupby_site)
  
}


##### 1) 검정 결과, 매우 특이하게도 시간과 관측소 모두에서 강수량의 결측값과 연관이 있는 것으로 판단된다.
###### (1) 그 이유는 아래서 서술할 187번 관측소와
#####3 (2) 모든 관측소들이 동일한 타임 포인트를 공유하지 않기 때문에 발생한다.=

chisq_only_p <- function(x){
  a <- chisq.test(x)
  return(sprintf("%.5f",a$p.value))
}

chisq_time <- function(x){
  temp_groupby_time <- groupby_time(x)
  return(apply(temp_groupby_time,2,chisq_only_p))
}

chisq_site <- function(x){
  temp_groupby_site <- groupby_site(x)
  return(apply(temp_groupby_site,2,chisq_only_p))
}

options(scipen=10)
k <- chisq_time(rawdata_all)

rawdata_all_NA_groupby_time <- groupby_time(rawdata_all)

rawdata_all_NA_groupby_site <- groupby_site(rawdata_all)

#rawdata_all_NA_anova_site <-
#  rowsum(rawdata_all_NA[year(rawdata_all_NA$일시) == 2015, ], rawdata_all_NA[year(rawdata_all_NA$일시) == 2015, c("V1")])


### 결측값 데이터셋의 일시에 따른 강수량 패턴

x_labels <- as.integer(rownames(rawdata_all_NA_groupby_site))
ggplot(rawdata_all_NA_groupby_site, aes(x = x_labels, y =  일조)) + geom_bar(stat = "identity") +   scale_x_continuous("ID", x_labels, breaks = x_labels) +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

x_labels <- as.integer(rownames(rawdata_all_NA_groupby_time))
ggplot(rawdata_all_NA_groupby_time, aes(x = x_labels, y = 일조)) + geom_bar(stat = "identity") +   scale_x_continuous("ID", x_labels, breaks = x_labels) +  theme(axis.text.x = element_text(angle = 90, hjust = 2))

###### 3시간 간격으로 결측값이 급격히 줄어드는 양상을 볼 수 있다.
###### 따라서 만약 3시간 간격으로 양쪽이 결측값이 아닐경우, 그 평균값으로 대체한다.

time_when_not_null <- hour(rawdata_all$"일시") == 3 & is.na(rawdata_all$강수량) == FALSE
time_when_not_null <- rawdata_all[time_when_not_null,]
rawdata_all[rawdata_all$일시 == time_when_not_null,c("강수량")]


### 2. 시간 포인트 동일성에 대한 검증

##### 1) 강수량의 경우, 시간 차원과 지점 차원 모두 결측값과 밀접한 연관이 있다.
###### (1) 187번 관측소 문제 : 다른 관측소와 달리, 187번은 단 4개의 데이터 포인트만 가지고 있다.
###### (2) 따라서, 187번은 삭제한다.

rawdata_all <- rawdata_all[rawdata_all$지점 != 187,] 


##### 2) 지점별 데이터들의 groupby sum
sum_site <- rowsum(rawdata_all$one, rawdata_all$지점)
sum_site <- cbind("site" = rownames(sum_site), "value" = sum_site)

sum_site <- as.data.frame(sum_site)
sum_site$one <- rep(1,length(sum_site[1]))

rowsum(sum_site$one, sum_site$V2)

##### 3) 시간별 데이터들의 groupby sum
sum_time <- rowsum(rawdata_all$one, rawdata_all$일시)
sum_time <- cbind("date" = rownames(sum_time),"value" = sum_time)

sum_time <- as.data.frame(sum_time)
sum_time$one <- rep(1,length(sum_time[1]))


###### (1) 24시간씩, 365일 수집했을 때, 2015 ~ 2018년 4년간 기대되는 수집 포인트는 

rowsum(sum_site$one, sum_site$V2)

site_for_searching_oversample <- as.integer(rownames(rowsum(sum_site$one, sum_site$V2)))


for(i in site_for_searching_oversample){
  print(i)
  print(as.vector(sum_site[sum_site$V2 == i,c("site")]))
}

24*365*4 #35040
이다.



###### (2) 가령, 최대치인 35064개의 시간 포인트를 갖고 있는 지점을 도출해내면

sum_site[sum_site$V2 == 35064,] #108,143

temp <- rawdata_all[rawdata_all$지점 == 108,]

####### - 중복값이 있는 걸까?

summary(duplicated(temp$일시)) # 전부 FALSE이다.

####### - 정각 이외의 다른 포인트가 있는걸까?

summary(minute(temp$일시) != 00)

####### - 범인은 윤달이었다.

for(i in seq(2015,2018)){
  print(unique(day(temp[year(temp$일시) == i & month(temp$일시) == 2,c("일시")])))
}

for(i in seq(2015,2018)){
  temp 
  print(unique(day(temp[year(temp$일시) == i & month(temp$일시) == 2,c("일시")])))
}

unique(rawdata_all[month(rawdata_all$일시) == 2 & day(rawdata_all$일시) == 29,c("지점")])

####### - 2016년은 윤달이 있던 해로, 2월 29일의 24시간이 추가된다.
####### - 다른 년도와의 동일성을 유지하기 위해, 2016년 2월 29일은 삭제해준다.

temp <- rawdata_all[month(rawdata_all$일시) != 2,]
temp_for_month <- rawdata_all[month(rawdata_all$일시) == 2 & day(rawdata_all$일시) != 29, ]
rawdata_all <- rbind(temp,temp_for_month)

##### 4) 각 시간별 가동중이었던 지점의 총합 도출
rowsum(sum_time$one, sum_time$V2)
###### (1) 즉, 다시말해, 95가 18947이란 말은 지점을 95개 관측소가 가동되어있던 시간대가 18947점 있다는 의미이다.
###### (2) 지점 91개 미만의 지점만 가동되었던 시간대가 매우 소수(1~10) 존재한다.

sum_time[sum_time$V2 != 95 & sum_time$V2 != 94 & sum_time$V2 != 93 & sum_time$V2 != 92,c("date","V2")]
###### (3) 확인 해보면, 30분 대가 포함되어있는 '소수 시간대'거나, 이유는 알 수 없지만 평균 96에 미달하는
###### 89~91개의 관측소만 가동되었던 시간대들이 포함되어 있음을 알 수 있다.

##2##### - 일단, 30분대의 시간대는 신경망 공급시 오류가 발생할 수 있으므로 제거해준다.

rawdata_all <- rawdata_all[minute(rawdata_all$일시) != 30,]

####### -  그 외에, 30분대가 아니지만 동시 가동 관측소가 적었던 시간대는 다른 방법을 강구한다.

sum_time$YMD <- as.Date(sum_time[,c("date")])
rowsum(sum_time[sum_time$V2 == 92, c("one")],sum_time[sum_time$V2 == 92,c("YMD")])

###### - 각각의 날짜에 대하여 동시가동 관측소가 92개였던 날짜의 시간대를 보면 


##### 5) 3개 요소의 삭제 후 재정렬

rawdata_all$one <- rep(1, length(rawdata_all[1]))

###### (1) 지점별 데이터들의 groupby sum
sum_site <- rowsum(rawdata_all$one, rawdata_all$지점)
sum_site <- cbind("site" = rownames(sum_site), "value" = sum_site)

sum_site <- as.data.frame(sum_site)
sum_site$one <- rep(1, length(sum_site[1]))

rowsum(sum_site$one, sum_site$V2)

rowsum(sum_site$one, sum_site$V2)

site_for_searching_oversample <-
  as.integer(rownames(rowsum(sum_site$one, sum_site$V2)))


for (i in site_for_searching_oversample) {
  print(i)
  print(as.vector(sum_site[sum_site$V2 == i, c("site")]))
}

###### - 93번, 177번 관측소의 시점 포인트가 매우 적은것을 확인할 수 있다.

for(i in c(93,177)){
  temp <- rawdata_all[rawdata_all$지점 == i, c('일시')]
  print(min(temp))
}

###### - 확인 결과, 이 관측소들의 최초 가동 시점은 2015년 1월 1일이 아닌, 그 이후이다.

rawdata_a <- rawdata
rawdata_a$일시 <- as.POSIXct(rawdata_a$일시,format="%Y-%m-%d %H:%M")

for(i in unique(rawdata_a$지점)){
  temp <- rawdata_a[rawdata_a$지점 == i, c('일시')]
  print(i)
  print(min(temp)) 
}

###### - 그 외의 다른 관측소들은 2015년 1월 1일부터 가동을 시작했다.

###### - 35040 * 2의 데이터를 지키기 위해 35040 * 94의 데이터를 포기하는것은 어불성설이므로, 두 관측소의
######   데이터는 포기한다.

rawdata_all <- rawdata_all[rawdata_all$지점 != 93, ]
rawdata_all <- rawdata_all[rawdata_all$지점 != 177, ]

###### - 4개 요소 삭제 후 재정렬

rawdata_all$one <- rep(1, length(rawdata_all[1]))

###### (1) 지점별 데이터들의 groupby sum
sum_site <- rowsum(rawdata_all$one, rawdata_all$지점)
sum_site <- cbind("site" = rownames(sum_site), "value" = sum_site)

sum_site <- as.data.frame(sum_site)
sum_site$one <- rep(1, length(sum_site[1]))

rowsum(sum_site$one, sum_site$V2)

site_for_searching_oversample <-
  as.integer(rownames(rowsum(sum_site$one, sum_site$V2)))


for (i in site_for_searching_oversample) {
  print(i)
  print(as.vector(sum_site[sum_site$V2 == i, c("site")]))
}


##### 6) 관측소별 누락된 시점 포인트 도출
##### (1) 35040이 모든 데이터지점이 포함된 완전한 데이터셋이므로, 이 데이터셋을 기준으로 해서 각 관측소의 값을 뺀다.

full_time_point <- rawdata_all[rawdata_all$지점 == 108,c("일시")]
full_time_point <- as.POSIXct(full_time_point)

eliminator <- function(x) {
  # 비교를 위해 각 관측소별 개별 일시 데이터를 임시로 만들어준다.
  temp_list <- rawdata_all[rawdata_all$지점 == x, c("일시")]
  for (j in temp_list) {
    #완전한 시점을 저장하고 있는 리스트에서 관측소별 개별 일시 데이터를 뺀
    full_time_point <- full_time_point[full_time_point != j]
  }
  return(full_time_point)
}

null_time_point <- lapply(unique(rawdata_all$지점),eliminator)

## 비중확인
ratio <- colSums(is.na(rawdata_all)) / length(row(rawdata_all[1]))
colSums(is.na(rawdata_all)) / length(row(rawdata_all[1])) < 0.6
rawdata_all[c(as.data.frame((colSums(is.na(rawdata_all))/length(row(rawdata_all[1])) < 0.6))[, 1])]

###### (2) 시간별 데이터들의 groupby sum
sum_time <- rowsum(rawdata_all$one, rawdata_all$일시)
sum_time <- cbind("date" = rownames(sum_time), "value" = sum_time)

sum_time <- as.data.frame(sum_time)
sum_time$one <- rep(1, length(sum_time[1]))

rowsum(sum_time$one, sum_time$V2)

# 다중공선성 확인

rawdata_int_cor <- cor(rawdata_int)
ggplot(melt(rawdata_int_cor),aes(x=Var1,y=Var2,fill=value)) + geom_tile()

# 이상치 확인

ggplot(rawdata_int,aes(y=colnames(rawdata_int))) + geom_boxplot()
melt(rawdata_int)

colnames(rawdata_int[1])
