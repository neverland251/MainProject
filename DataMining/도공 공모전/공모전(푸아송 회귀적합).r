conzone_sep[1]
## 연도별 각 고속도로 사망자수
highway_name <- c("경부","서해안","호남","중부내륙","영동")
death_2017 <- c(37,23,23,25,24)
death_2016 <- c(53,28,14,29,15)
death_2015 <- c(58,14,20,20,17)
death_2014 <- c(51,25,27,26,13)
death_2013 <- c(58,33,20,21,24)
death_2012 <- c(70,25,14,23,34)
death_data <- data.frame(death_2017,death_2016,death_2015,death_2014,death_2013,death_2012,row.names=NULL)
rownames(death_data) <- highway_name
colnames(death_data) <- seq(7,2)

## 실업률
unemployment_2017 <- c(rep(3.7,5))
unemployment_2016 <- c(rep(3.7,5))
unemployment_2015 <- c(rep(3.6,5))
unemployment_2014 <- c(rep(3.5,5))
unemployment_2013 <- c(rep(3.1,5))
unemployment_2012 <- c(rep(3.2,5))
unemployment <- data.frame(unemployment_2017,unemployment_2016,unemployment_2015,unemployment_2014,unemployment_2013,unemployment_2012)
rownames(unemployment) <- highway_name
colnames(unemployment) <- seq(7,2)

## 평균속도
rawdata_mean <- read.csv("C://Users//never//Workplace//공모전//도로공사//속도데이터-평균.csv")
rawdata_mean <- rawdata_mean[1:6,2:6]
rawdata_mean$year <- seq(2,7)

## 총주행거리(VKT)
rawdata_VKT <- read.csv("C://Users//never//Workplace//공모전//도로공사//총주행거리.csv",encoding="UTF-8",sep=",")
rownames(rawdata_VKT) <- rawdata_VKT$X
rawdata_VKT <- rawdata_VKT[2:7]
colnames(rawdata_VKT) <- seq(2,7)



library("reshape2")

## 데이터셋을 녹여서 세로로 긴 형식으로 데이터 구조를 변환한다.

mean_melt <- melt(rawdata_mean,id = "year",value.name = "speed")
colnames(mean_melt) <- c("year","name","speed")

death_melt <- melt(t(death_data),value.name = "death")
colnames(death_melt) <- c("year","name","death")

VKT_melt <- melt(t(rawdata_VKT),value.name="VKT")
colnames(VKT_melt) <- c("year","name","VKT")

unemploy_melt <- melt(t(unemployment),value.name="unemploy")
colnames(unemploy_melt) <- c("year","name","unemploy")

data <- merge(mean_melt,death_melt,by=c("name","year"))
data <- merge(data,VKT_melt,by=c("name","year"))
#data <- merge(data,unemploy_melt,by=c("name","year"))

## 사망자수는 대략적으로 푸아송 분포를 따르는 것으로 보인다.

plot(data$death)

formul <- death~speed+year+offset(log(VKT))
#+unemploy

## 푸아송 회귀 모델 합적합
### 일반 모델 적ㅎ
fitted_model <- glm(formul,family=poisson,data=data)
### 과산포 여부를 확인하기 위한 quasipoisson 회귀 적ㅎ
fitted_model_od <- glm(formul,family=quasipoisson,data=data)

#과산포 검정

h <- sum(((as.numeric(data$death) - fitted_model$fitted.values)/sqrt(fitted_model$fitted.values))^2)/fitted_model$df.residual
pchisq(h, fitted_model$df.residual)

# 과산포가 존재하므로, 과산포 변수를 조정한 값을 최종적으로 도출한다.

sum_fitted <- summary(fitted_model,dispersion=h)

# 과속 계수의 오즈비 도출수
exp(sum_fitted$coefficients[2]) - 1
