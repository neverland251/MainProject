library("cluster")
library("ggplot2")
library('dbscan')

row_names <- c("date","time","conzone","type","speed")

rawdata <- read.table("구간속도.csv",sep=",",stringsAsFactors = FALSE,encoding="utf-8",skip=1)

rawdata$V6 <- NULL

colnames(rawdata) <- row_names



## 두번 이상 실행필 필요 없ㅇ

times <- c(unique(rawdata$time))
dates <- c(unique(rawdata$date))
dates <- as.numeric(dates)
conzones <- c(unique(rawdata$conzone))

conzone_sep <- list()

for(i in conzones){
  temp <- rawdata[rawdata$conzone == i,c("speed","date","time")]
  conzone_sep[i] <- list(temp)
}

##

ggplot(rawdata[rawdata$conzone == conzones[[1]],],aes(x=speed, y=time)) + geom_point()
ggplot(rawdata[rawdata$conzone == conzones[[1]],],aes(x=speed, y=date)) + geom_point()

## 최적 군집을 도출하기 위한 갭 통계량 분ㅅ

gapAna <- clusGap(conzone_value[[1]],FUNcluster = kmeans,K.max = 24)

## K+1개 군집의 gap + 1표준편차의 범위 밖에 K개 군집의 GAP 통계량이 위치한경우, 그 군집이 최적의 군집수이다.
ggplot(as.data.frame(gapAna$Tab),aes(x=(1:nrow(gapAna[[1]])))) + geom_line(aes(y=gap),color="red") + geom_errorbar(aes(ymin=gap-SE.sim,ymax = gap+SE.sim),color="red") + geom_point(aes(y=gap),color="red")

library("cluster")
library("ggplot")

k_m <- kmeans(conzone_value[[1]],centers = 5)

## K_m$centers에 군집들의 평균값이 저장되어 있다.

gapAna_t <- clusGap(t(k_m$centers),FUNcluster = kmeans,K.max = 24)

ggplot(as.data.frame(gapAna$Tab),aes(x=(1:nrow(gapAna[[1]])))) + geom_line(aes(y=gap),color="red") + geom_errorbar(aes(ymin=gap-SE.sim,ymax = gap+SE.sim),color="red") + geom_point(aes(y=gap),color="red")

k_m_t <- kmeans(t(k_m$centers),centers = 5)
