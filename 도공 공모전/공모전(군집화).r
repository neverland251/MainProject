library("cluster")
library("ggplot2")
library('dbscan')
library("corrplot")
library("mnormt")

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
## K+1개 군집의 gap + 1표준편차의 범위 밖에 K개 군집의 GAP 통계량이 위치한경우, 그 군집이 최적의 군집수이다.

### 열을 기준으로 평균 압출

gapAna <- clusGap(conzone_value[[1]],FUNcluster = kmeans,K.max = 24)

ggplot(as.data.frame(gapAna$Tab),aes(x=(1:nrow(gapAna[[1]])))) + geom_line(aes(y=gap),color="red") + geom_errorbar(aes(ymin=gap-SE.sim,ymax = gap+SE.sim),color="red") + geom_point(aes(y=gap),color="red")음

k_m <- kmeans(conzone_value[[1]],centers = 5)

## 압축된 열평균을 다시 행으로 압축

gapAna_t <- clusGap(t(k_m$centers),FUNcluster = kmeans,K.max = 24)

ggplot(as.data.frame(gapAna_t$Tab),aes(x=(1:nrow(gapAna[[1]])))) + geom_line(aes(y=gap),color="red") + geom_errorbar(aes(ymin=gap-SE.sim,ymax = gap+SE.sim),color="red") + geom_point(aes(y=gap),color="red")

k_m_t <- kmeans(t(k_m$centers),centers = 3)

## 압축된 열평균을 사용하지 않고, 행만 따로 분리하여 압축 

gapAna_t_t <- clusGap(t(conzone_value[[1]]),FUNcluster = kmeans,K.max = 24)

ggplot(as.data.frame(gapAna_t_t$Tab),aes(x=(1:nrow(gapAna[[1]])))) + geom_line(aes(y=gap),color="red") + geom_errorbar(aes(ymin=gap-SE.sim,ymax = gap+SE.sim),color="red") + geom_point(aes(y=gap),color="red")

k_m_t_t <- kmeans(t(conzone_value[[1]]),centers = 3)

## 분산 병합
## 각각을 독립인 확률변수라고 가정한다면, 동일 평균에서 추출한 서로 다른 분산들의 결합 분포라고 생각할 수 있다.
## 이 때, 이 평균은 N(mean(x), var(x)/length(x))의 분포를 따르므로, 우선 공분산을 구하는 것이 우선시 된다.

# 상관행렬을 그린다.

test_1_1 <- t(conzone_value[[1]][names(k_m$cluster[k_m$cluster == 1]),names(k_m_t$cluster[k_m_t$cluster == 1])])
test_2_2 <- t(conzone_value[[1]][names(k_m$cluster[k_m$cluster == 2]),names(k_m_t$cluster[k_m_t$cluster == 2])])
test_4_4 <- t(conzone_value[[1]][names(k_m$cluster[k_m$cluster == 3]),names(k_m_t$cluster[k_m_t$cluster == 3])])

cor_1 <- cor(test_1_1)
cor_2 <- cor(test_2_2)
cor_4 <- cor(test_4_4)

corrplot(cor_1)
corrplot(cor_2)
corrplot(cor_4)

## 공분산 행렬을 저장한다.

conzone_1_cov <- cov(test_1_1)
conzone_2_cov <- cov(test_2_2)
conzone_4_cov <- cov(test_4_4)
## 공분산 행렬의 행렬식 값을 구하면

## 표본평균 행렬 도출

### k_m_t는 평균을 또 평균으로 압축한 기댓값을
### k_m_t_t는 열의 평균, 행의 평균을 따로 클러스터링 한 후 같은 클러스터링 그룹명끼리 묶은 결과를 뜻한다.

var_mean_t <- data.frame()
var_mean_t_t <- data.frame()
for(i in rep(1:5)){
  for(j in rep(1:3)){
    var_mean_t[i,j] <- mean(t(conzone_value[[1]][names(k_m$cluster[k_m$cluster == i]),names(k_m_t$cluster[k_m_t$cluster == j])]))
    var_mean_t_t[i,j] <- mean(t(conzone_value[[1]][names(k_m$cluster[k_m$cluster == i]),names(k_m_t_t$cluster[k_m_t_t$cluster == j])]))
  }
}

## 추정된 평균과 실제 평균의 차
### 이 갭이 작으면 작을수록 우수한 통계량을 나타낸다
### 갭이 작은 k-means 군집의 갯수롤 알고리즘 방법으로 도출한다.

mean(t(k_m_t$centers - var_mean_t))
mean(t(k_m_t$centers - var_mean_t_t))

# cdf 추정

a <- c()
b <- c()
c <- runif(10000,0,100)

for(i in c){
  a <- pmnorm(i,mean=k_m_t$centers[1],varcov=conzone_1_cov)
  b <- rbind(b,a[[1]])
}

plot(c,(1-b))
