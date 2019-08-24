# 일원배치 분산분석

# 1. 분산이 동일하고, 평균이 각각 mu1, mu2, mu3인 정규분포에서 추출한 독립인 확률표본의 관측값들이 다음과 같이 주어졌다.

c1 <- c(5,9,6,8)
c2 <- c(11,13,10,12)
c3 <- c(10,6,9,9)

data <- data.frame(c1,c2,c3)

## 1) a = 0.05에서 평균의 동일성을 검증할 때, H0에 대한 H1을 채택하는지 기각하는지 결정하시오

### (1) 등분산에서의 F통계량을 정의하면

lower <- sum((c1-mean(c1))^2)+sum((c2-mean(c2))^2)+sum((c3-mean(c3))^2)
upper <- (length(c1)*(mean(c1)-sum(c1+c2+c3)/12)^2) + (length(c2)*(mean(c2)-sum(c1+c2+c3)/12)^2) + (length(c3)*(mean(c3)-sum(c1+c2+c3)/12)^2)

f_stat <- (upper*(length(c1)+length(c2)+length(c3)-3)) / (lower*(3-1))

### (2) 이 데이터는 F(9,2)를 따른다. 이때 상위 5% 기각점은

f_stat > qf(0.95,2,9)

### (3) 따라서, 가설 H0을 기각하지 못하고, 데이터의 평균은 동일하다는 결론을 내린다.

# 2. 분산이 동일하고, 평균이 각각 mu1, mu2, mu3인 정규분포에서 추출한 독립인 확률표본의 관측값들이 다음과 같이 주어졌다.

c1 <- c(38.7,39.2,40.1,38.9,NaN)
c2 <- c(41.9,42.3,41.3,NaN,NaN)
c3 <- c(40.8,41.2,39.5,38.9,40.3)

data <- data.frame(c1,c2,c3)

### (1) 등분산에서의 F통계량을 정의하면

lower <- sum((c1-mean(c1,na.rm=TRUE))^2,na.rm=TRUE)+sum((c2-mean(c2,na.rm=TRUE))^2,na.rm=TRUE)+sum((c3-mean(c3,na.rm=TRUE))^2,na.rm=TRUE)
upper <- (length(c1)*(mean(c1,na.rm=TRUE)-sum(data,na.rm=TRUE)/12)^2) + (length(c2)*(mean(c2,na.rm=TRUE)-sum(data,na.rm=TRUE)/12)^2) + (length(c3)*(mean(c3,na.rm=TRUE)-sum(data,na.rm=TRUE)/12)^2)

f_stat <- (upper*(length(c1)+length(c2)+length(c3)-3)) / (lower*(3-1))

### (2) 이 데이터는 F(2,9)를 따른다. 따라서

f_stat > qf(0.95,2,9)

### (3) H0를 기각하고 적어도 하나의 확률표본은 평균이 다르다는 결론을 도출한다.
# 분산분석

# 1. a = 3, b = 4인 이원배치 분류와 관련하여 다음과 같은 관측값이 주어졌을 때
## 열 평균의 동일성과 
## 행 평균의 동일성에 이용되는 F통계량을 계산하라

q1 <- data.frame(c(3.1,2.7,4.0),c(4.2,2.9,4.6),c(2.7,1.8,3.0),c(4.9,3.0,3.9))

##1) 2차 형식으로 표현하면, 열 평균의 검정에 사용되는 우도비 검정 통계량은
## (Q4/(b-1))/(Q5/(a(b-1))) 인 F통계량을 따른다
## 즉, 자유도 (b-1)과 자유도 (a(b-1))을 따르는 카이제곱검정량의 비율을 따른다.

## Q4는 sum(행)sum(열){열평균 - 전체평균}^2이고
## Q5는 sum(행)sum(열){관측값 - 열평균 - 행평균 + 전체평균)}^2 이다.
a <- length(rownames(q1))
b <- length(colnames(q1))
total <- a*b

## Q4에서, 이 함수는 행에 의존하지 않으므로 sum(행) -> length(행)으로 바뀌어서 곱해진다. 즉
Q4 <- a * sum((colMeans(q1)-(sum(q1)/total))^2)
sum_q5 <- rbind(data.frame(q1[1,]-colMeans(q1) - rowMeans(q1)),data.frame(q1[2,] - colMeans(q1) - rowMeans(q1)),data.frame(q1[3,]-colMeans(q1) - rowMeans(q1)))
Q5 <- sum((sum_q5 + (sum(q1)/total))^2)

(Q4/(b-1))/(Q5/((a-1)*(b-1)))

#2) 2차 형식으로 표현하면, 행 평균의 검정에 사용되는 우도비 검정 통계량은
## Q2(a-1)/Q5/(a(b-1)))인 F통계량을 따른다
## 즉, 자유도 (a-1)과 자유도(a(b-1))을 따르는 카이제곱검정량의 비율을 따른다.

