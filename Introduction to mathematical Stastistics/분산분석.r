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

