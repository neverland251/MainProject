# 중심극한정리
# 1. n = 100, p = 1/2인 b(100,1/2)에서 추출한 확률표본 Y = c(48,49,50,51,52)라고 가정하자.
# 이 때, P(Y)를 계산하시오.

## 1) 이산형 확률변수이므로 이는 본질적으로 P(47.5 < Y < 52.5)와 다르지 않다.
## 이 때, np = mu = 50, np(1-p) = 25 이므로, CLT를 이용하여 극한표준정규분포로 변환하면

Y <- rep(48:52)
np <- 100*(1/2)
np1_p <- (np*(1/2)

lower <- (47.5-np)/(np1_p)^(1/2)
upper <- (52.5-np)/(np1_p)^(1/2)

# 이때 모집단의 평균은 다음과 같은 방법으로 구하고
interval <- c((pnorm(lower,0,1)*np1_p)+np,pnorm(upper,0,1)*np1_p+np)
# 확률은 다음과 같은 방법으로 구한다.
interval_p <- c(pnorm(lower,0,1),pnorm(upper,0,1))
# 한편, CLT에 의해 b의 평균 np, 표준편차 np1_p는 정규분포에 근사한다는 것을 알았으므로, 균등분포에서 확률을 무작위 선출하여 pdf를 그려보면
plot(density(qnorm(runif(10),np,np1_p)))
plot(density(qnorm(runif(5000),np,np1_p)))
#이다.



      