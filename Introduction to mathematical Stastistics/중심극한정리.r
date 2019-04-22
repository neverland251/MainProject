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



# 2. x_bar가 X^2(50)인 분포에서 추출한 크기 100인 확률표본의 평균이라고 하자
# P(49<x<51)의 근삿값을 계산하시오
## 1) 통계량 x_bar를 CLT에 따라 N(0,1)을 따르는 확률변수로 변환해야한다.
## (X_bar-mu)/sigma*n^(1/2)에서, sigma를 알아야 하는데, X^2의 2차 적률을 구해 1차 적률을 빼면 편차를 구할 수 있다.
r <- 50

k <- 1
E_1 <- (2^(k)*gamma(r/2+k))/gamma(r/2)

k <- 2
E_2 <- (2^(k)*gamma(r/2+k))/gamma(r/2)

sigma <- (E_2-E_1)^(1/2)

## 2) 표준편차를 구했으므로, 이를 이용하여 통계량의 확률 구간 정의하면

boundary <- c((49-E_1)/sigma*100^(1/2),(51-E_1)/sigma*100^(1/2))
sum(c(pnorm(boundary[0]),pnorm(boundary[1])))


# 3. 1<x<infinite 일 때, f(x) = 1/x^2, else 0인 pdf를 따르는 분포에서 n=72인 확률표본을 선출했다.
# 이때, 50개 이상의 관측값이 3보다 작을 확률을 구하시오

## 1) Y = (72,p)인데, 이때 p는 X의 cdf에 종속되는 확률변수이다. X의 cdf를 구하면

f <- function(x){
  1/x^2
}
p <- integrate(f,1,3)
p <- p$value

## 이때, 베르누이 확률변수를 CLT에 의거하여 그 확률을 구하면

n = 72
a <- (50 - n*p)/(n*p*(1-p))^(1/2)
print(1 - pnorm(a))
