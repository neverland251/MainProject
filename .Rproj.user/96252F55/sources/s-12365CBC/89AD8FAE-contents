# 1.(몬테카를로 시뮬레이션) 조건부 분포 f(x|y)와 사전분포 f(y)가 다음과 같이 제시되었다.
# f(x|y) = 2*exp(-2*y)
# f(y) = exp(-(x-y))
## 1) 조건부 분포와 사전분포를 이용하여 확률변수 x를 추출하시오
### (1) pdf에서 확률변수를 출력시키기 위해, 이들 pdf를 모두 역함수의 형태로 변환시키면
## f(x|y)^(-1) -> x = y - log(1 - p_x)
## f(y)^(-1) -> y = (1/2)*log(1 - p_y)

d <- c()

for(i in runif(100000,0,1)){
  a <- -(1/2)*log(1-i)
  b <- a - log(1-i)
  d <- c(d,b)
}

## 2) 도출된 샘플들의 평균, 표준편차를 구하라

mean(d)
var(d)^(1/2)

## 3) 도출된 샘플의 평균에 대한 95% 신뢰구간을 구하라

upper <- mean(d) - qnorm(0.025)*(sd(d)/length(d)^(1/2))
lower <- mean(d) + qnorm(0.025)*(sd(d)/length(d)^(1/2))
c(lower,upper)

# 2. (깁스샘플링) 조건부 분포 f(x|y)와 f(y|x)가 다음과 같이 주어졌다.
## f(y|x_i_1) 는 y^(a+x-1)*e^(-2y) 와 비레관계
## f(x|y) 는 e^(-y)*(y^(x)/factorial(x))

## 1) 6000개의 샘플을 깁스샘플링을 활용하여 추출하시오
### (1) f(y|x_i_1)은 gamma(a+x_i_1, 1/2)를 따른다. 

a <- 10
b <- c(1)
d <- c()
mean_iter_x <- c()
mean_iter_y <- c()
for(i in seq(1,6000)){
  temp <- rgamma(1,shape=a+b[i],scale=1/2)
  d[i] <- temp
  temp <- rpois(1,temp)
  b[i+1] <- temp
}

## 2) 3000개부터 6000개 사이의 샘플들 X,Y에 대한 평균과 분산을 추출하시오
### (1 - 1)Y에 대한 평균
mean_y <- (6000-3000)^(-1)*sum(b[3001:6000])
### (1 - 2)Y에 대한 분산
var_y <- var(d[3001:6000])
### (2 - 1)X에 대한 평균
mean_x <- (6000-3000)^(-1)*sum(d[3001:6000])
### (2 - 2)x에 대한 분산
var_x <- var(b[3001:6000])

c(mean_x,var_x,mean_y,var_y)

## 3) 2)에서 추출한 평균에 대한 95% 신뢰구간을 정하시오
### (1) 이들 평균들은 결론적으로 N(c,s)로 분포수렴한다. 이를 이용하여 X와 Y 각각에 대한 신뢰구간을 정하면
#### - Y에 대한 신뢰구간
upper <- mean_y - qnorm(0.025) * (var_y^(1/2)/length(b[3001:6000])^(1/2)) 
lower <- mean_y + qnorm(0.025) * (var_y^(1/2)/length(b[3001:6000])^(1/2)) 
c(lower,upper)

### - X에 대한 신뢰구간

upper <- mean_x - qnorm(0.025) * (var_x^(1/2)/length(d[3001:6000])^(1/2)) 
lower <- mean_x + qnorm(0.025) * (var_x^(1/2)/length(d[3001:6000])^(1/2))
c(lower,upper)

# 3. 표본이 N(a,b^2/n)의 정규븐포에서 추출되었고, 이 때 모수는 로지스틱 분포를 따른다고 알려졌다.

## 1) 사후 pdf를 정의하시오
### (1) 사후pdf는 사전 결합 pdf/예측 분포 pdf이다. 즉
#L(a,b^2/n) = 1/((1/(2*pi)^(1/2)*b)/(n^(1/2))) * exp((-1/2)*((y-a)^2/b^2/n))
#h(a) = (b^(-1)exp(-(a-k)/b))/(1+exp(-(a-k)/b))^2
#L(a,b^2/n)*h(a) 
###를 integrate(L(a,b^2/n)*h(a)) 로 나눈 값이 사후분포 k(a|x)가 된다.

## 2) 베이지안 추정해를 도출하시오. 손실함수는 MSE로 주어졌다.
### (1) 손실함수가 MSE로 주어졌을 때 베이지안 추정해는 사후분포의 기댓값이 된다. 즉, 이 값은 손실함수를 최소화한다.
### (2) 하지만, 로지스틱 분포는 대표적인 "닫힌 형태로 구할 수 없는 분포"이다. 따라서, 앞서 구한 사후분포 k(a|x)를 그대로 이용하면
#integrate(a * L(a,b^2/n) * h(a)) / integrate(L(a,b^2/n) * h(a))
#### 이를 예측 분포 pdf에 대한 기댓값으로 표현하면
#E(a*L(a,b^2/n)) / E(L(a,b^2/n)) #의 비율이다.

## 3) 베이지안 추정해를 MCMC로 구하시오

a <- 10
b <- 10
n <- 300

d <- c()
e <- c()

t_stat <- c()

### (1) (2 - 2)에서 도출한 베이지안 추정해의 공식을 알고리즘적 방식으로 구한다.
for(j in seq(0,300)){  
  for(i in runif(300,0,1)){
    temp <- a + b * log(i/(1-i))
    d <- c(d,rnorm(1,mean = temp, sd = b/n^(1/2)))
    e <- c(e,temp)
  }
  t_stat <- c(t_stat,(sum(e*d)/length(e))/sum(d/length(d)))
  print(c("iter",j,"complete"))
}

