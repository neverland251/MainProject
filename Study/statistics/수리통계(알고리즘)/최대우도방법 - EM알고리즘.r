# EM 알고리즘

#1. 어떤 확률변수의 다항 모형이 범주 C1,C2,C3,C4로 구분되어 있고, 각각의 빈도가 x1,x2,x3,x4일 때 각각의 확률이
# C1 = 1/2+1/4c, C2 = 1/4-1/4c, C3 = 1/4-1/4c, C4 = 1/4c 일때
# 실험 조건을 변경하여 C1를 C11 = 1/2, C12 = 1/4c로 분할하고, 각각의 빈도를 알 수 없는 확률변수 Z11과 Z12로 두자.
# 이 때, 결측값 Z11, Z12를 고려하여 c의 mle를 추정하시오

## 1) 결측값으로만 구성된 주변 pdf f(z11,z12|c2,c3,c4,c)를 구하기 위해선 
## 우선 결측값까지 포함한 완전한 형태(Full-shaped)의 pdf f(z11,z12,c2,c3,c4|c)와 
## 관측값으로만 구성된 주변 pdf f(c2,c3,c4|c)를 구해야 한다.
## pdf f(z11,z12,c2,c3,c4|c)는

numb <- factorial(n)/(factorial(x[0])*factorial(x[1])*factorial(x[2])*factorial(x[3])*factorial(x[4])
param <- 2^(x[0])*c^(x[1])*(1-c)^(x[2])*(1-c)^(x[3])*c^(x[4])

likelyhood_function <- numb * param

##Full-shaped pdf를 주변 pdf f(c2,c3,c4|c)로 나눈 비율인 <z만으로 구성된 조건부 pdf>는

numb_c <- factorial(x[0]+x[1])/factorial(x[0])*factorial(x[1])
param_c <- (c/(2+c))^(x[0])*(1-(c/(2+c)))^(x[1])

## 즉, 조건부 pdf는 n = x1, p = c/(2+c)인 이항분포임을 알 수 있다.

## 2) 표본이 다음과 같다.

x <- c(125,18,20,34)

## 이 때, EM 알고리즘으로 c의 최대우도값을 추정하면

## 완전한 pdf로 완전한 로그우도식을 구하면 그 형태는

full_loglikeli <- x[0]*log(2)+x[1]*log(c)+(x[2]+x[3])*log(1-c)+x[4]*log(c)

## EM 알고리즘에서 Expected 과정은, 이 완전한 로그우도식의 조건부PDF 가정 하의 기댓값을 구하는 것이다.
## 앞서, 조건부PDF는 이항분포를 따른다고 했으므로, 이항분포의 기댓값 = np이므로, 이를 이용하면(x[0]*log(2), 즉 Z11*log(2)는 단순한 상수이므로 알고리즘에선 제외한다.)
## E(Z12)*log(c) = ((z11+z12)=x1)*(c/(2+c)). 즉 다시 정리하면

expected_loglikeli <- (x[0]+x[1])*(c/(2+c))+(x[2]+x[3])*log(1-c)+x[4]*log(c)

## 이를 미분해서 0으로 해주면
## derivative(expected_loglikelihood) = 0 -> (x1*c+2*x4+x4*c)/n*c+2*(x2+x3+x4)이다.

f <- function(x,c){
  (x[1]*c+2*x[3]+x[4]*c)/(sum(x)*c+2*(x[2]+x[3]+x[4]))
}

## (x1-x2-x3+x4)/n을 초기 추정값으로 놓으면

c <- (x[1]-x[2]-x[3]+x[4])/sum(x)
for(i in 0:100){
  c <- f(x,c)
  print(c)
}

## 0.4902824가 결측값까지 고려했을때의 mle값이 된다.

x <- c(125,18,20,34)

c <- (x[1]-x[2]-x[3]-x[4])/sum(x)


for(i in seq(0,1000)){
  c <- (x[1]*c+2*x[4]+x[4]*c)/(sum(x)*c+2*(sum(x[2:4])))
  print(c)
}

x <- c(119,96,146.2,138.6,143.4,98.2,124.5,114.1,136.2,136.4,184.8,79.8,151.9,114.2,154.7,95.9,97.3,136.4,109.2,103.2)
x

p <- 0.7


for(i in seq(0,1000)){
  mu_1 <- sum((1-p)*x)/(length(x)*(1-p))
  sig_1 <- sum(((1-p)*(x-mu_1)^2)/(length(x)*(1-p)))
  mu_2 <- sum((p)*x)/(length(x)*p)
  sig_2 <- sum(((p)*(x-mu_2)^2)/(length(x)*p))
}

mean(x)
var(x)
