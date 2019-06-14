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

a <- 20
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

#3. 2단계 베이지안 모형의 결합 pdf가 다음과 같은 비례식으로 주어졌다.
# (factorial(n)/(factorial(x)*(factorial(n-x))*y^(x-a-1)*(1-y)n-x+b-1
## 1) 표본 모형을 X와 Y로 정의할 때, 각각의 조건부 pdf f(x|y)와 f(y|x)를 정의하라
### (1) f(x,y) = f(x|y)*f(x)와 같으므로, 우선 f(x,y)가 어떤 분포들의 결합으로 이루어졌는지 추정하면
#### - 이항분포 : (factorial(n)/(factorial(x)*(factorial(n-x))*y^(x)*(1-y)^(n-x)
#### - 베타분포 : K * y^(a-1)*(1-y)^(b-1) [단, k는 gamma(a+b)/gamma(a)*gamma(b)는 비례식에서 생략된 상태이다]

## 2) 깁스 샘플링을 작성하라

a <- 10
b <- 4
n <- 6000
x <- c(0)
y <- c()
for(i in seq(1,6000)){
  temp <- rbeta(1,shape1 = x[i] + a, shape2 = n - x[i] + b)
  y <- c(y,temp)
  x <- c(x,rbinom(1,n,temp)) 
}

### (1) E(X)를 구하면

e_x <- 6000*(a/(a+b))

### (2) 신뢰구간을 구하면

lower <- (sum(x[3001:6000])/3000) - qnorm(0.025,0,1)*(var(x)^(1/2)/3000^(1/2))
upper <- (sum(x[3001:6000])/3000) + qnorm(0.025,0,1)*(var(x)^(1/2)/3000^(1/2))

c(lower,upper,e_x)

#4. (계층적 깁스샘플러)
## 베이지안 모형이 다음과 같이 주어졌다.
# X|a <- N(a,b^2/n)
# a|k <- N(0,k^2)
# k <- gamma(alpha,beta)
## 1) 이 계층적 베이지안 모형의 전체 결합 pdf g(X,a,k)를 정의하시오
### 1) 계층적 깁스샘플링을 수행하기 위해선, 하이퍼파라미터 k에 대한 결합pdf와 파라미터 a에대한 결합 pdf 두개가 필요하다.
### 2) 우선, 전체 결합 pdf를 정의하면
# g(X,a,k) <- f(x|a)*h(a|k)*z(k)
# =(1/Gamma(alpha)*beta^(alpha))*(1/2*pi)*(1/(b/n^(1/2)))*(1/k)*exp(-(1/2)*((x-a)^2/b^2)+(a^2/k^2))*exp((-k/beta)) 이다.

## 2) 모수의 사전 결합 pdf와 하이퍼모수의 사전 결합 pdf를 각각 정의하시오
### 1) 모수 결합 pdf를 정의하면
g_a_k <- rnorm(1,(k^2/((b^2/n)+k^2))*x,(k^2*b^2)/(b^2+n*k^2)) 
#이다.
### 2) 하이퍼모수 결합pdf를 정의하면
g_k <- rgamma(shape = alpha+1/2,1/((a^2/2)+(1/beta)))
#이다.

## 3) a의 초깃값을 0으로 둘 때, 이 베이지안 모형의 추정해를 구하시오

x <- 0
a <- c(0)
k <- c()
results <- c()
alpha <- 1
beta <- 1

for(i in seq(1,3000)){
  temp_k <- rgamma(1,shape = alpha+(1/2),1/((a[i]^2/2)+(1/beta)))
  k <- c(k,temp_k)
  temp_a <- rnorm(1,(k[i]^2/((b^2/n)+k[i]^2))*x,(k[i]^2*b^2)/(b^2+n*k[i]^2))
  a <- c(a,temp_a)
  results <- c(results,rnorm(1,a[i+1],b^2/n))
}

# 5. 베이지안 모형이 다음과 같이 주어졌다.
# Y|p ~ binom(n,p)
# P|C ~ h(p|c) = c*p^(c-1)
# C ~ gamma(1,a)
## 1) 손실함수로 MSE가 주어졌을 때, 이 계층적 베이지안 모형의 손실함수의 기댓값(즉 위험함수)를 정의하시오
### 1) 3단계 계층적 베이지안 모형은 두개의 모수(모수, 하이퍼모수)에 대한 적분을 실시한다. 우선, 하이퍼모수 c에 대하여 적분하면
# integrate(손실함수)[integrate(f(y|p)*h(p|c)*u(c)dc)]dp / integate[integrate(f(y|p)*h(p|c)*u(c))dc]dp
# integrate(손실함수)*beta(y+1,n*-y+1)dp
### 이는 beta(y+1,n*-y+1)을 따른다.

rgamma(1,shape = 2, scale = (1/a - log(p[i]))^(-1))
temp <- rbeta(1,y+c,n-y+1)
### 모수에 대해 적분하지 않고, 하이퍼 모수에서만 적분을 실시한 채로 
## 2) 초깃값 y를 획득하라.

initial <- rgamma(1,shape=1,scale=2)
initial_p <- exp((runif(1)-log(initial))/(initial-1))
y <- c(rbinom(1,50,initial_p))
## 3) 깁스 샘플링 알고리즘을 작성하라
b <- c(initial)
p <- c()
y <- c(y)
n <- 50

for(i in seq(1,6000)){
  temp <- rbeta(1,shape1 = y[i]+b[i], shape2 = n - y[i] + 1) 
  p <- c(p,temp)
  temp <- rgamma(1,shape = 2, scale = ((1/a) - log(p[i]))^(-1))
  b <- c(b,temp)
  temp <- rbinom(1,n,p[i])
  y <- c(y,temp)
}

## 4) 모수 b의 기댓값과 함께, 모수 b의 95% 신용구간을 구하여라

upper <- (sum(b[3001:6000])/3000) - qnorm(0.025,0,1)*(var(b[3001:6000])^(1/2)/3000^(1/2))
lower <- (sum(b[3001:6000])/3000) + qnorm(0.025,0,1)*(var(b[3001:6000])^(1/2)/3000^(1/2))

c(upper,sum(b[3001:6000])/3000,lower)
