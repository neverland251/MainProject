#1. 조건부 분포 f(x|y)와 사전분포 f(y)가 다음과 같이 제시되었다.
# f(y) = exp(-y)
# f(x|y) = exp(-(x-y))
## 1) MCMC 알고리즘을 작성하라
### (1) f(y)의 역함수 f^(-1)(u)(0<u<1)은 다음과 같다.
fu <- function(n){
  -log(1-runif(n))
}

### (2) f(x|y)의 역함수 f^(-1)(u,y)(0<u<1)은 다음과 같다.

fuy <- function(n){
  fu(n) - log(1-runif(n))
  
}
### (3) 이를 이용하여 2000회의 시뮬레이션을 시행하면

x <- fuy(2000)

## 2) 위에서 도출한 알고리즘을 이용하여 E(X)의 95% 근사 신뢰구간을 정의하시오

### (1) sum(x)/n은 대수의 약법칙에 따라 E(X)로 확률 수렴한다. 또한, n^(1/2)*(sum(x)/n - E(x))/var(X)는 CLT에 따라 N(0,1)로 수렴하는데
### 문제는 var(X)를 그 누구도 모른다는 점에 있다.
### (2) sum(x-mean(x))^2/(n-1)은 대수의 약법칙에 따라 var(X)로 확률 수렴하며, 따라서 이를 이용하여 근사 신뢰구간을 정의한다. 

num <- length(x)

sd <- (sum(x - mean(x))^2/(num-1))^(1/2)

c(mean(x) - (qnorm(0.05) * (sd/num^(1/2))) , mean(x) + (qnorm(0.05) * (sd/num^(1/2)))) 

# 2.(몬테카를로 시뮬레이션) 조건부 분포 f(x|y)와 사전분포 f(y)가 다음과 같이 제시되었다.

# (a) f(x|y) = 2*exp(-2*y)
# (b) f(y) = exp(-(x-y))
## 1) 조건부 분포와 사전분포를 이용하여 확률변수 x를 추출하시오
### (1) pdf에서 확률변수를 출력시키기 위해, 이들 pdf를 모두 역함수의 형태로 변환시키면
# (a) f(x|y)^(-1) -> x = y - log(1 - p_x)
# (b) f(y)^(-1) -> y = (1/2)*log(1 - p_y)

fy <- function(u){
  -(1/2)*log(1-u)
}

fxy <- function(n){
  p <- runif(n)
  fy(p) - log(1-p)
}

x <- fxy(10000)

## 2) 도출된 샘플들의 평균, 표준편차를 구하라

mean(x)
var(x)^(1/2)

## 3) 도출된 샘플의 평균에 대한 95% 근사 신뢰구간을 구하라
### (1) 밑에서 논의하겠지만, E(X)를 직접적으로 구하기는 어렵다. 다만, var(X)의 일치추정량인 1/(n-1)*sum(x-mean(x))^2, 즉 표본분산을 이용하면
### E(X)의 근사 95% 신뢰구간을 구할 수 있다. 

upper <- mean(x) - qnorm(0.025)*(sd(x)/length(x)^(1/2))
lower <- mean(x) + qnorm(0.025)*(sd(x)/length(x)^(1/2))

### (2) E(X)에 대한 95% 근사 신뢰구간은 다음과 같다.

c(lower,upper)

# 3. MCMC를 활용한 베이지안해의 추정
## 1) '베이지안 해'는 '사후 pdf르 받침으로 갖는 분포'의 '손실함수'의 기댓값, 즉 E[(k - l(x))^2]이 최소화되는 함수 l(x)를 의미한다.
## 만약 손실함수가 위와 같이 RMSE로 주어졌을 경우 그 기댓값을 최소화하는 l(y)는 사후분포의 평균, 즉 E(w)가 된다.
## 만약 손실함수가 RMSE가 아닌 절대평균오차로 주어졌을 경우, 그 기댓값을 최소화하는 l(y)는 사후분포의 중앙값, 즉 med(w)가 된다.

## 2) 몬테카를로 기법을 활용하여, 다음의 베이지안 모형에 대한 베이지안 해를 추정하시오
### (a) Y|K는 N(k,var^2/n)을 따른다.
### (b) K는 모수 a,b의 로지스틱 분포를 따른다.

### (1) 로지스틱 분포는 연산의 결과가 닫힌 형태로 나타나지 않는 대표적인 분포이다. 따라서, 몬테카를로 기법을 이용하여 근사적으로 베이지안해를 추정하면
### integrate(k*pdf(y|k)*로지스틱pdf*dk) / integrate (pdf(y|K) * 로지스틱 pdf * dk)이다.
### 여기서 분자는 k에 대한 기댓값을 나타내고, 분모는 베이지안 추정을 할때 필요한 예측분포, g(k)이다.
### 위를 기댓값 연산자로 다시 쓰면
### E[k*pdf(y|k)]/E[pdf(y|K)]로 표현할 수 있는데, 앞에서 언급했듯이 로지스틱 분포는 닫힌 형태로 나타내기가 어려운 분포이다.
### 따라서, 대수의 약법칙과 슬러스키 정리를 활용하면, 저 기댓값의 비율에 대한 일치추정량은

inverse_logistic <- function(a,b,n){
  u <- runif(n)
  a + b*log(u/(1-u))
  
}

#pdf(y|k)의 함수를 정의

pdfy <- function(n,k,var){
  temp_vec <- c()
  for(i in seq(1,n)){
    temp <- dnorm(k,var/n)
    temp_vec <- c(temp_vec,temp)
  }
  temp_vec
}

#로지스틱 변수를 추출하고

logistic_dom <- inverse_logistic(a,b,1000)

#이를 이용하여 pdf(y|k)의 변수를 각각 추출한다.

numerator <- (sum(logistic_dom * pdfy(1000,logistic_dom,var)))/n
denominator <- (sum(pdf(1000,logistic_dom,var)))/n

# 몬테카를로 기법을 활용한 베이지안해 추정량은 아래에 나온다.

numerator/denominator

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

# (factorial(n)/(factorial(x)*(factorial(n-x))*y^(x-a-1)*(1-y)n-x+b-1 ( 상수항 c(y)는 생략되었다)

## 1) 표본 모형을 X와 Y로 정의할 때, 각각의 조건부 pdf f(x|y)와 f(y|x)를 정의하라
### (1) f(x,y) = f(y|x)*f(y)와 같으므로, 우선 f(x,y)가 어떤 분포들의 결합으로 이루어졌는지 추정하면
#### - 이항분포 f(y|x) : [(factorial(n)/(factorial(x)*(factorial(n-x))]*y^(x)*(1-y)^(n-x)
#### - 베타분포 f(y) : K * y^(a-1)*(1-y)^(b-1) [단, k는 상수항으로서 간략화하였다]

### (2) 마찬가지로, f(x,y) = f(x|y)*f(x)이므로
#### - 베타분포 f(x|y) : K * y^(x-a-1)*(1-y)^(n-x+b-1)
#### - ??분포 f(x) : [(factorial(n)/(factorial(x)*(factorial(n-x))]

## 2) 깁스 샘플링을 작성하라

### (1) 위에서 구했듯이 X|Y는 베르누이 분포를, y|X는 베타분포를 따른다. 따라서
#### - 이항분포(n,y) : [(factorial(n)/(factorial(x)*(factorial(n-x))]*y^(x)*(1-y)^(n-x)
#### - 베타분포(x-a, n-x+b) : K * y^(x-a-1)*(1-y)^(n-x+b-1) [단, k는 상수항으로서 간략화하였다]

func_x <- function(sample_num,y){
  rbinom(1,sample_num,y)
}

func_y <- function(sample_num,x){
  rbeta(1,shape1=x + a, shape2 = sample_num-x+b)
}

gibbs <- function(iter,sample_num,init_x){
  y <- c()
  x <- c()
  for(i in seq(1,iter)){
    if(i == 1){
      y <- c(y,func_y(sample_num,init_x))
      x <- c(x,func_x(sample_num,y[i]))
    }
    else{
      y <- c(y,func_y(sample_num,x[i-1]))
      x <- c(x,func_x(sample_num,y[i]))
    }
  }
  return(list("y" = y,"x" = x))
}

### 모수를 생산해줄 y분포에 쓰일 모수를 정의한다.
a <- 10
b <- 4
iter <- 3000
sample_num <- 6000

### 모수를 생산해줄 y분포에 쓰일 x의 초깃값을 하나 구해준다.
init_x <- func_x(sample_num = 6000,y = rbeta(1,shape1 = 10, shape2 = 4))

### 깁스샘플링을 실행한다.

k <- gibbs(iter = 3000,sample_num = 6000,init_x = init_x)



## E(X), E(Y)에 대한 신뢰구간을 작성하라
### (1) 대수의 약법칙에 따라, sum(y)/(sample_num-iter), sum(x)/(sample_num-iter)는 각각 E(Y)와 E(X)로 확률 수렴한다
### (2) 또한, CLT에 따라 ((iter)^(1/2)*(mean(x) - E(x)))/var(X)는 N(0,1)로 분포 수렴하고, 이때 모수 var(X)대신 일치추정량

# sd <- sum(x - mean(x))^2/(iter-1)

###을 사용하여 근사 신뢰구간을 정의하면

sd_x <- sum((k$x - mean(k$x))^2)/(iter-1)
sd_y <- sum((k$y - mean(k$y))^2)/(iter-1)


#x에 대한 신뢰구간 

lower <- mean(k$x) + (qnorm(0.025)*(sd_x/length(k$x))^(1/2))
upper <- mean(k$x) - (qnorm(0.025)*(sd_x/length(k$x))^(1/2))

c(lower,upper)

#y에 대한 신뢰구간

lower <- mean(k$y) + (qnorm(0.025)*(sd_y/length(k$y))^(1/2))
upper <- mean(k$y) - (qnorm(0.025)*(sd_y/length(k$y))^(1/2))

c(lower,upper)

## E(X), E(Y)는 각각의 결합 pdf로부터 추정하면 다음과 같다.

e_y <- a/(a+b) #결합 pdf는 x에 의존하지 않는 y만의 pdf로 분리할 수 있고, b(a,b)의 기댓값은 왼쪽과 같다.
e_x <- sample_num*(a/(a+b)) #결합 pdf에 E에 대한 기댓값을 구하면 왼쪽과 같이 나온다.

# 4.(계층적 깁스샘플링) 베이지안 모형이 다음과 같이 주어졌다.

# (a) Y ~ b(n,p)
# (b) p|K ~ h(p|k) = k*p^(k-1)
# (c) k ~ gamma(1,a) (단,a는 알려졌다.)

# 1) n = 50, a = 2라고 한다. MCMC를 활용하여 K에 대한 베이지안 추정해를 구하라

## (1) 우선, 모수 p와 모수k의 사후pdf를 정의하여 그 사후분포가 무엇인지 확인해야한다. 확인해본 결과
#(a) f(p|y,k)의 pdf는 beta(y+k,n-y+1)을 따르는 분포의 pdf인것으로 밝혀졌다.
#(b) f(k|y,p)의 pdf는 gamma(2,((1/a) - log(p))^(-1))을 따르는 분포의 pdf인 것으로 밝혀졌다.

## (2) 위에서 구한 pdf를 토대로 깁스 샘플러를 작성한다.

# 가장 말단인 k를 생성하는 함수를 작성
func_a <- function(a,p){
  rgamma(1,shape=2,scale=((1/a)-log(p))^(-1))
}

# p를 생성하는 함수를 작성
func_b <- function(n,y,k){
  rbeta(1,shape1 = y+k,shape2=n-y+1)
}

# y를 생성하는 함수를 작성
func_c <- function(n,p){
  rbinom(1,size = n, p = p)
}

gibbs_hire <- function(iter,num_sample,alpha,init_p,init_k){
  k <- c()
  p <- c()
  y <- c()
  for(i in seq(1,iter)){
    if(i == 1){
      k <- c(k,func_a(alpha,init_p))
      y <- c(y,func_c(num_sample,init_p))
      p <- c(p,func_b(num_sample,y[i],init_k))
    }
    else{
      k <- c(k,func_a(alpha,p[i-1]))
      y <- c(y,func_c(num_sample,p[i-1]))
      p <- c(p,func_b(num_sample,y[i],k[i]))
    }
  }
  return(list("y" = y, "p" = p, "k" = k))
}


init_k <- rgamma(1,1,2)
init_p <- exp(log(runif(1)/init_k)/(init_k-1))

result <- gibbs_hire(3000,50,2,init_p,init_k)

## (3) 베이지안 추정해는 mean(result$k)이다. 대수의 약법칙에 따라 이 추정해는 E(K)로 수렴한다.

mean(result$k)

## (4) 이 때, CLT를 이용하면 베이지안 추정해의 근사 신뢰구간을 구할 수 있다.

sd_k <- sum((result$k - mean(result$k))^2)/(iter-1)


###k에 대한 신뢰구간 

lower <- mean(result$k) + (qnorm(0.025)*(sd_k/length(result$k))^(1/2))
upper <- mean(result$k) - (qnorm(0.025)*(sd_k/length(result$k))^(1/2))

c(lower,upper)

# 2) p에 대하여 베이지안 추정해를 구하라. 단 6000회 반복하고, 3000회 이후의 값을 사용하라

result <- gibbs_hire(6000,50,2,init_p,init_k)

## (1) 3000~6000번대의 p에 대한 표준편차를 정의한ㄷ

sd_p <- sum((result$p[3001:6000] - mean(result$k[3001:6000]))^2/(3000-1))다

## (2) CLT

lower <- mean(result$p[3001:6000]) + (qnorm(0.025)*(sd_p/3000)^(1/2))
upper <- mean(result$p[3001:6000]) - (qnorm(0.025)*(sd_p/3000)^(1/2))

c(lower,upper)


#5. (계층적 깁스샘플러2)
## 베이지안 모형이 다음과 같이 주어졌다.

# (a) X|a <- N(a,b^2/n)
# (b) a|k <- N(0,k^2)
# (c) k <- gamma(alpha,beta)

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
