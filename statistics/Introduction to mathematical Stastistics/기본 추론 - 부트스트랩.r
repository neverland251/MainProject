# 부트스트랩 절차

# 1. a = 1, b = 100의 감마분포로부터 샘플을 무작위 추출한 후, 부트스트랩 신뢰수준 95%의 신뢰구간을 구하시오
## 균등분포를 통해 gamma함수의 역함수로부터 샘플들을 무작위 추출한다.
sampling <- qgamma(runif(3000),shape=1,scale=100)
sampling_theta <- sum(a)/length(a)
## 샘플들의 히스토그램을 그려본다.
hist(sampling)

## 부트스트랩을 실시한다.
for(i in 0:3000){
  ## 균등분포로부터 100개의 값을 추출하여, 소수점을 절사하고 numb에 담는다.
  numb <- round(runif(100,min=1,max=1000))
  ## 샘플변수를 담는다.
  boot <- sampling[numb]
  ## 감마분포에서 평균을 의미하는 b의 점추정량을 추정하기 위하여 샘플에서 평균을 구해준다.
  boot_theta <- sum(boot)/length(boot)
  ## 해당 값을 일단 저장해준다.
  theta_boot[i] <- boot_theta
}
## 부트스트랩된 평균들의 분포를 그려본다. 예쁜 정규분포가 완성된걸 확인할 수 있다.
hist(theta_boot)
## 이 평균들의 평균을 구해준다.
theta_boot_mean <- sum(theta_boot)/length(theta_boot)
## 평균들의 표준편차를 구해준다.
theta_boot_sd <- (sum((theta_boot - theta_boot_mean)^2)/(length(theta_boot)-1))^(1/2)
## 95% 신뢰수준의 상한과 하한을 구한다.
lower <- theta_boot_mean - (qnorm(0.975,0,1)*theta_boot_sd)/(length(theta_boot))^(1/2)
upper <- theta_boot_mean + (qnorm(0.975,0,1)*theta_boot_sd)/(length(theta_boot))^(1/2)
print(c(lower,upper))

#2. 주어진 오염된 정규분포에서 e = 0.2, sige = 4일때
# Xi <- 10Wi + 100, Yi <- 10wi+15 + 120이다. (단, 1<i<50)
#부트스트랩을 이용하여 p=0.05 수준에서 X와 Y가 서로 다른 표본인지 검증하시오
# 오엳된 정규분포 : pnorm(w)*(1-e)+pnorm(w/sige)*(e/sige)

## 1. 주어진 CD의 역함수와 uniform 분포를 결합하여 100개를 무작위로 추출한다.
e = 0.2
sige = 4
ind <- rbinom(30,1,e)
a <- rnorm(30)
W <- a*(1-ind)+sige*a*ind

## 주어진 확률변수로 변환해준다.
X <- 10*W[1:15]+100
Y <- 10*W[15:30]+120

boxplot(X)
boxplot(Y)

## 비교용 검정통계량을 생산한다.

X_bar <- sum(X)/length(X)
Y_bar <- sum(Y)/length(Y)
V_mean <- Y_bar - X_bar

X_med <- median(X)
Y_med <- median(Y)
V_med <- Y_med - X_med

## H0를 기각하는 것이 목적이므로, 두 표본을 하나 합친 확률변수를 생성한다.
z <- c(X,Y)
theta_boot <- c()
## 부트스트랩을 실시한다.
for(i in 0:3000){
  ## 균등분포로부터 10개의 값을 추출하여, 소수점을 절사하고 numb에 담는다.
  #X_numb <- round(runif(10,min=1,max=30))
  #Y_numb <- round(runif(10,min=1,max=30))
  ## 샘플변수들의 평균을 구한다.
  X_boot <- mean(sample(z,15),replace=TRUE)
  Y_boot <- mean(sample(z,15),replace=TRUE)
  ## 평균값의 차이를 구한다.
  boot_theta <- Y_boot - X_boot
  ## 해당 값을 일단 저장해준다.
  theta_boot[i] <- boot_theta
}
# 검정통계량인 v값과 부트스트랩된 vj값을 비교할 수 있도록 히스토그램을 그린다.
hist(theta_boot,probability = TRUE)
length(theta_boot[theta_boot > V_mean])/length(theta_boot)
# 이 때, v=20은 부트스트랩된 확률분포의 확률 0.05 오른쪽에 있으므로, H0를 기각하고 둘은 다른 분포임을 확인할 수 있다.

# 이번엔 중앙값을 확인한다.
z <- c(X,Y)
theta_boot <- c()
## 부트스트랩을 실시한다.
for(i in 0:3000){
  ## 균등분포로부터 10개의 값을 추출하여, 소수점을 절사하고 numb에 담는다.
  #X_numb <- round(runif(10,min=1,max=30))
  #Y_numb <- round(runif(10,min=1,max=30))
  ## 샘플변수들의 평균을 구한다.
  X_boot <- median(sample(Z,15),replace=TRUE)
  Y_boot <- median(sample(Z,15),replace=TRUE)
  ## 평균값의 차이를 구한다.
  boot_theta <- Y_boot - X_boot
  ## 해당 값을 일단 저장해준다.
  theta_boot[i] <- boot_theta
}
# 검정통계량인 v값과 부트스트랩된 vj값을 비교할 수 있도록 히스토그램을 그린다.
hist(theta_boot,probability = TRUE)

length(theta_boot[theta_boot > V_med])/length(theta_boot)


# 3. X = c(x1,x2...xn)과 Y = c(y1,y2....yn)인 두 확률표본의 실현값들의 모임이다. 검정통계량은 표본평균의 차이 y_bar-x_bar이다. 이 검정의 p값을 추정하시오
X <- c(94.2,111.3,90.0,99.7,116.8,92.2,166.0,95.7,109.3,106.0,111.7,111.9,111.6,146.4,103.9)
Y <- c(125.5,107.1,67.9,98.2,128.6,123.5,116.5,143.2,120.3,118.6,105.0,111.8,129.3,130.8,139.8)
Z <- c(X,Y)
## 부트스트랩을 실시한다.
theta_boot <- c()
n <- length(Z)
for(i in seq(0:3000)){
  choice <- sample(n)
  temp_X <- Z[choice[1:(n/2)-1]]
  temp_Y <- Z[choice[(n/2):n]]
  temp <- mean(temp_Y) - mean(temp_X)
  theta_boot[i] <- temp

}

## 확률값을 도출한다.

length(theta_boot[theta_boot>mean(Y) - mean(X)])/length(theta_boot)

# 부트스트랩을 활용한 일표본 검정(평균)
## 

e = 0.2
sige = 4
ind <- rbinom(30,1,e)
a <- rnorm(30)
W <- a*(1-ind)+sige*a*ind

X <- 10*W + 100

theta_boot <- c()
for(i in seq(1,3000)){
  temp <- sample(X,length(X),replace=TRUE)
  temp <- temp - mean(X) + 90
  theta_boot[i] <- mean(temp)   
  
}

length(theta_boot[theta_boot > mean(X)])/length(theta_boot)

#부트스트랩을 활용한 일표본 검정(중앙값)
e = 0.2
sige = 4
ind <- rbinom(30,1,e)
a <- rnorm(30)
W <- a*(1-ind)+sige*a*ind

X <- 10*W + 100

theta_boot <- c()
for(i in seq(0,3000)){
  temp <- sample(X,length(X),replace=TRUE)
  temp <- temp - median(X) + 90
  theta_boot[i] <- median(temp)   
  
}

length(theta_boot[theta_boot > median(X)])/length(theta_boot)



(20^(1/2)*(mean(x)-90))/var(x)^(1/2)
# 4. 네이만-피어슨 정리에 의거한 우도비 검정을 통해 

# H0 : c1 = c2, h1 : c1 =/ c2인 복합 가설을 검정하는 통계량 g(y)를 정의한 결과 
# g(y) = L(a^)/L(b^) <= k가 되었다.(단, a^,b^는 a와 b에 대한 mle 추정값을 투입한 최대우도함수)
# 이 때, 이 역함수 y = g^(-1)(L(a^)/L(b^)) 는 (L(a^)/L(b^))^(2/m+n)이 되었고, 이 식은 정리하면 다음의 식을 따르는 t검정량 공식이 나온다.

((((n*m)/(n+m))^(1/2))*(x_mean-y_mean))/((x_var+y_var)/(n+m-2))^(1/2)

## 1) 분산이 225로 동일한 어떤 확률변수 x와 y에 대하여, H0 : c1 = c2, h1 : c1 =/ c2를 검정할 때
## 조건이 다음과 같다고 하자

n = m = 100
exp_mean_x <- 100
exp_mean_y <- 100
exp_sd <- 15

## 이 때, 귀무가설 H0 c1 = c2를 만족하는 경우, T통계량은 N(0,1)로 분포수렴함을 부트스트랩을 통해 증명하라

theta_boot <- c()
for(i in 0:3000){
  ## 균등분포로부터 100개의 확률값을 임의로 선출하고, 각각 x와 y의 확률표본을 선출한다.
  x_choice <- qnorm(runif(n), mean = exp_mean_x, sd = exp_sd)
  y_choice <- qnorm(runif(m), mean = exp_mean_y, sd = exp_sd)
  ## E(x)와 E(X^2)를 각각 정의한다.
  x_mean <- sum(x_choice)/n
  y_mean <- sum(y_choice)/m
  x_var <- sum((x_choice - x_mean)^2)
  y_var <- sum((y_choice - y_mean)^2)
  ## t통계량으로 부트스트랩을 실시한다.
  theta_boot[i] <- ((((n*m)/(n+m))^(1/2))*(x_mean-y_mean))/((x_var+y_var)/(n+m-2))^(1/2)
}

## 히스토그램을 그려본 결과, N(0,1)의 분포를 따름을 알 수 있다.

hist(theta_boot)
## 비모수 검정법은 KS검정의 결과도, 분포가 동일하다는 귀무가설을 기각하지 못하기 때문에 동일한 분포임을 확인할 수 있다.
ks.test(theta_boot,qnorm(runif(3000),mean = 0,sd = 1))

## 부트스트랩 중앙값에 대한 90% 신뢰구간 구하기

x <- c(131.7,182.7,73.3,10.7,150.4,42.3,22.2,17.9,264.0,154.4,4.3,264.6,61.9,10.8,48.8,22.5,8.8,150.6,103.0,84.9)

result <- c()
for(i in seq(0,3000)){
  temp <- sample(x,length(x),replace=TRUE)
  theta <- mean(temp)
  result <- c(result,theta)
}

lower <- ((0.05/2) * length(result))
upper <- length(result) + 1 - lower

print(c(sort(result)[lower],sort(result)[upper]))

# 이 부트스트랩 신뢰구간은 실제 모수를 포함한다.
mean(x)

## 추정량의 표준화

result <- c()
for(i in seq(0,3000)){
  temp <- sample(x,length(x),replace=TRUE)
  theta <- mean(temp)
  # 부트스트랩 통계량을 t분포를 따르는 값으로 재정의한다.
  theta <- (length(temp)^(1/2)*(theta - mean(x)))/(var(temp)^(1/2))
  result <- c(result,theta)
}

result <- sort(result)

print(c(mean(x)-(result[lower]*(var(x)^(1/2))/length(x)^(1/2)),mean(x)-(result[upper]*(var(x)^(1/2))/length(x)^(1/2))))

# 표준화한 부트스트랩 통계량은 앞서 구한 통계량의 신뢱구간과 거의 일치하나, 조금 더 보수적이다.

# 지메이스 자료를 이용한 부트스트랩

X <- c(23.5,12.0,21.0,22.0,19.125,21.5,22.125,20.375,18.250,21.625,23.250,21,22.125,23,12)
Y <- c(17.375,20.375,20.0,20,18.375,18.625,18.625,15.250,16.5,18.0,16.25,18,12.75,15.5,18)

# 가설 검정은 <X와 Y의 차이>에 대하여 그 값이 0이라는 가설을 검정한다.
# H0 하에서 검정을 실시해야 하기 때문에 <차이 값>에서 <평균의 차이 값>을 빼준 후, 가설 평균 0을 더하여 H0를 만족하도록 변수를 조작해준다
# 이렇게 까지 했는데도 0과 유의미한 차이가 있다면, 이는 귀무가설을 기각하고 연구가설을 채택하는 아주 강력한 근거가 될 것이다.

Z <- X - Y
Z_real <- mean(X) - mean(Y)

# H0  : mu = 0이라는 가설이 참이라는 전제 하에 검정을 실시할 수 있도록 위치를 조정해준다.

d <- (Z - (mean(X) - mean(Y)) + 0)

# 부트스트랩 3000번 실시

for(i in seq(0,3000)){
  temp <- sample(d,length(d),replace=TRUE)
  boot_theta[i] <- mean(temp)
}

#확률을 구해준다.

length(boot_theta[boot_theta>(mean(X)-mean(Y))])/length(boot_theta) < 0.05
