# 회귀분석

# 1. 다음에 제시된 자료에 대한 반응변수는 1950년부터 1973년까지 벨기에에서 전화를 건 수이다.

y <- c(0.44,0.47,0.47,0.59,0.66,0.73,0.81,0.88,1.06,1.20,1.35,1.49,1.61,2.12,11.90,12.40,14.20,15.90,18.20,21.20,4.30,2.40,2.70,2.90)

x <- rep(50:73)

## 1) 최소회귀제곱선을 젝합하라

### 절편 a의 최대우도추정량은 반응변수의 평균과 같다. 즉
a <- mean(y)

## 한편, 계수 b의 최대우도추정량은 (sum(y)*sum(x-mean(x)))/sum(x-mean(x))^2와 같으며
b <- sum(y*(x-mean(x)))/sum((x-mean(x))^2)

### 이 때, 예측값은
y_hat <- a + b*(x-mean(x))

### 이에 대한 잔차는
err <- y - y_hat

### y의 분산은 이 잔차의 제곱의 합과 같다. 즉
y_var <- sum(err^2)/length(y)

## 2) a와 b의 신뢰구간을 t통계량을 이용하여 구하시오
### (a - a_true)/(y_var/(length(y)-2))^(1/2) 는 T(n-2)를 따른다. 신뢰구간은
lower <- a - ((y_var/length(y)-2)^(1/2))*qt(0.975,df=length(y)-2) 
upper <- a + ((y_var/length(y)-2)^(1/2))*qt(0.975,df=length(y)-2) 
c(lower,upper)

### (b - b_true)/(length(y)*y_var)/((length(y)-2) - sum((x-mean(x)^2))) 는 T(n-2)를 따른다. 즉

lower <- b - ((length(y)*y_var)/((length(y)-2)*sum((x-mean(x))^2)))^(1/2) * qt(0.975,df = length(y)-2)
upper <- b + ((length(y)*y_var)/((length(y)-2)*sum((x-mean(x))^2)))^(1/2) * qt(0.975,df = length(y)-2)
c(lower,upper)

##3) H0 : mu = 0을 검증하기 위한 t통계량을 구하고, a = 0.05 수준에서 가설의 기각 여부를 검정하시오

### 한편, h0 : mu = 0 가설 하에서 이에 해당하는 t통계량을 구하면
h_0 <- (b - 0) / ((length(y)*y_var)/((length(y)-2)*sum((x-mean(x))^2)))^(1/2)

## 이 때, 자유도 22 수준에서 H_0하 t통계량의 확률을 구하면
dt(h_0,df=22)

## 이다.

# 다변량 회귀분석

# 2. 다음과 같은 데이터가 존재한다.

x1 <- c(1,1,1,1,1)
x2 <- c(1,4,2,4,3)
x3 <- c(2,3,2,2,2)
y <- c(3,6,4,4,4)

data <- data.frame(x1,x2,x3,y)
data <- as.matrix(data)

## 1) 회귀계수에 대한 최소적합을 실시하시오

xTx <- t(data[,1:3])%*%data[,1:3]
xTy <- t(data[,1:3])%*%data[,4]

### (1) 이 때, solve 명령어는 해당 행렬의 역행렬을 구해준다.
b <- solve(xTx)%*%xTy


