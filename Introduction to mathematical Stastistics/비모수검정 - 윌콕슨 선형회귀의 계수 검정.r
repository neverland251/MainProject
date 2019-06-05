# 비모수 선형회귀 적합
## 이동모형인 선형회귀에서 잔차 e에 대해 정규성을 가정하지 않고 회귀식을 적합하는 것
## 정규성 가정을 포기함으로서 다양한 비모수 기반 점수 함수를 절편 a, 계수 b..를 추정하는데 활용할 수 있다.

# 1. 회귀분석(계수검정).r에서 사용한 벨기에 전화 데이터를 활용하여 b,a를 적합하라
# 단, e는 정규분포를 따르지 않는다.

y <- c(0.44,0.47,0.47,0.59,0.66,0.73,0.81,0.88,1.06,1.20,1.35,1.49,1.61,2.12,11.90,12.40,14.20,15.90,18.20,21.20,4.30,2.40,2.70,2.90)

x <- rep(50:73)

n <- length(y)

#1) 윌콕슨 기반 점수함수를 통해 점근적 정규분포를 구해준다.

## (1)라오-크래머 하한을 만족하는 윌콕슨 점수 함수 : root(12) * ((y순서통계량/n-1) * 1/2))
## 우선 정규화를 위한 피벗을 만들기 위해, 분자에 들어가는 값인 윌콕슨 t를 먼저 구해준다.
fun <- 12^(1/2)*((rank(y)/(n-1))-(1/2))
wilcoxn_t <- sum((x-mean(x))*fun)

## (2)wilcoxn_t의 분산은 다음과 같이 구한다
## 1/(n-1) * sum(x-mean(X))^2 * Sa^2
## 여기서 Sa는 윌콕슨 점수함수의 제곱합이다.

Sa <- sum(fun)
var_t <- sum((x-mean(x))^2) * Sa^2 * (n-1)^(-1)

Z_t <- wilcoxn_t/var_t^(1/2)
## 이 때

1 - pnorm(Z_t,0,1)

#는 한없이 0에 수렴하므로, 계수 b는 0이라는 귀무가설을 기각한다.

# 2. 윌콕슨 선형회귀와 모수기반 선형회귀의 강건성(robust) 비교


y_1500 <- c(373.2,246,245.4,252,243,236.8,241.8,233.6,233.2,231.2,227.8,229.8,225.2,221.2,215.6,218.1,214.9,216.3,219.2,218.4)
x_maraton <- c(3530,3585,5333,3084,3318,2215,1956,2483,1977,1896,1759,2092,1383,1500,916,731,1226,740,595,663)

## 1) 단순선형회귀와 윌콕슨 선형회귀를 각각 적합해준다.
b <- sum(y_1500*(y_maraton-mean(x_maraton)))/sum((x_maraton-mean(x_maraton))^2)
a <- mean(y_1500)

z_lm <- lm(x_1500 ~ y_maraton)
z_wilcoxon <- rfit(x_1500~y_maraton,scores = wscores)

## 2) 회귀선을 긋고, 결과를 비교한다.

ggplot(X_y,aes(y=x_1500,x=y_maraton)) + geom_point() + geom_text(label = rownames(X_y),nudge_y= 4) + geom_line(aes(y=z_wilcoxon$fitted.values),color = "red") + geom_line(aes(y = z_lm$fitted.values),color = "blue")

## 3) 적합된 회귀선으로 판단해보건데, 단순선형회귀보단 윌콕슨 선형회귀가 이상치에 더 강건한(robust)것을 확인할 수 있다.