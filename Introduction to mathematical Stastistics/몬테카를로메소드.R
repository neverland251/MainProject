# 공정한 육면체 주사위가 있을때, 던진 결과가 {x:1,2}면 1, 나머지는 0이라고 가정하자. 즉
## px = 1 if 1,2 0 else이다.
## 이때, 기댓값 mu는 1/sum(c(1*1,1*2,0*3,0*4,0*5,0*6)) 이다.
## 이때, 확률변수 X는 
## X = 1 if 0<U<=1/3 0 if 1/3<U<=1, (U는 균등분포(0,1) 이다.)

set.seed(2599)

n <- 10
u <- runif(n)
X <- rep(0,n)
cut <- 1/3
for(i in 1:n){
  if(u[i] <= cut){
    X[i] <- 1
  }
}

# 1X1의 단위 정사각형에서 무작위로 선택할 때, 샘플이 뽑힐 확률은 각각 iid인 균등(0,1)을 따른다.
# 그 샘플이 r=1의 단위원안에 들어있을 확률 pmf는 
#X <- 1 if U1^2+U2^2<1
#     0 else
# (단, U1과 U2는 균등(0,1)을 따르는 iid) 
# 이 때, 기댓값 mu는 1*(pi/4)+0*(1-pi/4) = pi/4이다.
# pi = 4*mu이므로, 수치적 방법으로 이를 근사하면

x <- rep(0,10000)
for(i in 1:10000){
  if(runif(1)^2+runif(1)^2 < 1){
    x[i] <- 1
  }
  else{
    x[i] <- 0 
  }
}
print(4*(sum(x)/length(x)))

## 시행횟수가 많아질수록 pi에 정확히 근사하는 것을 확인할 수 있다.

for(i in 1:100000){
  if(runif(1)^2+runif(1)^2 < 1){
    x[i] <- 1
  }
  else{
    x[i] <- 0 
  }
}
print(4*(sum(x)/length(x)))

## 이 때, pi의 95% 신뢰구간을 대표본 신뢰구간을 구하는 방법으로 구하면
## 확률의 대표본 신뢰구간은 (1-a)% = p-Za*((p*(1-p)/n)^(1/2)와 같으므로
lower <- 4*sum(x)/length(x) - qnorm(0.975,0,1)*4*(((sum(x)/length(x))*(1-(sum(x)/length(x))))/length(x))^(1/2)
upper <- 4*sum(x)/length(x) + qnorm(0.975,0,1)*4*(((sum(x)/length(x))*(1-(sum(x)/length(x))))/length(x))^(1/2)
print(c(lower,upper))

#몬테카를로 적분
## 초등함수로 역도함수가 표현이 안될때 수치적인 방법으로 적분을 구하는것
## integrate(g(x),a,b) = (b-a) * integrate(g(x)*(1/(b-a)),a,b) = (b-a)E(g(x))를 이용한다.

## 1. pi를 몬테카를로 적분을 통해 추정하라. 단, 균등분포 (0,1)을 이용하라.
## (1-x^2)^(1/2) = arctan(1) = pi/4에서, 4arctan(1) = pi를 구할 수 있다.
## Y = (1-X^2)^(1/2)라고 한다면, 

X <- runif(10000000)
Y <- 4*(1-X^2)^(1/2)
Y_bar <- sum(Y)/length(Y)
Y_var <- sum((Y - Y_bar)^2/length(Y))

## 이때 95% 신뢰구간은
lower <- Y_bar - pnorm(0.975,0,1)*(Y_var^(1/2)/length(Y))
upper <- Y_bar + pnorm(0.975,0,1)*(Y_var^(1/2)/length(Y))
print(c(lower,upper))

## 2. y1 = exp((-x1^2+x2^2)/2), y2 = 1/2pi*arctan(x2/x1)을 이용하여 정규분포에서 무작위 관측값을 생성하시오(단, Y1

Y1 <- runif(1000)
Y2 <- runif(1000)

## 표준정규분포의 pdf를 역함수로 변환한 함수를 f^(-1)라고 정의하자
## 균등분포 Y1과 Y2에 이 역함수를 적용하면 f^(-1)(y1)|J| f^(-1)(y2)|J|이다.
## 그리고. 이 역함수 f^(-1)(y1)|j|와 f^(-1)(y2)|j|를 결합한 결합pdf {f^(-1)(y1) * f^(-1)(y2)}*|j|는 
## X1 = (-2logY1)^(1/2)*cos(2*pi*Y2) 혹은 X2 = (-2logY2)^(1/2)*sin(2*pi*Y2)의 결합 pdf로 알려져 있다.

X1 <- (-2*log(Y1))^(1/2)*cos(2*Y_bar*Y2)

## 오염된 정규분포가 e = 0.25와 sige = 25를 따를 때, n=20에서 다음의 가설을 검정하시오
## H0 : mu = 0, H1 : mu > 0 (유의수준 a = 0.05)
e = 0.25
sige = 25

Y1 <- runif(20)
Y2 <- runif(20)
X1 <- (-2*log(Y1))^(1/2)*cos(2*Y_bar*Y2)
W <- pnorm(X1)*(1-e)+pnorm(X1/sige))
X1_bar <- sum(X1)/length(X1)
X1_sig <- sum((X1-X1_bar)^2)/(length(X1)-1)
t_test <- (X1 - X1_bar)/ 
