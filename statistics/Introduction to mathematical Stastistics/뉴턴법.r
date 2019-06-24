# 뉴턴법

#1. X1,X2,X3,X4,X5가 중앙값 c인 코시분포에서 추출한 확률표본이다. 즉
# (1/pi)*(1/(1+(x-c)^2))가 공통 pdf인 iid이다.
# 확률표본의 값이 다음과 같다.
x <- c(-1.94,0.59,-5.98,-0.08,-0.77)
#c의 mle를 수치적 방법으로 구하라

## 뉴턴법
## 코시분포의 pdf에 뉴턴법을 적용하기 위해 1계 미분을 실시하면
f_prime <- function(x,ceta){
  2*sum((x-ceta)/(1+(x-ceta)^2))
}
## 와 같다.
## 뉴턴법은 xn = Xn+1 - F/F_prime 의 반복이므로
## 함수 f를 정의하면

f <- function(x,ceta){
  length(x)*log(pi)+sum(log(1+(x-c)^2))
  
}

#초깃값으로 뉴턴법 함수를 1계 미분한 후 <0이 되도록 초깃값 ceta를 정해주는 방법도 있으나, 간단하게 x의 중앙값을 초깃값으로 사용해준다.
ceta <- median(x)

for(i in 0:10000){
  ceta <- ceta - f(x,ceta)/f_prime(x,ceta)
  print(ceta)
}

# 2. 뉴턴법을 활용하여 세 점과의 거리가 최소화되는 지점인 스테이너의 지점을 도출하시오
## 스테이너의 꼭지점 함수 f = d1+d2+d3를 사용하시오
## 단, a = (0,0), b = (3,0), c = (x,y)이다.
## (단, dn = (x^2+y^2)^(1/2)인 L2norm)

steiner <- function(x,y){
  for(i in rep(0:10)){
    dfx <- 4*x - 6
    dfxx <- 4
    dfy <- 4*y
    dfyy <- 4
    x <- x - dfxx
    y <- y - dfy/dfyy
    print(c(x,y))
  }
}
# 3. X1...X100을 N(c,100)에서 추출한 확률포본이라고 하자
## H1 : c = 78, H0 : c = 75를 검정한다고 하자
## 이 때, 손실함수 L(c,c1)에서 c는 참값, c1은 함수를 통해 구해진 추정값이라고 하고 L(75,78) = 3이고, 반대로 L(78,75) = 1이라고 할 때
## 이 손실함수를 기반으하 하는 위험함수를 최소화하는 최대최소 기각역과 검정력을 구하라

## 1) 최대최소 기각역은 L(75,78)*PH1(y>g^(-1)(k) = c), 즉 연구가설을 만족한다고 가정할 때 모수가 귀무가설의 모수일 확률(즉 2종 오류의 확률)과
## 2) L(78,75)*PH0(y<g^(-1)(k) = c), 즉 귀무가설을 만족한다고 가정할 때 모수가 대립가설의 모수일 확률(즉 1종 오류의 확률)의 곱이
## 같아 지는 지점에서 최대최소 기각역이 생성된다.
## 즉, E(L(C,C1)) = E(L(C,C2)) 인 integrate L(C,C1)*pdf(x)dx(구간은 1-C2) = integrateL(C,C2)*pdf(x)dx(구간은 C1) 인 지점에서 최대최소 기각역이 생성된다.


a <- function(c){
  for(i in rep(0:1000)){
  g <- pnorm(c - 78) - 3*(1-pnorm(c - 75)) 
  g_d <- dnorm(c-78) + 3*(dnorm(c-75))
  c <- c - (g/g_d)
  print(c)
  }
}

## 중간값인 c0 = 76으로 시도할 때, 이 값은 76.78318로 수렴하는 것을 알 수 있다. 즉, 기각역은 C = 76.78318이고
## 이 때 검정력은

pnorm(76.78318,78,10)

#이다.

x <- -100

# 뉴턴법을 활용하여, cdf = 1 - e^(-x) - x*e^(-x)의 중위수(즉, 1/2)를 구하여라
## 1) x - f(x)/f'(x) - 1/2 가 수렴하는 x를 구하면 된다
## 2) f(x)*f''(x)/f'(x)^2의 수렴테스트 결과, x > -1.65 를 만족하면 어떤 지점에서든 다 수렴하는 것으로 나타났다. 이 값 이상으로 아무 값이나 지정해주면

for(i in seq(0,100)){
  x <- x - ((1-exp(-x)-x*exp(-x)+x)/(exp(-x)*x+1) - (1/2))
  print(x)
}

## 3) 중위수는 x = 0.5524685 이다.

# 기타 수치적 방법

## 할선법

# x^2 - 4를 f(xn)/(((fxn-1)-f(xn))/xn-1 - xn)인 할선법을 이용하면
## 이 알고리즘은 전단계 값과 전전단계 값 두개가 입력되어야 한다.
## 할선법을 이용하여 식을 정리하면
x <- c(1,2.5)
for(i in seq(2,1000)){
  x <- c(x,x[i] - ((x[i]^2-4)*(x[i]-x[i-1]))/(x[i]^2-x[i-1]^2))
}
## x = 2에서 수렴하는 것을 볼 수 있다.

## IBR
# 중간값(1/2)*(x[n-1]+x[n])의 값이 같은 부호를 가지는 경우 멈추는 방법
## 부호가 같은 값이 해당 값의 위치 범위가 된다.

x <- c(1,2.5)
for(i in seq(2,1000)){
  x <- c(x,((1/2)*(x[i-1]+x[i]))^2 - 4)
  if(i != 2&sign(x[i]) == sign(x[i-1])){
    stop()
  }
}
print(x)
## 값은 [-0.9375,-3.3896] 사이에 존재한다.