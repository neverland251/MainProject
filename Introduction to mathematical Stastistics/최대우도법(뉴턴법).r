# 최대 우도 추정

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
