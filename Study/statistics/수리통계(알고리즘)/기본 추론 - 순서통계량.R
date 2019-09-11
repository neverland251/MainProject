
## 1. 자료의 표본이 다음과 같다. 
p <- c(13,5,202,15,99,4,67,83,36,11,301,23,213,40,66,106,78,69,166,84,64)
### 1) 이 자료의 5분위수를 구하여라
### 분위수 공식은 p = k/(n+1), k = p(n+1)이므로, 각 분위수의 순서량을 구하면
q = c(0,0.25,0.5,0.75,0.99)
fiveorder = q*(length(p)+1)
### p를 정렬한 후,  각 분위수를 구하면
fivequrter = sort(p)[fiveorder]
sort_p <- c(min(p),fivequrter)
print(sort_p)
### 2) 이 자료의 이상값이 있는지 확인하라
h <- (1.5)*(sort_p[4] - sort_p[2])
c(sort_p[2] - h, sort_p[4] + h)
#### 이 자료엔, 3분위수의 h를 초과하는 자료인 202,213,301의 3개의 이상값이 존재한다.

## 2. 주어진 분포에서 잠재적 이상값, 즉 h = (1.5(Q3 - Q1)) 외에 존재할 확률을 구하시오

### 1) 정규분포

#### (1)역함수를 통해 1분위수(0.25), 3분위수(0.75)를 이용해 h를 구해준다.

h <- (1.5)*(qnorm(0.75)-qnorm(0.25))

#### (2)1분위수와 3분위수에서 h를 빼고, 더한값의 확률값을 다시 출력시키면

sum(c(pnorm(qnorm(0.25)-h),1 - pnorm(qnorm(0.75) + h)))

#### 이것이 잠재적 이상값의 확률이다

### 2) exp(-x)/(1+exp(-x))^2를 pdf로 가지는 분포
logistic_func <- function(x){
  return(exp(-x)/(1+exp(-x))^2)
}
#### (1) pdf를 적분하여 cdf로 바꿔주면, 이는
# -1/(1+exp(-x))와 같다.
#### (2) cdf를 역함수로 바꿔주면
inverse_function <- function(x){
  return(log((1-x)/x))
}
# x = log((1-p)/p) 와 같다.
#### (3) 역함수를 통해 1분위수, 3분위수, h를 구하면

h <- (1.5)*(inverse_function(0.75) - inverse_function(0.25))

sum(c(logistic_func(inverse_function(0.25) - h), logistic_func(inverse_function(0.75) + h)))

#### 이 확률이 로지스틱 분포의 잠재적 이상값의 확률이 된다.

### 3) (1/2)*exp(-|x|)를 pdf로 가지는 분포
raplace_func <- function(x){
  return((1/2)*exp(-abs(x)))
}

#### (1) pdf를 적분해서 cdf로 변환하면
(1/2)*exp(-x) #(0< x < infinity)
(1/2)*exp(x) #(-infinity <x <0)

### (2) 구한 cdf의 역함수를 정의하면

inverse_function <- function(x){
  if(x < 0.5){
    return(-log(2*x))
  }
  if(x > 0.5){
    return(log(2*x))
  }
}

h <- (1.5)*(inverse_function(0.75) - inverse_function(0.25))
sum(c(raplace_func(inverse_function(0.25)-h),raplace_func(inverse_function(0.75) + h)))

