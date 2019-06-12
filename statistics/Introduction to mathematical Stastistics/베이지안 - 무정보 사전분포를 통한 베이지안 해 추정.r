# 1. X1, X2를 pdf가 
# (1/pi) * (a/(a^2+(x-b)^2)) 인 코시분포에서 추출한 확률표본이라고 하자.

## 1) 무정보 사전표본의 pdfㄹ h(a,b) = 1이라고 할 때, 사후 pdf를 구하시오
### 이변량 확률변수이므로를, 일ㄷ 두 확률변수의 결합우도함수를 정의한다. 이를 손으로 풀면
# (1/pi)^2 * (a^2/((a^2+(x-b)^2) * (a^2 + (y-b)^2)))
### 가 도출된다. 사전표본의 pdf는 h(a,b) = 1이므로, 사후 pdf는 L(a,b|x)*h(a,b) = L(a,b|x) * 1과 비례관계에 있다. 즉 
# (a^2/((a^2+(x-b)^2) * (a^2 + (y-b)^2)) 
### 와 비례관계에 있다.

## 2) 평가 대상인 모수의 목록이 다음과 같다.

a <- c(1,2,3,4)
b <- c(0.5,1,1.5,2)

## x = 1, y = 4일때 사후 pdf를 평가하시오

x <- 1
y <- 4

result <- data.frame()

for(i in a){
  for(j in b){
    result <- rbind(result,data.frame("a" = i^2/((i^2+(x-j)^2) * (i^2 + (y-j)^2)), "b" = i, "c" = j))
  }
}

# 평가 결과, b = 1, c = 1.5일때 pdf의 확률이 최대가 된다.

# 2. X1...X10을 pdf가 gamma(3,1/a)인 감마분포를 따른다고 하고, 모수 a에 대한 사전분포는 gamma(10,2)를 따름이 알려졌다고 하자.

## 1) 사후분포 pdf를 구하여라

### (1) 우선, 감마분포는 가법성을 가지므로 10개의 결합 pdf는 gamma(3 * 10, 1/a)의 분포를 따른다.
### (2) 한편, 사후분포는 gamma(10,2)를 따르는 모수a에 대한 분포이므로, 이 둘을 결합하여 정리해주면

(prod(x)/gamma(30))*(a^(30+10-1)*exp(a*(1/2+length(x)*mean(x)))) 이다.

### (3) 위 식에서, 필요없는 상수항인 prod(x)/gamma(30)을 제외하고, 켤레분포족인 감마분포의 pdf에 대한 식만 남기면
(a^(30+10-1)*exp(a*(1/2+length(x)*mean(x))))
### 사후 pdf는 위의 식과 비례관계에 있다.

## 2) mean(x) = 18.2로 밝혀졌다. 손실함수롤 mse = (a - mean(x))^2로 정의할 때, 손실함수의 위험함수를 최소화하는 베이지안의 추정해는 무엇인가?
### (1) 위험함수는 손실함수의 사후분포CDF에서의 기댓값을 최소화하는 값이다. 
### (2) 따라서, 손실함수가 MSE로 지정된 경우, 그 베이지안 추정해는 사후분포의 평균이 적절한 추정량이 된다.
### (3) 앞서 구한 사후분포의 pdf는 gamma(40, 1/(1/2+length(x)*mean(x))) 를 따르는 감마분포족의 비례 관계이므로, 감마분포의 평균은 

40*(1/(1/2+length(x)*mean(x))) 
###가 된다. 
### (4) 여기서, length(x) = 10이고, mean(x) = 18.2이므로, 이를 통해 사후분포의 평균을 구하면

print(40/((1/2) + 10*(18.2)))

### 가 손실함수를 최소화하는 베이지안의 추정해가 된다.
