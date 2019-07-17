#p.231
## 1. 해당 p의 히스토그램과 gamma(1,ceta)일 때 ceta의 최대우도추정값을 구하라
p <- c(1,4,5,21,22,28,40,42,51,53,58,67,95,124,124,160,202,260,303,363)
hist(p,h=25)

hist(beta(1,1))

## ceta^(-1)*e^(-p*ceta^(-1))의 결합분포는
## rank(ceta^(-1))*e^(-sum(p)*ceta^(-1))과 같다.
## 이를 log를 붙여 풀어주면 sum(log(ceta^-1)) + sum(-sum(p)^ceta^(-1))
## 이를 미분하면 sum(p)/length(p) = ceta가 된다.
mle = sum(p)/length(p)
print(mle)

#p.239

## 1. N(mu,80), n = 20, mu = 81.2, sig = 26.5, 이때, 신뢰계수 90%, 95%, 99%의 신뢰구간을 구하면?
### mu - t(0.9,19) * (root(26.5)/root(n))
lower = 81.2 - qt(0.05,19)*(26.5^(1/2)/20^(1/2))
upper = 81.2 + qt(0.05,19)*(26.5^(1/2)/20^(1/2))
print(c(lower,upper))

### mu - t(0.95,19) * (root(26.5)/root(n))
lower = 81.2 - qt(0.025,19)*(26.5^(1/2)/20^(1/2))
upper = 81.2 + qt(0.025,19)*(26.5^(1/2)/20^(1/2))
print(c(lower,upper))

### mu - t(0.95,19) * (root(26.5)/root(n))
lower = 81.2 - qt(0.0125,19)*(26.5^(1/2)/20^(1/2))
upper = 81.2 + qt(0.0125,19)*(26.5^(1/2)/20^(1/2))
print(c(lower,upper))

## 2. X1...Xn이 N(mu,sig^2)에서 추출한 9인 확률표본
### 1. sig가 알려진경우, 신뢰구간이 9^(1/2)*(X-mu)/sig에 근거를 두고 있다면 95% 신뢰구간을 구하여라
### 이는 t-score를 따르는 통계치이므로, 자유도=8, p = 0.025의 95% 구간을 구하면

lower = "mu" - qt(0.95,8)*"sig"/8^(1/2)
upper = "mu" + qt(0.95,8)*"sig"/8^(1/2)

t = qt(0.025,8)

### 2. sig가 알려져있지 않은 경우, 신뢰구간이 9^(1/2)*(X-mu)/sig에 근거를 두고 있다면 95% 신뢰구간의 길이의 기댓값을 구하여라
##
2*qt(0.95,8)/3*"expectation(S)"

### 3. Y가 b(300,p)를 따른다고 하자. Y의 관찰값이 y=75일 때, p에 대한 90% 근사 신뢰구간을 구하시오
p <- 75/300
c(p - qnorm(0.05)*(p*(1-p)/300)^(1/2),p + qnorm(0.05)*(p*(1-p)/300)^(1/2))
#### 이것이 바로 비율에 대한 90% 신뢰구간이다.

### 4. 모수 mu와 sig가 알려지지 않은 정규분포(mu,sig)에서 추출한 확률표본을 X1...Xn이라고 하자.
#### 1) sig^2에 대한 90% 신뢰구간을 구하시오.(단, n = 9, s^2 = 7.93이다.)
##### (1) (n-1)s^2/sig^2은 chisq(r-1)을 따른다고 알려져 있다. 이를 이용하면
n <- 9
s_2 <- 7.93
a <- qchisq(0.05,n-1)
b <- qchisq(0.95,n-1)

c(a,b)
##### (n-1)s^2/sig^2는 c(a,b) 사이에 존재한다.
##### (2) 이를 sig^2가 가운데에 오도록 식을 고치면

c(((n-1)*s_2)/b,((n-1)*s_2)/a)

##### sig^2는 이 구간 내에 존재한다.

### 5. 모수가 n1 = n2 = 100인 확률 p1,p2를 따르는 이항분포를 따르는 Y1,Y2의 관찰값인 각각 y1 = 50, y2 = 40이라고 한다.
#### 1) p1 - p2에 대한 90% 근사 확률구간을 정의하시오

n1 <- 100
n2 <- 100
y1 <- 50
y2 <- 40

upper <- ((y1/n1) - (y2/n2)) + qnorm(0.05)*(((y1/n1)/n1)+((y2/n2)/n2))^(1/2)
lower <- ((y1/n1) - (y2/n2)) - qnorm(0.05)*(((y1/n1)/n1)+((y2/n2)/n2))^(1/2)

c(lower,upper)

# 이것이 95% 신뢰구간이 된다.
