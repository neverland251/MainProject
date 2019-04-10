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
W <- qnorm(runif(100))*(1-e)+qnorm(runif(100)/sige)*(e/sige)

## 주어진 확률변수로 변환해준다.
X <- 10*W[1:50]+100
Y <- 10*W[51:100]+120

boxplot(X)
boxplot(Y)

## 비교용 검정통계량을 생산한다.

X_bar <- sum(X)/length(X)
Y_bar <- sum(Y)/length(Y)
v <- Y_bar - X_bar

## H0를 기각하는 것이 목적이므로, 두 표본을 하나 합친 확률변수를 생성한다.
Z <- c(X,Y)
## 부트스트랩을 실시한다.
for(i in 0:3000){
  ## 균등분포로부터 10개의 값을 추출하여, 소수점을 절사하고 numb에 담는다.
  X_numb <- round(runif(10,min=1,max=30))
  Y_numb <- round(runif(10,min=1,max=30))
  ## 샘플변수들의 평균을 구한다.
  X_boot <- sum(Z[X_numb])/length(Z[X_numb])
  Y_boot <- sum(Z[Y_numb])/length(Z[Y_numb])
  ## 평균값의 차이를 구한다.
  boot_theta <- Y_boot - X_boot
  ## 해당 값을 일단 저장해준다.
  theta_boot[i] <- boot_theta
}
# 검정통계량인 v값과 부트스트랩된 vj값을 비교할 수 있도록 히스토그램을 그린다.
hist(theta_boot,probability = TRUE)
print(v)
# 이 때, v=20은 부트스트랩된 확률분포의 확률 0.05 오른쪽에 있으므로, H0를 기각하고 둘은 다른 분포임을 확인할 수 있다.

# 3. X1...Xn이 gamma(a=1,b=b)에서 추출한 확률표본이다.
## 1) 신뢰구간 [(2n*X_bar)/chisq(2n))^(1-(a/2)),(2n*X_bar)/chisq(2n))^(a/2)]가 b에 대한 정확한 (1-a)% 신뢰구간임을 보여라
