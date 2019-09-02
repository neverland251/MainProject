# 로지스틱 회귀
acs <- read.table("http://jaredlander.com/data/acs_ny.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
head(acs)

##연소득이 150000 이하인 가정은 True, 그렇지 않은 가정은 False로 만들어 이진화한다.
acs$Income <- acs$FamilyIncome < 150000 

## 로지스틱 회귀 모형을 적합한다. 확률분포로 이항분포를 사용하면서, 그에 대한 변수는 로짓과 연결해 이를 미분, 경사하강법으로 기울기가 최소가 되는 지점을 고른다.
house1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType,data = acs ,family = binomial(link = "logit"))

## 로지스틱 회귀의 계수 추정을 위해선 로짓함수의 역함수가 필요하다.

invlogit <- function(x){
  1 - 1/(1+exp(-x))
}

invlogit(house1$coefficients)

# 로지스틱 회귀(연결 함수로 포아송 분포를 사용)

ggplot(acs, aes(x=NumChildren)) + geom_histogram(binwidth = 1)

##타겟변수의 분포가 포아송분포를 보이는것을 확인할 수 있다.

house2 <- glm(NumChildren ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType,data = acs ,family = poisson(link = "log"))

## 포아송 회귀를 사용할 때, 포아송 분포보다 변이가 더 크게 일어나는 과대산포를 확인해본다.

### 과대산포 확인 OD = sig(z)/df , z = 표준화된 잔차(y-yhat/sd(yhat))

### 표준화된 잔차
z = (acs$NumChildren - house2$fitted.values)/sqrt(house2$fitted.values)
### sum
sig = sum(z^2)
### 과대산포 확인. 이 값이 2를 넘어가면 과대산포를 의심해볼 수 있다.
od = sig/house2$df.residual
### 하지만, 카이제곱 분포와 확률값이 1로 과대산포가 유의함을 시사한다.
pchisq(sig,house2$df.residual)


# 생존분석

library(survival)
head(bladder)
## stop은 연구에서 제외된 날짜를, event는 그 시점에서 특정 이벤트가 발생했음을, rx는 플라시보 여부를 나타낸다.
## Surv는 특정 기간 이후로도 계속 생존했는지, 사망했는지를 +를 붙여 나타내준다.(+가 붙어 있을경우 생존)
survObject <- with(bladder[100:105,],Surv(stop,event))
## 콕스 비례 위험 모형으로 이를 모델링한다.
cox1 <- coxph(Surv(stop,event) ~ rx+number+size+enum,data=bladder)
summary(cox1)
## 기간별 생존 비율을 그래프르 보여준다. 
plot(survfit(cox1),xlab = "Days",ylab = "Survival Rate",conf.int= TRUE)
## 기간별 생존 비율을 보는데, 플라시보 효과를 고려하여 그래프를 나눌수도 있다.
cox2 <- coxph(Surv(stop,event) ~ strata(rx) + number + size + enum,data=bladder)
plot(survfit(cox2),conf.int = TRUE,col = 1:2)
