library("ggplot2")
library("scales")

##PAX : 방한관광객수
## Cur : 원 - 엔 환율
## IAA : 산업생산지수
## FRI_ab : 국가 우호도 조사에서의 우호도 : 한국을 매우 좋아한다(5점 중 5점)

#데이터 파일을 불러온다.
series <- read.table(file="PAXyear.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
print(series)

series_test <- series

str(series_test)

# 타겟 변수들의 시계열 변수로 재정의한다.
PAX <- ts(series_test$PAX, start = min(series_test$year), end = 2016)
fri_ab <- ts(series_test$Fri_ab, start = min(series_test$year), end = 2016)
Cur <- ts(log(series$Cur), start = min(series_test$year), end = 2016)

# 그래프를 그려본다.
plot(PAX, ylab = "PAX", xlab = "Year")

#방한관광객수 변수의 자기상관함수 그래프를 그린다.
acf(PAX)
## 확인결과 지수함수적으로 감소하지 않아 이 시계열의 공분산이 일정해야한다는 시계열 분석의 기본 가정을 만족하지 못하는 것으로 의심할 수 있다.
library(forecast)
ndiffs(x = PAX)
plot(diff(PAX,1))
acf(diff(PAX,1))
pacf(diff(PAX,1))
## 실제로, 단위근 검정 결과 이를 기각하지 못해서 추가적인 차분이 필요한 것으로 나타났다.

#1계 차분을 실시한다.
PAX_1 <- diff(PAX, differences = 1)

#차분한 시계열의 acf와 pacf 그래프를 그려본다.
acf(PAX_1)
pacf(PAX_1)

# auto arima로 적합해본 결과 MA(1)을 추천하였다. 실제로, ARIMA(1,1,1)등 다양한 방법으로 적합을 시도했을때 AR(1)이 가장 BIC가 우수하였다.
PAX_fit <- auto.arima(PAX_1)
PAX_fit

# ARIMA(1,1,0)으로 적합을 시도하고, 그 때의 잔차를 검정한다.
a <- arima(log(PAX), order=c(0,1,1))
tsdisplay(residuals(a), lag.max=15, main='PAX ARIMA(1,1,0)')
#BOX-Ljung 테스트 결과 잔차들이 서로 독립적이라는 가설을 기각하지 못했다. 따라서 잔차들은 서로 독립적이며 모형이 잘 적합됬음을 확인할 수 있따.
Box.test(resid(a),type="Ljung",lag=20,fitdf=1)

PAX_diff_log <- diff(log(PAX),differences = 1)


# 국가 우호도(fri_ab) 또한 차분을 실시해준다. 
fri_ab_diff = diff(fri_ab,difference = 1)
Cur_diff <- diff(Cur,diffrences=1)
# CCF 확인을 위해 타겟변수인 "방한 관광객수"의 시계열 구조(ARIMA(1,1,0))과 그때의 계수들을 고정시켜놓고 ARIMA모형을 적합한다.
b = arima(fri_ab, order=c(1,1,0), fixed=c(a$coef))
c = arima(Cur, order=c(1,1,0), fixed=c(a$coef))

# CCF 확인을 위해 잔차들만 따로 저장을 해준다.
## 방한관광객수의 잔차
y <- a$residuals
## 국가우호도의 잔차
x <- b$residuals
## 환율의 잔차
z <- c$residuals

ccf(y,x)
ccf(y,z)
## ccf 확인 결과, 방한관광객 - 국가 우호도는 상관성이 없었고, 방한관광객 - 환율은 상관성이 있었다.
## 또한, 0차시에 최초의 상관관계까 나타났으므로 지연모수 b=0으로 추정하고, 투입계열 모수는 s는 1차시와 5차시에서 최초 상관관계가 나타났으므로, 모수 절약
## 원칙에 따라 1을 선택해준다. 또한, 1차시 이후로 5차시까지 상관관계가 지속적으로 줄어드는 형태로 보여, 산출계열 모수 r은 5로 추정할 수 있으나, 1차시 이후로
## 상관관계가 절단되었다고 볼 수 있다면 산출계열 모수 r은 0으로 추정할 수 있다.
##

## CCF 결과에 따라 투입계열 Xt의 1차시 변수를 포함하도록 데이터프레임을 만든다.
### 투입계열 모수 1, 산출계열 모수 1, 즉 ARIMAX s = 1, r = 1
su1 = ts.intersect(PAX_diff_log,Cur_diff,Cur_diffd1=lag(Cur_diff,-1))
### 투입계열 모수 1, 산출계열 모수 0, 즉 ARIMAX s = 1, r = 0 
su2 = ts.intersect(PAX_diff_log, pax_diffd1=lag(PAX_diff_log,-1),Cur_diff)

summary(su2.fit <- lm(PAX_diff_log+ pax_diffd1~ 0+Cur_diff,data=su2))
summary(su1.fit <- lm(PAX_diff_log~0+Cur_diffd1,data=su1))

### 투입계열 모수1, 산출계열 모수0의 경우가 BIC기준 더 안정적이므로, 이 모델을 수립한다.

## 임시로 모형을 적합했으므로, 이제 잔차들의 ARMA 구조를 파악하여 잔차를 남김없이 여과해준다.
su1fit <- lm(PAX_diff_log~0+Cur_diffd1,data=su1)

## 적합한 모형의 잔차들만 따로 저장해준다.
su1res <- su1fit$residuals
su1res <- ts(su1res,start = 1989, end = 2016)


## 잔차들의 acf와 pacf 구조를 보건데, 이 잔차들은 ACF는 상관관계가 없고 PACF가 6차시 이후로 절단되었으므로, ARMA(0,6)구조를 잠정 추정해볼 수 있다.
plot(su1res)
acf(su1res)
pacf(su1res)

## 모형 적합용 데이터셋을 설정한다. 타겟변수인 PAX_diff와 환율의 1차시 뒤 값, 잔차들의 1~6차시 뒤 값들을 하나로 모아준다. 
fitted = ts.intersect(PAX_diff_log ,Cur_diffd1=lag(Cur_diff,-1),su1resd1=lag(su1res,-1),su1resd2=lag(su1res,-2),su1resd3=lag(su1res,-3),su1resd4=lag(su1res,-4),su1resd5=lag(su1res,-5),su1resd6=lag(su1res,-6))


library("TSA")
## 환율을 auto.arima로 적합한다.
model1 <- auto.arima(Cur_diff)
## 2017년 값을 예측해준 후, 이를 cur_diff_fore 데이터셋에 담는다.
cur_diff_fore <- predict(model1, h = 1)
cur_diff_fore <-append(Cur_diff, cur_diff_fore$pred[1])
cur_diff_fore <- ts(cur_diff_fore, end = 2017)

## 잔차들을 AR(6)으로 1차시 예측해준 후, su1res_fore에 담아준다.
su1res_fore <- predict(arima(su1res, order=c(6,0,0),include.mean=FALSE),h=1)
su1res_fore <- append(su1res, su1res_fore$pred[1],after = length(su1res))
su1res_fore <- ts(su1res_fore, start = 1989, end = 2017)

## 적합용 데이터셋을 설정한다. 2017년 예측 생산값이 담긴 fore데이터셋에서 환율, 잔차들의 1~6차시 뒤 값들을 하나로 모아준다.
pred <- ts.intersect(cur_diff_fore, su1resd1=lag(su1res_fore,-1),su1resd2=lag(su1res_fore,-2),su1resd3=lag(su1res_fore,-3),su1resd4=lag(su1res_fore,-4),su1resd5=lag(su1res_fore,-5),su1resd6=lag(su1res_fore,-6),su1res_fore)

## 예측용 데이터셋을 생성한다. 2017년 예측을 위해 앞전 단계에서 1차시 예측 생산한 환율과, 잔차들의 데이터를 담는다.
fitt <- ts(data.frame(pred[23,1],pred[23,8],pred[23,2],pred[23,3],pred[23,4],pred[23,5],pred[23,6]),names=c("cur_diff_fore","su1resd1","su1resd2","su1resd3","su1resd4","su1resd5","su1resd6"))

## arimax 모형을 적합한다. Y의 차수를 ARIMA(1,0,0)으로, "환율"을 xreg로 회귀분석으로 적합하고, 방금 적합한 AR(6)의 잔차는 
#xtransf에 투입한 후에 그 차수는 ARMA(6,0)으로 최종적으로 필터ㄹ해준다.(model1)
model1 <- arimax(fitted[,1],order=c(1,0,0),xreg=pred[1:22,1],xtransf=pred[1:22,8],transfer=list(c(6,0)))
Box.test(model2$residuals,type = c("Ljung-Box"))

## 한편, 잔차를 필터로 처리하지 않고, 잔차까지 단순하게 투입변수로 취급한 적합모형도 테스트해준다(model2)
model2 <- arimax(fitted[,1],order=c(1,0,0),xreg=pred[1:22,0:7])
Box.test(model1$residuals,type = c("Ljung-Box"))
## 단순하게 투입변수로 취급한 모형이 AIC, 로그우도 모두 우수하게 나타났다.

final <- predict(model1,newxreg=fitt,n.ahead=1)

exp(14.647503+final$pred)
# 실측값 2311447과 0.8% 정확도로 예측하였다.
