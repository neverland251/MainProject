library("ggplot2")
library("scales")
library("forecast")
library("TSA")

##PAX : 방한관광객수
## Cur : 원 - 엔 환율
## IAA : 산업생산지수
## FRI_ab : 국가 우호도 조사에서의 우호도 : 한국을 매우 좋아한다(5점 중 5점)

#데이터 파일을 불러온다.
series <- read.table(file="C://users//권종익//Documents//MainProject//MainProject//PAXyear.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
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
b = arima(fri_ab, order=c(0,1,1), fixed=c(a$coef))
c = arima(Cur, order=c(0,1,1), fixed=c(a$coef))

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
su1 = ts.intersect(PAX_diff_log,Cur_diff)

su1.fit <- arimax(su1[,1],order=c(0,0,1),xtransf=su1[,2],transfer=list(c(1,0)),include.mean = FALSE)


## 적합한 모형의 잔차들만 따로 저장해준다.
su1res <- su1.fit$residuals
su1res <- ts(su1res)


## 잔차들의 acf와 pacf 구조를 보건데, 이 잔차들은 시계열적인 패턴이 없는 완전한 백색잡음이다. 따라서, ARMA(0,0)을 고려한다.
plot(su1res)
acf(su1res)
pacf(su1res)

transf_frame <- data.frame(Cur_diff,su1res)

su1.fit <- arimax(su1[,1],order=c(0,0,1),xtransf=transf_frame,transfer=list(c(1,0),c(0,0)))
Box.test(su1.fit$residuals,type = c("Ljung-Box"))

## 환율을 auto.arima로 적합한다.
model1 <- auto.arima(Cur_diff)
## 2017년 값을 예측해준 후, 이를 cur_diff_fore 데이터셋에 담는다.
cur_diff_fore <- predict(model1, h = 1)
cur_diff_fore <- append(Cur_diff, cur_diff_fore$pred[1])
cur_diff_fore <- ts(cur_diff_fore,start=1989,end=2017)

model2 <- auto.arima(su1res)
su1res_fore <- predict(model2,h = 1)
su1res_fore <- append(su1res, su1res_fore$pred[1])
su1res_fore <- ts(su1res_fore,start=1989,end=2017)

transf_fore <- data.frame(cur_diff_fore,su1res_fore)
colnames(transf_fore) <- colnames(transf_frame)


final <- predict(su1.fit,xreg=transf_fore[29,],n.ahead=1)

abs((2311447 - exp(14.647503+final$pred[1]))/2311447)*100
# 실측값 2311447과 0.8% 정확도로 예측하였다.
