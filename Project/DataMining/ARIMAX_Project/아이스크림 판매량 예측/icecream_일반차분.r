library("ggplot2")
library("scales")
library(forecast)
library("TSA")
library("aTSA")

##PAX : 방한관광객수
## Cur : 원 - 엔 환율
## IAA : 산업생산지수
## FRI_ab : 국가 우호도 조사에서의 우호도 : 한국을 매우 좋아한다(5점 중 5점)

#데이터 파일을 불러온다.
series <- read.table(file="C://Users//never//OneDrive//문서//2019 하반기 자소서//포트폴리오//졸업논문//icecream.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
print(series)

series_test <- series[0:25,]

str(series_test)

# 타겟 변수들의 시계열 변수로 재정의한다.
cons <- ts(series_test$cons,frequency=12)
income <- ts(series_test$income,frequency=12)
price <- ts(series_test$price,frequency=12)
temp <- ts(series_test$temp,frequency=12)

# 그래프를 그려본다.

plot(cons, ylab = "Consumption")
plot(stl(cons,s.window="periodic"))


#자기상관함수 그래프를 그린다.
acf(cons)
pacf(cons)
adf.test(cons)
## 확인결과 지수함수적으로 감소하지 않아 이 시계열의 공분산이 일정해야한다는 시계열 분석의 기본 가정을 만족하지 못하는 것으로 의심할 수 있다.


#그냥 일반차분
plot(diff(cons,differences = 1))
acf(diff(cons,differences = 1),lag.max = 30)
pacf(diff(cons,differences = 1),lag.max = 30)
adf.test(diff(cons,differences = 1))

## 1계 계절차분을 실시한 결과, 추세와 상수항이 없는 모델의 경우 


# ARIMA(1,1,0)으로 적합을 시도하고, 그 때의 잔차를 검정한다.
a <- arima(cons,order=c(1,1,0))
tsdisplay(residuals(a), lag.max=15, main='PAX ARIMA(1,1,0)')
#BOX-Ljung 테스트 결과 잔차들이 서로 독립적이라는 가설을 기각하지 못했다. 따라서 잔차들은 서로 독립적이며 모형이 잘 적합됬음을 확인할 수 있따.
Box.test(resid(a),type="Ljung",lag=20)


# CCF 확인
b = arima(income,order=c(1,1,0),fixed=c(a$coef))
c = arima(price,order=c(1,1,0),fixed=c(a$coef))
d = arima(temp,order=c(1,1,0),fixed=c(a$coef))

# CCF 확인을 위해 잔차들만 따로 저장을 해준다.

## 판매량의의 잔차
y <- a$residuals
## 수익의 잔차
x <- b$residuals
## 가격의 잔차
z <- c$residuals
## 온도의 잔차
k <- d$residuals

# 0차시 이후 유의미한 관계 없음
ccf(y,x,lag.max = 30)
# 0차시 이후 유의미한 관계 없음
ccf(y,z,lag.max = 30)
ccf(y,k,lag.max = 30)
## ccf 확인 결과, 지연모수는 2, 산출계열 1 또는 2를 추정 가능하다.(fixed 활용)

su1 = ts.intersect(diff(cons,differences = 1),diff(temp,differences = 1))
su1 = ts.intersect(cons,temp)

su1.fit <- arimax(su1[,1],order=c(1,0,0),xtransf=su1[,2],transfer=list(c(3,0)))
su1.fit
su1res <- su1.fit$residuals

plot(stl(su1res,s.window="periodic"))
# 잔차의 단위근 검정
adf.test(su1res)
# 잔차의 시계열성 검정
Box.test(su1res,type="Ljung",lag=20)
# 잔차와 입력 시계열의 CCF 검정
ccf(temp,su1res)

# 잔차는 단위근이 존재하지 않고, 서로 시계열적인 상관성도 없다. 즉, 모형은 잘 적합되었고 추가적인 선형필터 적합은 필요하지 않다.


## 예측값 한차시 생성
acf(temp,lag.max = 30)
pacf(temp,lag.max = 30)

#temp는 추세는 없지만 상수항은 있는 모델을 따른다.
plot(stl(temp,s.window="periodic"))
adf.test(temp)

# adf 테스트 결과 temp는 계절성이 명백히 존재한다. 일단 계절차분부터 시도해보자.

model1 <- arima(temp,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12))
model1res <- model1$residuals

acf(model1res)
pacf(model1res)
# 잔차는 완전한 백색잡음이다.
adf.test(model1res)
Box.test(model1res,type="Ljung")

## 모형은 잘 적합되었다.

temp_fore <- predict(model1, n.ahead = 5)

final <- su1.fit$coef[2] + su1.fit$coef[1] * diff(cons,differences=1)[24] + (su1.fit$coef[6]/(1-su1.fit$coef[3]-su1.fit$coef[4]-su1.fit$coef[5])) * temp_fore$pred[1]
final_vec <- c(final)

for (i in seq(2,5)){
  final <- su1.fit$coef[2] + su1.fit$coef[1] * final + (su1.fit$coef[6]/(1-su1.fit$coef[3]-su1.fit$coef[4]-su1.fit$coef[5])) * temp_fore$pred[i]
  final_vec <- c(final_vec,final) 
}


#시점별 오차율

(series[26:30,"cons"] - (series[26:30,"cons"] + final_vec))/series[26:30,"cons"]*100





# 서울대 레퍼런스

transf_frame <- data.frame(su1,su1res)

auto.arima(su1.fit$residuals)


su1res_fore <- predict(auto.arima(su1res),h=1)
su1res_fore <- append(su1res, su1res_fore$pred[1])
su1res_fore <- ts(su1res_fore,end = 2017)


model1 <- auto.arima(cons_diff)
## 2017년 값을 예측해준 후, 이를 cur_diff_fore 데이터셋에 담는다.
cons_diff_fore <- predict(model1, h = 1)
cons_diff_fore <- append(cons_diff, cons_diff_fore$pred[1])
cons_diff_fore <- ts(cons_diff_fore, end = 2017)



transf_fore <- data.frame(cons_diff_fore,su1res_fore)
colnames(transf_fore) <- colnames(transf_frame[,2:3])

final <- predict(su1.fit,xreg=transf_fore,n.ahead=1)

cons[29] + diff(cons,differences=1)[28] + final$pred[1]

(series[30,"cons"] - (cons[29] + diff(cons,differences=1)[28] + final$pred[1]))/series[30,"cons"]*100
