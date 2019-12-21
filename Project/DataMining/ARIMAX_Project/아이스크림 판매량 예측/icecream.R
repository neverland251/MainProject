library("ggplot2")
library("scales")
library(forecast)
library("TSA")
library("aTSA")
library("lmtest")

##Cons : 아이스크림 판매량(인당 파인트)
## income : 가구당 평균소득
## price : 파인트당 달러
## temp : 기간중 평균기온(화씨)


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

# 계절차분
plot(stl(diff(cons,lag = 12),s.window="periodic"))
acf(diff(cons,lag = 12),lag.max = 30)
pacf(diff(cons,lag = 12),lag.max = 30)
adf.test(diff(cons,lag = 12))


## 1계 계절차분을 실시한 결과, 추세와 상수항이 없는 모델의 경우 0.08 로 낮은 수치가 나왔다.


# ARIMA(1,0,0)(0,1,0,12)으로 적합을 시도하고, 그 때의 잔차를 검정한다.
cons_diff = diff(cons,differences=1)
income_diff = diff(income,differences = 1)
price_diff = diff(price,differences = 1)
temp_diff = diff(temp,differences = 1)

grangertest(cons_diff~income_diff,order=3)
grangertest(income_diff ~ cons_diff,order=3)

grangertest(cons_diff ~ price_diff,order=3)
grangertest(price_diff ~ cons_diff,order=3)

grangertest(cons_diff ~ temp_diff,order=3)
grangertest(temp_diff ~ cons_diff,order=3)

a <- arima(cons_diff, order=c(1,0,0))
tsdisplay(residuals(a), lag.max=15, main='PAX ARIMA(1,1,0)')
#BOX-Ljung 테스트 결과 잔차들이 서로 독립적이라는 가설을 기각하지 못했다. 따라서 잔차들은 서로 독립적이며 모형이 잘 적합됬음을 확인할 수 있따.
Box.test(resid(a),type="Ljung")


# CCF 확인
b = arima(income_diff,order=c(1,0,0),fixed=c(a$coef))
c = arima(price_diff,order=c(1,0,0),fixed=c(a$coef))
d = arima(temp_diff, order=c(1,0,0),fixed=c(a$coef))
# CCF 확인을 위해 잔차들만 따로 저장을 해준다.

## 판매량의의 잔차
Consumer <- a$residuals
## 수익의 잔차
Incomes <- b$residuals
## 가격의 잔차
Prices <- c$residuals
## 온도의 잔차
Temperature <- d$residuals

# 0차시 이후 유의미한 관계 없음
ccf(Consumer,Incomes,lag.max = 30)
# 0차시 이후 유의미한 관계 없음
ccf(Consumer,Prices,lag.max = 30)
ccf(Consumer,Temperature,lag.max = 30)
## ccf 확인 결과, 지연모수는 2, 산출계열 1 또는 2를 추정 가능하다.(fixed 활용)

su1 = ts.intersect(cons_seasonal,temp_seasonal)

su1.fit <- arimax(su1[,1],order=c(1,0,0),xtransf=su1[,2],transfer=list(c(5,0)),fixed=c(NA,NA,0,0,0,0,NA,NA))
su1.fit
su1res <- su1.fit$residuals

plot(stl(su1res,s.window="periodic"))
# 잔차의 단위근 검정
adf.test(su1res)
# 잔차의 시계열성 검정
Box.test(su1res,type="Ljung")
# 잔차와 입력 시계열의 CCF 검정
ccf(temp_diff,su1res)

acf(su1res)
pacf(su1res)
ccf(temp_diff,su1res)
# 잔차는 단위근이 존재하지 않고, 서로 시계열적인 상관성도 없다. 즉, 모형은 잘 적합되었고 추가적인 선형필터 적합은 필요하지 않다.


## 예측값 한차시 생성
acf(temp,lag.max = 30)
pacf(temp,lag.max = 30)

#temp는 추세는 없지만 상수항은 있는 모델을 따른다.
plot(stl(temp,s.window="periodic"))
adf.test(diff(temp,lag=12))

# adf 테스트 결과 temp엔 단위근이 존재한다. 계절차분보단 1계 일반차분을 실시하자.

model1 <- arima(temp_diff,order=c(1,0,0))
model1res <- model1$residuals

acf(model1res)
pacf(model1res)
# 잔차는 완전한 백색잡음이다.
adf.test(model1res)
Box.test(model1res,type="Ljung")

## 모형은 잘 적합되었다.

temp_fore <- predict(model1, n.ahead = 5)

final <- su1.fit$coef[2] + su1.fit$coef[1] * cons_diff[24] - su1.fit$coef[7] * cons_diff[20] + su1.fit$coef[8] * temp_fore$pred[1]
final_vec <- c(final)

ytransf_i <- 20
current_i <- 25

for (i in seq(1,4)){
  if (current_i - (ytransf_i + i) > 0){
    final <- su1.fit$coef[2] + su1.fit$coef[1] * final - su1.fit$coef[7] * cons_diff[ytransf_i + i] + su1.fit$coef[8] * temp_fore$pred[i + 1]
    final_vec <- c(final_vec,final)
    print(final)
  }
  else {
    final <- su1.fit$coef[2] + su1.fit$coef[1] * final - su1.fit$coef[7] * final_vec[abs(current_i - (ytransf_i + i)) + 1] + su1.fit$coef[8] * temp_fore$pred[i + 1]
    final_vec <- c(final_vec,final)
    print(final)
  }
}


#시점별 오차율

((series[26:30,"cons"] - (series[25:29,"cons"] + final_vec))/series[26:30,"cons"])*100



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
