library("ggplot2")
library("scales")
library("forecast")
library("TSA")

##PAX : 방한관광객수
## Cur : 원 - 엔 환율
## IAA : 산업생산지수
## FRI_ab : 국가 우호도 조사에서의 우호도 : 한국을 매우 좋아한다(5점 중 5점)

#데이터 파일을 불러온다.
series <- read.table(file="C://Users//never//OneDrive//문서//GitHub//MainProject//Project//DataMining//ARIMAX_Project//방한관광객수 예측//PAXyear.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
print(series)

series_test <- series[1:25,]

str(series_test)

# 타겟 변수들의 시계열 변수로 재정의한다.
PAX <- ts(log(series_test$PAX), start = min(series_test$year), end = 2012)
fri_ab <- ts(series_test$Fri_ab, start = min(series_test$year), end = 2012)
Cur <- ts(log(series$Cur), start = min(series_test$year), end = 2012)

# 그래프를 그려본다.
plot(PAX, ylab = "PAX", xlab = "Year")

#방한관광객수 변수의 자기상관함수 그래프를 그린다.
acf(PAX)
pacf(PAX)
adf.test(PAX)
## 확인결과 지수함수적으로 감소하지 않아 이 시계열의 공분산이 일정해야한다는 시계열 분석의 기본 가정을 만족하지 못하는 것으로 의심할 수 있다.

ndiffs(x = PAX)
plot(diff(PAX,1))
acf(diff(PAX,1))
pacf(diff(PAX,1))
adf.test(diff(PAX,differences=1))
## 실제로, 단위근 검정 결과 이를 기각하지 못해서 추가적인 차분이 필요한 것으로 나타났다.

#1계 차분을 실시한다.
PAX_1 <- diff(PAX, differences = 1)




# ARIMA(1,1,0)으로 적합을 시도하고, 그 때의 잔차를 검정한다.
PAX_fit <- arima(PAX_1,order=c(1,0,0))
PAX_fit
acf(resid(PAX_fit))
pacf(resid(PAX_fit))
#BOX-Ljung 테스트 결과 잔차들이 서로 독립적이라는 가설을 기각하지 못했다. 따라서 잔차들은 서로 독립적이며 모형이 잘 적합됬음을 확인할 수 있따.
Box.test(resid(PAX_fit),type="Ljung",lag=20,fitdf=1)




# 국가 우호도(fri_ab) 또한 차분을 실시해준다. 
fri_ab_diff = diff(fri_ab,difference = 1)
Cur_diff <- diff(Cur,diffrences=1)
# CCF 확인을 위해 타겟변수인 "방한 관광객수"의 시계열 구조(ARIMA(1,1,0))과 그때의 계수들을 고정시켜놓고 ARIMA모형을 적합한다.
b = arima(fri_ab_diff, order=c(1,0,0), fixed=c(PAX_fit$coef))
c = arima(Cur_diff, order=c(1,0,0), fixed=c(PAX_fit$coef))

grangertest(PAX_1 ~ fri_ab_diff,order=4)
grangertest(fri_ab_diff ~ PAX_1,order=4)

grangertest(PAX_1 ~ Cur_diff, order=4)
grangertest(Cur_diff ~ PAX_1,order=4)

# CCF 확인을 위해 잔차들만 따로 저장을 해준다.
## 방한관광객수의 잔차
PAXs <- PAX_fit$residuals
## 국가우호도의 잔차
Likes <- b$residuals
## 환율의 잔차
Curs <- c$residuals

ccf(Curs,PAXs,lag.max=30)
# 지연모수 3, 투입모수 0, 산출모수 Only 2
ccf(Likes,PAXs,lag.max = 30)
# 지연모수 8, 투입모수 0, 산출모수 0

Cur_diff_shift <- ts(Cur_diff[1:(24-4)])
PAX_1_shift <- ts(PAX_1[4:24])
su1 = ts.intersect(PAX_1_shift,Cur_diff_shift)

su1.fit <- arimax(su1[,1],order=c(1,0,0),xtransf=su1[,2],transfer=list(c(2,0)),include.mean = FALSE,fixed=c(NA,0,NA,NA))


## 적합한 모형의 잔차들만 따로 저장해준다.
su1res <- su1.fit$residuals
su1res <- ts(su1res)

plot(su1res)
acf(su1res)
pacf(su1res)
Box.test(su1res,type=c("Ljung-Box"))

#추가적인 ARMA 선형필터 적합은 필요 없어보인다.

su1.fit <- arimax(su1[,1],order=c(1,0,0),xtransf=su1[,2],transfer=list(c(2,0)),fixed=c(NA,NA,0,NA,NA))

su1res <- su1.fit$residuals
su1res <- ts(su1res)
Likes <- ts(Likes)
ccf(su1res,Cur_diff_shift,lag.max=30)

#잔차를 이용하여 두 번째 투입변수인 Likes도 남김없이 적용할 수 있는지, 잔차와 Likes의 cCF를 살펴본다.

ccf(su1res,Likes,lag.max=30)

Likes_shift <- ts(Likes[3:22])
su2 = ts.intersect(su1,Likes_shift)

su2.fit <- arimax(su1[,1],order=c(1,0,0),xtransf=data.frame(su2[,2],su2[,3]),transfer=list(c(2,0),c(0,0)),fixed=c(NA,NA,0,NA,NA,NA))

su2res <- su2.fit$residuals
su2res <- ts(su2res)
Box.test(su2res,type=c("Ljung-Box"))
acf(su2res,lag.max=30)
pacf(su2res,lag.max=30)
ccf(su2res,Likes_shift,lag.max=30)

# 예측값 생성

adf.test(Cur)
acf(Cur)
pacf(Cur)

adf.test(Cur_diff_shift)
acf(Cur_diff_shift)
pacf(Cur_diff_shift)

model1 <- arima(Cur_diff_shift,order=c(1,0,0))
model1res <- model1$residuals
model1res <- ts(model1res)
acf(model1res)
pacf(model1res)
Box.test(model1res,type=c("Ljung-Box"))

adf.test(fri_ab)
acf(fri_ab)
pacf(fri_ab)

adf.test(Likes_shift)
acf(Likes_shift,lag.max=30)
pacf(Likes_shift,lag.max=30)

model2 <- arima(Likes_shift,order=c(1,0,0))
model2res <- model2$residuals
model2res <- ts(model2res)

acf(model2res)
pacf(model2res)
Box.test(model2res,type=c("Ljung-Box"))
## 2017년 값을 예측해준 후, 이를 cur_diff_fore 데이터셋에 담는다.
cur_fore <- predict(model1, n.ahead = 5)
cur_fore <- ts(cur_fore)

likes_fore <- predict(model2,n.ahead=5)
likes_fore <- ts(likes_fore)

final <- su1.fit$coef[2] + su1.fit$coef[1] * PAX_1_shift[21] - su1.fit$coef[4] * PAX_1_shift[20] + su1.fit$coef[5] * cur_fore$pred[1]
final_vec <- c(final)

ytransf_i <- 20
current_i <- 22

for (i in seq(1,4)){
  if (current_i - (ytransf_i + i) > 0){
    final <- su1.fit$coef[2] + su1.fit$coef[1] * final - su1.fit$coef[4] * PAX_1_shift[ytransf_i + i] + su1.fit$coef[5] * cur_fore$pred[i + 1]
    final_vec <- c(final_vec,final)
    print(final)
  }
  else {
    final <- su1.fit$coef[2] + su1.fit$coef[1] * final - su1.fit$coef[4] * PAX_1_shift[abs(current_i - (ytransf_i + i)) + 1] + su1.fit$coef[5] * cur_fore$pred[i + 1]
    final_vec <- c(final_vec,final)
    print(final)
  }
}

(series$PAX[25:29] - (exp(log(series$PAX)[25:29] + final_vec)))/(series$PAX[25:29]) * 100





final <- su2.fit$coef[2] + su2.fit$coef[1] * PAX_1_shift[21] - su2.fit$coef[4] * PAX_1_shift[20] + su2.fit$coef[5] * cur_fore$pred[1] + su2.fit$coef[6] * likes_fore$pred[1]
final_vec <- c(final)

ytransf_i <- 20
current_i <- 22

for (i in seq(1,4)){
  if (current_i - (ytransf_i + i) > 0){
    final <- su1.fit$coef[2] + su1.fit$coef[1] * final - su1.fit$coef[4] * PAX_1_shift[ytransf_i + i] + su1.fit$coef[5] * cur_diff_fore$pred[i + 1] + su2.fit$coef[6] * likes_fore$pred[i+1]
    final_vec <- c(final_vec,final)
    print(final)
  }
  else {
    final <- su1.fit$coef[2] + su1.fit$coef[1] * final - su1.fit$coef[4] * PAX_1_shift[abs(current_i - (ytransf_i + i)) + 1] + su1.fit$coef[5] * cur_diff_fore$pred[i + 1] + su2.fit$coef[6] * likes_fore$pred[i+1]
    final_vec <- c(final_vec,final)
    print(final)
  }
}

(series$PAX[25:29] - (exp(log(series$PAX)[25:29] + final_vec)))/(series$PAX[25:29]) * 100
