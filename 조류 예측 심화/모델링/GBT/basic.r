library("ggplot2")
library("scales")
library("forecast")
library("TSA")
library("ggplot2")
library("dplyr")
library("reshape2")
library("magrittr")
library("lubridate")
library("tseries")


## Cell : 총조류수


## temp : 온도
## rain : 강수량
## moment : 최대순간풍속
## velocity : 최대풍속
## hpa : 최고해면기압
## suntime : 하루일조시간
## sun : 일사량
## humidity : 습도


## wt : 수온
## ph : 수소이온농도 : 산성, 염기성 여부. 정상 ph = 7
## ec : 전기전도도 : 깨끗할수록 낮다. 오염될수록 높다.
## ntu : 탁도 : 물의 탁한 정도
## toc : 총유기탄소 : 물에 존재하는 유기 탄소의 양
## tn : 총인 : 물에 존재하는 인의 양
## tp : 총질소 : 물에 존재하는 무기, 유기 질소의 양
## do : 생물학적 산소 요구량

#데이터 파일을 불러온다.
rawdata <- read.table(file="C://Users//23//Desktop//조류 예측//데이터//data_mart_final_V1.2.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
print(rawdata)
rawdata_null_columns <- colSums(is.na(rawdata)) / length(rawdata[,1])
rawdata_null_columns_table <- names(rawdata_null_columns[rawdata_null_columns != 0])

# 결측간 상관관계 분석
## 결측치끼리의 상관관계를 분석합니다.
## 높은 상관관계는 결측 패턴이 서로 비슷하다는 것을 의미합니다.

sort(colSums(is.na(rawdata)) / length(rawdata[,1]),decreasing = T)

rawdata_int_NA <- apply(sapply(rawdata,is.na),2,as.integer)

ggplot(melt(cor(rawdata_int_NA)[rawdata_null_columns_table, rawdata_null_columns_table]),
       aes(x = Var1, y = Var2)) + geom_tile(aes(fill = value)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "white", high = "red"))

names(rawdata_null_columns_over_0.1[rawdata_null_columns_over_0.1 != 0])


# 시간에 따른 남조류수 변화의 그래프
## 차분 전
plot(ts(log(rawdata$cells_mean + 0.00001)),ylab="log(cells/mL)")
## 차분 후
plot(diff(ts(log(rawdata$cells_mean + 0.00001))),ylab="log(cells/mL)")

# acf, pacf 그래프
## 차분 전
plot(acf(log(rawdata$cells_mean + 0.00001)), main = "ACF graph of cell_means")
plot(pacf(log(rawdata$cells_mean + 0.00001)), main = "PACF graph of cell_means")

## 차분 후
plot(acf(diff(log(rawdata$cells_mean + 0.00001))), main = "ACF graph of cell_means")
plot(pacf(diff(log(rawdata$cells_mean + 0.00001))), main = "PACF graph of cell_means")

# Agumented-Dickey-Fuller 테스트와 Box-Ljung test
## ADF test : 시계열의 정상성을 판단하는 검정입니다.
## 귀무가설은 시계열이 정상적이지 않다고, 이를 기각해야 시계열이 정상적이라고 판단합니다.
adf.test(log(rawdata$cells_mean + 0.00001))

## BOx-Ljung 검정 : 변수들이 서로 시계열적으로 연관되었는지 검정합니다
## 귀무가설은 변수의 시계열이 서로 독립이다. 이고 , 귀무가설을 기각하면 시계열성이 있다고 판단합니다..
Box.test(log(rawdata$cells_mean + 0.00001), type=c("Ljung-Box"))

## 차분 후 ADF, box-Ljung

adf.test(diff(log(rawdata$cells_mean
                  + 0.00001)))
Box.test(diff(log(rawdata$cells_mean
                  + 0.00001)), type=c("Ljung-Box"))


# auto-arima에 모든 변수들을 다 때려박은 모델

## auto_arima용 임시 데이터셋을 만듭니다.
rawdata_temp_for_auto_arima <- as.matrix(na.fall(log(rawdata[,3:length(colnames(rawdata))] + 0.00001)))
k <- auto.arima(rawdata_temp_for_auto_arima[,c("cells_mean")],xreg=rawdata_temp_for_auto_arima[,2:length(colnames(rawdata_temp_for_auto_arima))])

## 해당 모형의 잔차로 Box-Test를 해본 결과 시계열이 독립이다라는 귀무가설을 기각하지 못합니다.
## 이는 다시 말해, 모형의 잔차가 서로 독립이다라는 의미이고, 모델이 잘 적합됬음을 의미합니다.
Box.test(k$residuals, type=c("Ljung-Box"))

# boruta 변수 선별

library('Boruta')

rawdata_none_NA <- na.omit(rawdata)
rawdata_none_NA_temp <- na.omit(rawdata)

#rawdata_none_NA_temp[,c("cells_mean")] <- NULL
#rawdata_none_NA_temp[1:(length(rawdata_none_NA_temp[, 1]) - 1), c("cells_mean")] <-
# rawdata_none_NA[2:(length(rawdata_none_NA[, 1])), c("cells_mean")]

Boruta_result <-
  Boruta(cells_mean ~ ., data = rawdata_none_NA_temp[1:(length(rawdata_none_NA_temp[, 1]) - 1), ], maxRuns = 10000)



na_inf <- function(x){
  mean_of_not_inf <- mean(x[is.finite(x)])
  return(mean_of_not_inf)
}

sorting_columns <- sort(apply(Boruta_result$ImpHistory,2,na_inf),decreasing = T)

Boruta_melt <- melt(as.data.frame(Boruta_result$ImpHistory[,names(sorting_columns)]))


#sapply(names(Boruta_result$finalDecision[Boruta_result$finalDecision == "Confirmed"]),making_highlited_columns)

names_of_confirmed <- names(Boruta_result$finalDecision[Boruta_result$finalDecision == "Confirmed"])
names_of_rejected <- names(Boruta_result$finalDecision[Boruta_result$finalDecision == "Rejected"])
names_of_tentative <- names(Boruta_result$finalDecision[Boruta_result$finalDecision == "Tentative"])
names_of_shadow <- c("shadowMax","shadowMean","shadowMin")

Boruta_melt[Boruta_melt$variable %in% names_of_confirmed, c("highlited")] <- "Confirmed"
Boruta_melt[Boruta_melt$variable %in% names_of_rejected, c("highlited")] <- "Rejected"
Boruta_melt[Boruta_melt$variable %in% names_of_tentative, c("highlited")] <- "Tentative"
Boruta_melt[Boruta_melt$variable %in% names_of_shadow,c("highlited")] <- "Shadow"


ggplot(Boruta_melt[Boruta_melt$variable %in% names_of_confirmed, ], aes(x = variable, y = value, fill = highlited)) + geom_boxplot(outlier.shape = NA) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Z-score of Importance")



## 결과는 Boruta_result에 저장되어 있습니다.

names(Boruta_result$finalDecision[Boruta_result$finalDecision == "Confirmed"])


## 교차상관분석

rawdata_sc_temp <- rawdata %>% arrange(year.iso, week.iso)
rawdata_sc <- as.data.frame(scale(rawdata_sc_temp[,3:82]))
rawdata_sc <- bind_cols(rawdata_sc_temp[,1:2], rawdata_sc)

adf.test(rawdata$cells_mean)


## 사전 백색화를 위한 auto.arima 적합
a <- arima(diff(rawdata_sc[1:104,c("cells_mean")]),order=c(1,0,0),include.mean = F)
b <- arima(diff(rawdata_sc[1:104,c("wt_temp_mean")]),order=c(1,0,0),include.mean = F,fixed=a$coef)

Box.test(b$residuals,type=c("Ljung-Box"))

## 교차상관함수 그래프를 그린다.
plot(ccf(a$residuals,b$residuals),main = "wt_temp_min") 

#su1.fit <- arimax(rawdata_sc[,3],order=c(1,0,0),xtransf=rawdata_sc[,c("wt_temp_max")],transfer=list(c(6,3)),include.mean = F,fixed=c(NA,0,0,0,0,0,NA,0,0,NA,NA))
su1.fit <-
  arimax(
    rawdata_sc[1:104, 3],
    order = c(0, 0, 3),
    xtransf = rawdata_sc[1:104, c("wt_temp_min")],
    transfer = list(c(2, 1)),
    include.mean = F,
    method = "ML",
    fixed = c(NA, NA, NA, NA, NA, 0, NA)
  )

Box.test(su1.fit$residuals,type=c("Ljung-Box"))
acf(su1.fit$residuals[6:length(su1.fit$residuals)])
pacf(su1.fit$residuals[6:length(su1.fit$residuals)])

pred <- su1.fit$coef[3] * su1.fit$residuals[(length(su1.fit$residuals) - 50) : (length(su1.fit$residuals) - 3)] +
  su1.fit$coef[2] * su1.fit$residuals[(length(su1.fit$residuals) - 49) : (length(su1.fit$residuals) - 2)] +
  su1.fit$coef[1] * su1.fit$residuals[(length(su1.fit$residuals) - 48) : (length(su1.fit$residuals) - 1)] +
  su1.fit$coef[4] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1), 3] +
  su1.fit$coef[5] * rawdata_sc[107:(length(rownames(rawdata_sc)) - 2), 3] +
  su1.fit$coef[7] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1), c("wt_temp_mean")] 

sqrt(sum((rawdata_sc[109:length(rownames(rawdata_sc)),3] - pred) ** 2))



c <- arima(diff(rawdata_sc[1:104,c("tn_mean")]),order=c(1,0,0),include.mean = F,fixed=a$coef)

plot(ccf(a$residuals,c$residuals),main = "tn_min") 

su2.fit <-
  arimax(
    rawdata_sc[1:104, 3],
    order = c(0, 0, 3),
    xtransf = rawdata_sc[1:104, c("wt_temp_mean","tn_min")],
    transfer = list(c(2, 2),c(0,3)),
    include.mean = F,
    method = "ML",
    fixed = c(NA,NA,NA,NA, NA, 0, NA, NA, 0, NA, 0, NA)
  )

Box.test(su2.fit$residuals,type=c("Ljung-Box"))
acf(su2.fit$residuals[5:length(su2.fit$residuals)])
pacf(su2.fit$residuals[5:length(su2.fit$residuals)])

pred <- su2.fit$coef[3] * su2.fit$residuals[(length(su2.fit$residuals) - 50) : (length(su2.fit$residuals) - 3)] +
  su2.fit$coef[2] * su2.fit$residuals[(length(su2.fit$residuals) - 49) : (length(su2.fit$residuals) - 2)] +
  su2.fit$coef[1] * su2.fit$residuals[(length(su2.fit$residuals) - 48) : (length(su2.fit$residuals) - 1)] +
  su2.fit$coef[4] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1), 3] +
  su2.fit$coef[5] * rawdata_sc[107:(length(rownames(rawdata_sc)) - 2), 3] +
  su2.fit$coef[7] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1), c("wt_temp_mean")] +
  su2.fit$coef[8] * rawdata_sc[107:(length(rownames(rawdata_sc)) - 2),c("wt_temp_mean")] +
  su2.fit$coef[10] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1),c("tn_min")] +
  su2.fit$coef[12] * rawdata_sc[106:(length(rownames(rawdata_sc)) - 3),c("tn_min")]

sqrt(sum((rawdata_sc[109:length(rownames(rawdata_sc)),3] - pred) ** 2,na.rm=T))

sum(abs(rawdata_sc[109:length(rownames(rawdata_sc)),3] - pred),na.rm=T)

d <- arima(diff(rawdata_sc[1:104,c("do_mean")]),order=c(1,0,0),include.mean = F,fixed=a$coef)

plot(ccf(a$residuals,d$residuals),main = "do_mean") 

su3.fit <-
  arimax(
    rawdata_sc[1:104, 3],
    order = c(0, 0, 3),
    xtransf = rawdata_sc[1:104, c("wt_temp_mean","tn_mean","do_mean")],
    transfer = list(c(2, 2),c(0,3),c(0,3)),
    include.mean = F,
    method = "ML",
    fixed = c(NA,NA,NA,NA, NA, 0, NA, NA, 0, NA, 0, NA, 0, 0, 0, NA)
  )

Box.test(su3.fit$residuals,type=c("Ljung-Box"))
acf(su3.fit$residuals[5:length(su3.fit$residuals)])
pacf(su3.fit$residuals[5:length(su3.fit$residuals)])

pred <- su3.fit$coef[3] * su3.fit$residuals[(length(su3.fit$residuals) - 50) : (length(su3.fit$residuals) - 3)] +
  su3.fit$coef[2] * su3.fit$residuals[(length(su3.fit$residuals) - 49) : (length(su3.fit$residuals) - 2)] +
  su3.fit$coef[1] * su3.fit$residuals[(length(su3.fit$residuals) - 48) : (length(su3.fit$residuals) - 1)] +
  su3.fit$coef[4] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1), 3] +
  su3.fit$coef[5] * rawdata_sc[107:(length(rownames(rawdata_sc)) - 2), 3] +
  su3.fit$coef[7] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1), c("wt_temp_mean")] +
  su3.fit$coef[8] * rawdata_sc[107:(length(rownames(rawdata_sc)) - 2),c("wt_temp_mean")] +
  su3.fit$coef[10] * rawdata_sc[108:(length(rownames(rawdata_sc)) - 1),c("tn_mean")] +
  su3.fit$coef[12] * rawdata_sc[106:(length(rownames(rawdata_sc)) - 3),c("tn_mean")] +
  su3.fit$coef[16] * rawdata_sc[106:(length(rownames(rawdata_sc)) - 3),c("do_mean")]


sqrt(sum((rawdata_sc[109:length(rownames(rawdata_sc)),3] - pred) ** 2,na.rm=T))

sum(abs((rawdata_sc[109:length(rownames(rawdata_sc)),3] - pred)))

temp <-
  data.frame(
    pred_value = ifelse(
      pred * sd(rawdata$cells_mean) + mean(rawdata$cells_mean) < 0,
      0,
      pred * sd(rawdata$cells_mean) + mean(rawdata$cells_mean)
    ),
    original = rawdata_sc[109:length(rownames(rawdata_sc)), 3] * sd(rawdata$cells_mean) + mean(rawdata$cells_mean),
    time = paste(rawdata[109:length(rownames(rawdata)), c("year.iso")], rawdata[109:length(rownames(rawdata)), c("week.iso")])
  )
sqrt(sum((rawdata_sc[109:length(rownames(rawdata_sc)),3] - pred) ** 2))

ggplot(data = temp, aes(x = time, y = pred_value, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time , y = original, group = 1), colour = "hotpink", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("pred Vs original")

#su1.fit <- arimax(rawdata_sc[,3],order=c(1,0,0),xtransf=rawdata_sc[,c("wt_temp_max")],transfer=list(c(0,3)),include.mean = F,fixed=c(NA,0,0,NA,NA))




## 실제 두 변수의 겹친 그래프

test <- rawdata_sc

test$week.iso <- ifelse(nchar(test$week.iso) == 2, test$week.iso, paste(0 ,test$week.iso, sep=""))
str(test)
test$time <- paste0(test$year.iso, test$week.iso)

ggplot(data = test, aes(x = time, y = cells_mean, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = time, y = wt_temp_mean, group = 1), colour = "hotpink", size = 1.2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("wt_temp_mean")




###### 밑은 전부 임시 코드이니 신경쓰지 않으셔도 됩니다. ########
##################################################################

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
