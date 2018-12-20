series <- read.table(file="PAXyear.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
print(series)
setwd()
series_test <- series

library("ggplot2")
library("scales")

str(series_test)

PAX <- ts(series_test$PAX, start = min(series_test$year), end = 2016)
fri_ab <- ts(series_test$Fri_ab, start = min(series_test$year), end = 2016)
Cur <- ts(log(series$Cur), start = min(series_test$year), end = 2016)
plot(PAX, ylab = "Per Capital GDP", xlab = "Year")

acf(PAX)



PAX_1 <- diff(PAX, differences = 1)


acf(PAX_1)
pacf(PAX_1)

library(forecast)
ndiffs(x = PAX)
plot(diff(PAX,1))
acf(diff(PAX,1))
pacf(diff(PAX,1))

PAX_fit <- auto.arima(PAX)
PAX_fit

#VAR
Cur <- log(Cur)

#model fit
a <- arima(log(PAX), order=c(1,1,0))
tsdisplay(residuals(a), lag.max=15, main='PAX ARIMA(1,1,0)')
Box.test(resid(a),type="Ljung",lag=20,fitdf=1)
fri_ab_diff = diff(fri_ab,difference = 1)
b = arima(fri_ab, order=c(1,1,0), fixed=c(a$coef))
c = arima(Cur, order=c(1,1,0), fixed=c(a$coef))

y <- a$residuals

x <- b$residuals

z <- c$residuals

ccf(y,x)
ccf(y,z)

Cur_diff <- diff(Cur,diffrences=1)
su1 = ts.intersect(pax_diff,Cur_diff,Cur_diffd1=lag(Cur_diff,-1))
su2 = ts.intersect(pax_diff, pax_diffd1=lag(pax_diff,-1),Cur_diff)
summary(su2.fit <- lm(pax_diff~0+Cur_diff+pax_diffd1,data=su2))
summary(su1.fit <- lm(pax_diff~0+Cur_diffd1,data=su1))

su1fit <- lm(pax_diff~0+Cur_diffd1,data=su1)

su1res <- su1fit$residuals
su1res <- ts(su1res,start = 1989, end = 2016)

acf(su1res)
pacf(su1res)

su1ar <- arima(su1res, order=c(6,0,0))



fitted = ts.intersect(pax_diff,Cur_diffd1=lag(Cur_diff,-1),su1resd1=lag(su1res,-1),su1resd2=lag(su1res,-2),su1resd3=lag(su1res,-3),su1resd4=lag(su1res,-4),su1resd5=lag(su1res,-5),su1resd6=lag(su1res,-6))
model1 <- fitted.fit <- lm(pax_diff~Cur_diffd1+su1resd1+su1resd2+su1resd3+su1resd4+su1resd5+su1resd6, data=fitted)
model2<- fitted.fit <- lm(pax_diff~Cur_diffd1+su1resd1,data=fitted)
model3<- fitted.fit <- lm

library("TSA")
model1 <- arimax(fitted_p, order=c(1,0,0), xreg = fitted)
predict(model1,cur_diff_fore,h=1,newxreg=cur_diff_fore)
arima(su1res,order=c(6,0,0),include.mean=FALSE)
cur_diff_fore <- diff(log(series_test[29:30,"Cur"]),differences=1)
cur_diff_fore <-append(Cur_diff, cur_diff_fore)
cur_diff_fore <- ts(cur_diff_fore, start = 1989, end = 2017)
su1res_fore <- predict(arima(su1res, order=c(6,0,0),include.mean=FALSE),h=1)
su1res_fore <- append(su1res, su1res_fore$pred,after = length(su1res))
su1res_fore <- ts(su1res_fore, start = 1989, end = 2017)
pred <- ts.intersect(cur_diff_fore, su1res_fore,su1resd1=lag(su1res_fore,-1),su1resd2=lag(su1res_fore,-2),su1resd3=lag(su1res_fore,-3),su1resd4=lag(su1res_fore,-4),su1resd5=lag(su1res_fore,-5))

predict(model1,newxreg=pred[24,],h=1)
