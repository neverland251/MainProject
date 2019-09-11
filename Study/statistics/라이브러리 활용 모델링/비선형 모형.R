# 스플라인
## 평활 스플라인
data(diamonds,package="ggplot2")

diaSpline1<- smooth.spline(x=diamonds$carat, y=diamonds$price)
diaSpline2 <- smooth.spline(x=diamonds$carat, y=diamonds$price,df= 2)
diaSpline3 <- smooth.spline(x=diamonds$carat, y=diamonds$price,df= 10)
diaSpline4 <- smooth.spline(x=diamonds$carat, y=diamonds$price,df= 20)
diaSpline5 <- smooth.spline(x=diamonds$carat, y=diamonds$price,df= 50)
diaSpline6 <- smooth.spline(x=diamonds$carat, y=diamonds$price,df=100)

get.spline.info <- function(object){
  data.frame(x=object$x,y=object$y,df=object$df)
}

library(plyr)
library(ggplot2)

splineDF <- ldply(list(diaSpline1,diaSpline2,diaSpline3,diaSpline4,diaSpline5,diaSpline6),get.spline.info)
head(splineDF)

ggplot(diamonds,aes(x=carat,y=price)) + geom_point() + geom_line(data=splineDF,aes(x=x,y=y,color=factor(round(df,0)),group=df)) + scale_color_discrete("Degrees of /n Freedom")
## 자유도가 많아질수록 변곡점도 더 많아지는 것을 확인할수 있다.

## 기저 스플라인

library(splines)
ns(diamonds$carat, df=1)

## 일반화 가법 모형 : 각각의 변수별로 독립적인 평활함수를 적용하는것. 

library(mgcv)

download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data","german.data")


credit <- read.table("german.data")
names(credit) <- c("Checking","Duration","CreditHistory","Purpose","CreditAmount","Savings","Employment","InstallmentRate","GenderMarital","OtherDebtors","YearsAtResidence","RealEstate","Age","OtherInstallment","Housing","ExistingCredits","Job","NumLiable","Phone","Foreign","Credit")

creditHistory <- c(A30 = "AllPaid", A31 = "AllPaidThisBank",A32 = "Up To Date",A33 = "Late Payment", A34 = "Critical Account")
purpose <- c(A40 = "car(new)", A41 = "car(used)", A42 = "furniture/equipment", A43 = "radio/television", A44 = "domestic appliances",
             A45 = "repairs", A46 = "education", A47 = "vacation", A48 = "retraining", A49 = "business", A410 = "others")
employment <- c(A71="unemployed",A72="< 1 year",A73 = "1 - 4 years", A74 = "4 - 7 years", A75 = ">= 7 years")

credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]


credit$Credit <- ifelse(credit$Credit == 1,"Good","Bad")
credit$Credit <- factor(credit$Credit, levels=c("Good","Bad"))

head(credit[,c("CreditHistory","Purpose","Employment","Credit")])

library(mgcv)

creditGam <- gam(Credit ~ te(CreditAmount) + s(Age) + CreditHistory + Employment,data=credit,family=binomial(link="logit"))

summary(creditGam)

# 의사결정나무

library(rpart)
library(rpart.plot)
creditTree <- rpart(Credit ~ CreditAmount + Age + CreditHistory + Employment,data=credit)

## rpart.plot으로 시각화한다.
rpart.plot(creditTree,extra = 4)

# 부스티드 트리

library(useful)

creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount -1

## 예측변수와 결과변수를 나누어준다.
creditX <- build.x(creditFormula,data=credit,contrasts = FALSE)
creditY <- build.y(creditFormula,data=credit)

## 논리형으로 되어있는 벡터를 [0,1]로 변환해준다.
creditY <- as.integer(relevel(creditY,ref="Bad")) - 1

library(xgboost)

creditBoost <- xgboost(data = creditX, label = creditY, max.depth = 3, eta = 0.3, nthread = -1, nrounds = 100,objective = "binary:logistic")

## xgboost로 적합된 결과를 시각화한다.
xgb.plot.multi.trees(creditBoost,feature_names = colnames(creditX))
xgb.plot.importance(xgb.importance(creditBoost,feature_names = colnames(creditX)))

#랜덤 포레스트

library(randomForest)

## 예측변수와 결과변수를 나누어준다.
creditX <- build.x(creditFormula,data=credit,contrasts = FALSE)
creditY <- build.y(creditFormula,data=credit)

creditForest <- randomForest(x=creditX,y=creditY)
creditForest

## xgboost의 설정을 수정하면 랜덤포레스트를 동일하게 수행할 수 있다.

## 논리형으로 되어있는 벡터를 [0,1]로 변환해준다.
creditY <- as.integer(relevel(creditY,ref="Bad")) - 1
creditXForest <- xgboost(data = creditX, label = creditY, max.depth = 3, num_parallel_tree = 1000, subsample = 0.5, colsample_bytree = 0.5, nrounds= 3,objective = "binary:logistic")

## XGboost로 랜덤포레스트를 수행하면, 앞서 구현된 시각화툴을 그대로 쓸 수 있다. 
xgb.plot.multi.trees(creditXForest,feature_names = colnames(creditX),render = TRUE)
xgb.plot.importance(xgb.importance(creditXForest,feature_names = colnames(creditX)))
