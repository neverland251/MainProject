# glmnet의 엘라스틱넷 사용법

library("glmnet")
library("useful")
## "useful 패키지의 build.x 함수를 이용해 엘라스틱넷을 쓸 수 있도록 데이터 행렬을 적절히 변환시켜준다.
acsX <- build.x(Income~NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1,data=acs,contrasts=FALSE)

## 명목형 변수들이 원핫인코딩으로 변환되는 등 모형 적합에 적절한 데이터셋으로 바뀐것을 확인할 수 있다.
head(acsX)

## "usefl 패키지의 build.y 함수를 이용해 타겟변수도 인코딩해준다.
acsY <- build.y(Income~NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1,data=acs)

head(acsY)
tail(acsY)

## 릿지와 랏소를 조절하는 람다 하이퍼파라미터의 최적값을 뽑아낼 수 있도록 교차 타당성 검증을 실시한다.

set.seed(1853561)
acsCV1 <- cv.glmnet(x = acsX, y = acsY, family = "binomial",nfold = 5)
## acsCV1dml $lambda.min은 오차가 가장 작은 람다값을 반환하고, $lambda.1se는 오차의 1표준편차에 위치한 조금 부정확한 람다값도 같이 제공한다.
plot(acsCV1)
## 그래프에서 y축은 교차타당성 오차를, x축은 해당 람다값의 로그값을 나타낸다. 상단의 숫자는 자유도로, 선택된 변수의 갯수를 뜻한다.
coef(acsCV1,s = "lambda.1se")
## "."으로 나타나는 변수는 벌점이 적용된 결과 선택되지 않은 변수들이다.

## 계수들이 람다 하이퍼파라미터 변화에 따라 어떤 값으로 변화하였는지 추적도 가능하다.
plot(acsCV1$glmnet.fit,xvar="lambda")

## 람다 하이퍼파라미터가 아닌, 릿지와 랏소의 트레이드오프를 조정하는 알파 하이퍼파라미터를 조정한다.
## alpha = 0란 의미는, 릿지에 기울기 1을 부여하고 라쏘에 기울기 0을 부여하는, 즉 라쏘를 아예 꺼버리는 것이다.
set.seed(71623)
acsCV2 <- cv.glmnet(x = acsX, y = acsY, family = "binomial",nfold = 5,alpha = 0)

plot(acsCV2)
coef(acsCV2,s="lambda.1se")
plot(acsCV2$glmnet.fit,xvar="lambda")

## 최적의 알파값을 찾기 위해선 함수로 이를 순회시켜야 한다.
library("doParallel")
library("parallel")
set.seed(2834673)
## 타당성 검증 대상이 될 케이스들을 샘플링한다.
theFolds <- sample(rep(x=1:5, length.out=nrow(acsX)))
## 검증 대상인 alpha값을 만들어준다.
alphas <- seq(from = 0.5, to = 1, by=0.05)

acsDouble <- foreach(i=1:length(alphas),.errorhandling = "remove",.inorder = FALSE,.multicombine = TRUE, .export = c("acsX","acsY","alphas","theFolds"),.packages="glmnet") %dopar%{
  print(alphas[i])
  cv.glmnet(x=acsX,y=acsY,family="binomial",nfolds=5,foldid=theFolds,alpha=alphas[i])
}

##acsDobule엔 앞서 순회시킨 11개의 알파값 각각의 출력값이 담긴 리스트이다.
sapply(acsDouble,class)
head(acsDouble)

## 교차타당성 오류, 람다값만 찾아서 뽑아내는 함수를 작성한다.
extractGlmnetInfo <- function(object){
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  data.frame(lambda.min = lambdaMin, error.min = object$cvm[whichMin],
             lambda.1se = lambda1se, error.1se = object$cvm[which1se])
}
alphaInfo <- Reduce(rbind,lapply(acsDouble,extractGlmnetInfo))

alphaInfo$Alpha <- alphas

library("reshape2")
library("stringr")

alphaMelt <- melt(alphaInfo, id.vars = "Alpha", value.name="Value",variable.name = "Measure")
alphaMelt$Type <- str_extract(string = alphaMelt$Measure,pattern="(min)|(1se)")
alphaMelt$Measure <- str_replace(string=alphaMelt$Measure,pattern="\\.(min|1se)",
                                 replacement="")
alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure, value.var="Value")

ggplot(alphaCast,aes(x=Alpha, y = error))+
  geom_line(aes(group=Type)) + 
  facet_wrap(~Type,scales="free_y",ncol=1) +
  geom_point(aes(size=lambda))

##alpha = 0.75에서 최적의 값이 도출됬기 때문에, 이 값을 토대로 엘라스틱넷을 다시 적합한다.

acsCVfinal <- cv.glmnet(x=acsX, y = acsY, family="binomial",nfolds=5,alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])

plot(acsCVfinal$glmnet.fit,xvar="lambda")

coef(acsCVfinal,"lambda.1se")

# 베이즈 축소

download.file("http://jaredlander.com/data/ideo.rdata","ideo.rdata")
load("ideo.rdata")
head(ideo)

library(dplyr)

## 파이프라인으로 연도별 그룹바이 한 뒤, 각각 적합한 glm모형의 결과값을 저장한다.
results <- ideo %>% group_by(Year) %>% do(Model = glm(Vote ~ Race + Income + Gender + Education,data=.,family=binomial(link="logit")))

head(results)
names(results$Model) <- as.character(results$Year)
## 적합된 모형 각각의 리스트명을 연도로 바꿔준다.

voteInfo <- multiplot(results$Model, coefficients = "Raceblack", plot = FALSE)
multiplot(results$Model,coefficients="Raceblack",secret.weapon = TRUE) + coord_flip(xlim = c(-20,10))
## 1964년 계수의 표준오차가 이상한 것을 확인할 수 있다. 다중공선성 문제가 있는걸까?

group_Year <- ideo %>% group_by(Year)
group_Year[group_Year$Year == 1964|group_Year$Race == "black",]

## 베이즈glm을 이용해 사전확률을 지정해준다. 코시 확률 2.5, 자유도 1로 지정하고 다시 모형을 적합한다.
resultsB <- ideo %>% group_by(Year) %>% do(Model = arm::bayesglm(Vote~Race + Income + Gender + Education, data=., family=binomial(link="logit"),prior.scale=2.5,prior.df=1))

names(resultsB$Model) <- as.character(resultsB$Year)
multiplot(resultsB$Model,coefficients="Raceblack",secret.weapon=TRUE) + coord_flip(xlim=c(-20,10))
