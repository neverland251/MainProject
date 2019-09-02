install.packages("usingR")

data(father.son,package="UsingR")

library(ggplot2)

head(father.son)

ggplot(father.son,aes(x= fheight, y = sheight)) + geom_point() + geom_smooth(method = 'lm') + labs(x = "Fahters",y="Sons")

## 시각화로서의 회귀가 아니라 실제 회귀직선 적합

heightsLM <- lm(fheight ~ sheight,data = father.son)

summary(heightsLM)

## 분산분석 대신 회귀 사용

data(tips,package = "reshape2")
head(tips)

tipsAnova <- aov(tip ~ day - 1, data=tips)
tipsLM <- lm(tip ~ day-1,data=tips)

### 회귀의 F통계량(MSR/MSE)과 분산분석의 F통계량(집단간분산 / 집단내분산)의 값이 같다.

## 다중회귀분석

### 데이터셋을 읽어온다.

housing <- read.table("https://www.jaredlander.com/data/housing.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
names(housing)

### 컬럼명 재지정
names(housing) <- c(names(housing)[1],"Class","Units","YearBuilt","Sqft","Income","IncomePerSqft","Expense","ExpensePerSqft","NetIncome","Value","ValuePerSqft","Boro")

### 히스토그램을 그려본다.
ggplot(housing,aes(x=ValuePerSqft,fill=Boro)) + geom_histogram(binwidth=10)

##Facet_wrap은 히스토그램을 분할해서 보여준다.
ggplot(housing,aes(x=ValuePerSqft,fill=Boro)) + geom_histogram(binwidth=10) + facet_wrap(~Boro)

### 세대수(Units)와 평방미터(Sqft)의 상관관계

#g1 <- ggplot(housing,aes(x=Sqft)) + geom_histogram()
g1 <- ggplot(housing[housing$Units < 1000,],aes(x=Sqft)) + geom_histogram()
#g2 <- ggplot(housing,aes(x=Units)) + geom_histogram()
g2 <- ggplot(housing[housing$Units < 1000,],aes(x=Units)) + geom_histogram()

gridExtra::grid.arrange(g1,g2)

### 세대수와 평방피트당 가격의 산점도 그리기

p1 <- ggplot(housing,aes(x= Units, y = ValuePerSqft)) + geom_point()
p2 <- ggplot(housing,aes(x=Sqft,y=ValuePerSqft)) + geom_point()
gridExtra::grid.arrange(p1,p2)

p1 <- ggplot(housing[housing$Units < 1000,],aes(x=Units,y = ValuePerSqft)) + geom_point()
p2 <- ggplot(housing[housing$Units < 1000,],aes(x=Units,y=ValuePerSqft)) + geom_point()
gridExtra::grid.arrange(p1,p2)

### 세대수를 1000 미만으로 고정하는것이 추세가 더 뚜렷하게 보인다.

p1 <- ggplot(housing[housing$Units < 1000,],aes(x=log(Sqft),y=ValuePerSqft)) + geom_point()
p2 <- ggplot(housing[housing$Units < 1000,],aes(x=Sqft,y=log(ValuePerSqft))) + geom_point()
p3 <- ggplot(housing[housing$Units < 1000,],aes(x=log(Sqft),y=log(ValuePerSqft))) +  geom_point()
gridExtra::grid.arrange(p1,p2,p3)

## 모형 적합

house <- lm(ValuePerSqft ~ Units + Sqft + Boro, data = housing)
summary(house)

### 적합된 계수를 확인한다.
house$coefficients

library(coefplot)

coefplot(house)

## 모형적합2 - 교호작용 확인
### 개별변수 효과 + 상호작용 효과
house2 <- lm(ValuePerSqft ~ Units * Sqft + Boro,data=housing)
### 개별변수 효과는 제외하고, 상호작용 효과만 확인하기
house3 <- lm(ValuePerSqft ~ Units : Sqft + Boro,data=housing)
### 범주형 변수와 연속형 변수를 상호작용 효과 확인하면, 1,0일 때의 상호작용을 모두 보여준다.
house4 <- lm(ValuePerSqft ~ Boro * Sqft,data=housing)
### 범주형 변수와 범주형 변수를 상호작용 효과 확인하면, 모든 경우의 수에 대해 상호작용 결과를 보여준다.
house5 <- lm(ValuePerSqft ~ Boro * Class, data=housing)

gridExtra::grid.arrange(coefplot(house2),coefplot(house3),coefplot(house4),coefplot(house5))


## 모델적합3 - 변수 정규화

house6 <- lm(ValuePerSqft ~ scale(Sqft) + scale(Units) + Boro,data=housing)
coefplot(house6)
### 정규화 해준 결과, 계수들이 유의미한 것으로 나타났다.

## 적합한 모형을 토대로 미래값을 예측한다.

housingNew <- read.table("http://www.jaredlander.com/data/housingNew.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
names(housingNew) <- c(names(housing)[1],"Class","Units","YearBuilt","Sqft","Income","IncomePerSqft","Expense","ExpensePerSqft","NetIncome","Value","ValuePerSqft","Boro")

housePredict <- predict(house,newdata = housingNew,interval="prediction")
