# 모형 적합 확인

## 잔차 vs 적합값 그래프

housing <- housing[housing$Units < 1000,]

### 모형을 적합한다.

house1 <- lm (ValuePerSqft ~ Units + Sqft + Boro, data = housing)

### geom_smooth

ggplot(house1, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = FALSE)

### 잔차들이 임의적이지 않은 것으로 보인다. 적합이 잘못된걸까?

ggplot(house1, aes(x=.fitted, y=.resid)) + geom_point(aes(color=Boro)) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE)

### 예측값 자체가 Boro별로 뚜렷하게 구분되어 예측됬기 때문에 일어난 현상이다. 중간에 비어있는 부분을 없다고 가정한다면 잔차들은 무작위로 분포했다고 보여진다.

## Q-Q 플롯

## stat_qq는 qq플롯을, geom_abline은 45도 각도의 선을 그어준다.
ggplot(house1, aes(sample=.stdresid)) + stat_qq() + geom_abline()

## 잔차 히스토그램

ggplot(house1, aes(x=.resid)) + geom_histogram()


# 모형 성능 비교

house2 <- lm(ValuePerSqft ~ Units * Sqft + Boro, data = housing)
house3 <- lm(ValuePerSqft ~ Units + Sqft * Boro, data = housing)
house4 <- lm(ValuePerSqft ~ Units + Sqft * Boro + Sqft*Class, data=housing)
house5 <- lm(ValuePerSqft ~ Boro + Class, data=housing)

multiplot(house1,house2,house3,house4,house5)

## 잔차제곱합(RSS)를 확인하기 위해, ANOVA를 활용할 수 있다.

anova(house1,house2,house3,house4,house5)

## glm 모형에 anova를 사용할 경우 이탈도를 얻을 수 있다.

housing$HighValues <- housing$ValuePerSqft >= 150
high1 <- glm(HighValues ~ Units + Sqft + Boro, data = housing,family=binomial(link="logit"))
high2 <- glm(HighValues ~ Units * Sqft + Boro, data = housing,family=binomial(link="logit"))
high3 <- glm(HighValues ~ Units + Sqft * Boro + Class, data = housing,family=binomial(link="logit"))
high4 <- glm(HighValues ~ Units * Sqft * Boro + Sqft * Class, data = housing,family=binomial(link="logit"))
high5 <- glm(HighValues ~ Boro + Class, data = housing,family=binomial(link="logit"))

anova(high1,high2,high3,high4,high5)

### RSS값은 house4가 가장 낮게 나오나, 문제는 이것이 데이터셋에 과적합됬을 가능성이 크다는 것이다.
### 최대 우도법을 사용하여 모형을 판단하지만, 계수가 늘어나는만큼 패널티를 부여하는 AIC, BIC를 대신 사용할 수 있다.

AIC(house1,house2,house3,house4,house5)
BIC(house1,house2,house3,house4,house5)

# K겹 교차검증

install.packages("boot")
library("boot")

## boot 패키지는 lm은 인식을 못하고, glm만 인식한다. glm을 lm으로 인식하도록 연결분포를 가우시안으로, 연결함수는 x가 그대로 들어가도록 항등함수를 쓴다.

houseG1 <- glm(ValuePerSqft ~ Units + Sqft + Boro, data = housing, family=gaussian(link="identity"))
houseG2 <- glm(ValuePerSqft ~ Units * Sqft + Boro, data = housing, family=gaussian(link="identity"))
houseG3 <- glm(ValuePerSqft ~ Units + Sqft * Boro, data = housing, family=gaussian(link="identity"))
houseG4 <- glm(ValuePerSqft ~ Units + Sqft * Boro + Sqft*Class, data = housing, family=gaussian(link="identity"))
houseG5 <- glm(ValuePerSqft ~ Boro + Class, data = housing, family=gaussian(link="identity"))

houseCV1 <- cv.glm(housing,houseG1,K=5)
houseCV2 <- cv.glm(housing,houseG2,K=5)
houseCV3 <- cv.glm(housing,houseG3,K=5)
houseCV4 <- cv.glm(housing,houseG4,K=5)
houseCV5 <- cv.glm(housing,houseG5,K=5)
### $delta에서, 첫번째 숫자는 기존의 K 교차타당성 비용함수의 값을, 두 번째 숫자는 Leave-one-out에서 사용하지 않은 포인트를 보상한다.

data.frame(rbind(houseCV1$delta,houseCV2$delta,houseCV3$delta,houseCV4$delta,houseCV5$delta))

# 단계적 선택법

houseNull <- lm(ValuePerSqft ~ 1, data = housing)
houseFull <- lm(ValuePerSqft ~ Units + Sqft*Boro + Boro*Class, data=housing)

houseStep <- step(houseNull,scope=list(lower = houseNull, upper = houseFull),direction = "both")
