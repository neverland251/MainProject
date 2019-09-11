# 부트스트랩

## 부트스트랩은 어떤 통계치(회귀분석의 예측값, 표준편차, 트리모형의 분류 클래스 등)를 여러개 생산하는 방법론이다
## 구체적으론 데이터셋에서 n개의 샘플을 임의 선출하며, 샘플링한 데이터셋으로 모형을 적합, 통계치를 생산한다
## 큰수의 법칙에 따라, 이 통계치는 정규분포를 그리게 된다. 최빈값(평균)과 표준편차도 알 수 있다.

plyr::baseball -> baseball

baseball <- baseball[baseball$year >= 1990,]
head(baseball)

## 평균타율 계산 함수를 만들어준다.
bat.avg <- function(data,indices = 1:NROW(data),hits="h",at.bats="ab"){
  sum(data[indices,hits],na.rm=TRUE)/
    sum(data[indices,at.bats],na.rm=TRUE)
}
##전체 데이터셋의 평균 타율을 계산해본다.
bat.avg(baseball)

avgBoot <- boot(data = baseball, statistic = bat.avg, R=1200, stype="i")

## 이 부트스트랩을 시각화한다.

ggplot() + geom_histogram(aes(x=avgBoot$t))

## 2표준편차 위치도 함께 넣어준다.

ggplot() + geom_histogram(aes(x=avgBoot$t)) + geom_vline(xintercept=avgBoot$t0 + c(-1,1)*2*sqrt(var(avgBoot$t)))
                          