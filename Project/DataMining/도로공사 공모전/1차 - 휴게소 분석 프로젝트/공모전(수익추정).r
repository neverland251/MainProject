# 수익 추정

n <- 1008671+1103506+1078653+1165638
b <- n * (1-sum(x))

# 주교통수단을 이용한 평균 이동횟수
## 총 발생건수 1977회, 주 이동 인원수 923명, 1인당 평균 2.1회 이동(공항 이동 제외 수치임)

total_case <- b * 2.1

# 기댓갑 행렬 도출

## 대절 전세버스를 타고 이동한 비율을 구한다.
main_transport <- read.csv("주교통수단.csv",stringsAsFactors = FALSE,header=TRUE)
rownames(main_transport) <- main_transport[,1]
main_transport <- main_transport[1:9,-c(1)]

## 지역별 이동 분포 행렬을 구한다.
main_transport_case <- read.csv("주교통수단_사례수.csv",stringsAsFactors = FALSE,header=TRUE)
rownames(main_transport_case) <- main_transport_case[,1]
main_transport_case <- main_transport_case[1:9,-c(1)]

##30 표본 미만의 소표본은 전부 제외한다.
main_transport_case[main_transport_case < 30] <- 0

## 이동 분포 행렬에서, 전체 유효 사례수인(1973-273)을 구한다. 273을 빼는 이유는 30표본 미만인 사례수가 273건이기 때문이다. 이를  제외하기 위해서다.
main_transport_p <- (main_transport_case/(1977 - 273))

#외부타당성 검증

## 이동인원의 기댓값은 다음과 같다.
sum(total_case*main_transport_p*main_transport)/2.1

##  단체여행을 즐긴 인원 중 다른 지역으로 떠난 사람의 기댓값은 다음과 같다.
n * tour_total * (1-sum(x_package))

#수익 추정

## 전체 기댓값 행렬 도출(이동 건수 기댓값 * 이동 분포 확률 * 각 구간별 전세버스 이용 비율)
total_case * main_transport_p * main_transport

# 도출한 기댓값 행렬에, 단체관광객의 1인당 평균 식비 지출액 5.59달러(82.3/4.9/3)을 곱하면(총지출액/평균체류일수/세끼 중 한끼)

round(total_case * main_transport_p * main_transport * 0.1 * 5.59)



