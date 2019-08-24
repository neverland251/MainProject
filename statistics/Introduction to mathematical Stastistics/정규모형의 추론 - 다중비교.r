# 쉐페의 다중비교

## 1) F분포를 풀어서 신뢰구간을 정의한 다중비교법으로, 다음의 공식을 따른다.

scheffe <- function(k,x,alpha){
  # 데이터셋의 각 열의 평균
  colmean <- colMeans(x,na.rm=TRUE)
  # 열과 행의 갯수
  b <- length(colnames(data))
  a <- length(rownames(data))
  # 자유도 열-1, 열*(행-1)을 따르는 비중심 F분포의 통계량
  f_stat <- qf(1-alpha,b-1,b*(a-1))
  # 자유도 열*(행-1)을 따르는 비중심 카이스퀘어 통계량. 직접적으로 정의하였다
  v_stat <- (1/(b*(a-1)))*sum((x - colmean)^2,na.rm=TRUE)
  
  # 첫번째 항. 관심 있는 관계의 선형결합 벡터와 열의 평균을 내적한다.
  term1 <- k %*% colmean
  # 두번째 항, F통계량과 V통계량, F통계량을 곱한 값이다.
  term2 <- b*f_stat*sum(k^2)*(v_stat/a)
  
  # 이로서, 관심있는 선형결합을 모두 내포하는 '다중비교' 신뢰구간이 도출된다. 모수는 이 신뢰구간 사이에 a = 0.05의 1종오류 확률로 존재한다.
  
  lower_bound <- term1 - term2^(1/2)
  upper_bound <- term1 + term2^(1/2)

  print(data.frame(stat = term1,upper_bound,lower_bound))

  
}
### (1) 이 때, k는 관심있는 선형결합의 벡터이다. 예를 들어, 열이 3개인 데이터셋에서 mu1 - mu2의 관계에 대한 가설검정을 원하면
k <- c(1,1,0)
### 으로 정의하면 된다.

scheffe(k,data,0.05)

# 본페로니 다중비교

## 1) 쉐페의 다중비교보다 신뢰구간이 더 좁은 다중비교법으로,
## F통계량을 풀어 쓴 쉐페의 방법과는 달리 비중심 T통계량을 풀어 신뢰구간을 정의한다.

bonferoni <- function(k,x,alpha){
  colmean <- colMeans(x,na.rm=TRUE)
  # 열과 행의 갯수
  b <- length(colnames(data))
  a <- length(rownames(data))
  # 자유도 열*(행-1)의 t통계량
  t_stat <- qt(1-alpha,b*(a-1))
  # 자유도 열*(행-1)을 따르는 비중심 카이스퀘어 통계량. 직접적으로 정의하였다
  v_stat <- (1/(b*(a-1)))*sum((x - colmean)^2,na.rm=TRUE)
  
  term1 <- k %*% colmean
  term2 <- sum(k^2)*(v_stat/a)
  
  lower_bound <- -(t_stat) + term1*(term2^(1/2))
  upper_bound <- (t_stat) + term1*(term2^(1/2))
  
  print(data.frame(stat = term1,upper_bound,lower_bound))
  
}

bonferoni(k,data,0.05)

## 2) 본페로니 비교법의 장점은 여러개의 관심 선형결합을 동시에 검증할 수 있다는 점이다.

k <- data.frame(c(1,-1,0),c(0,1,-1))
### (1) 관심 선형결합은 mu1 - mu2, mu2 - mu3이다. 이 둘을 모두 고려한 가설검정을 실시한다.

bonferoni_m <- function(k_m,x,alpha){
  colmean <- colMeans(x,na.rm=TRUE)
  # 열과 행의 갯수
  b <- length(colnames(data))
  a <- length(rownames(data))
  # 관심 선형결합의 행의 갯수
  m <- length(colnames(k))
  # (자유도 열*(행-1))/가설갯수 의 t통계량. 이 때, 본페로니 검정의 경우 가설 갯수로 alpha를 나누어준다.
  t_stat <- qt((1-alpha)/(2*m),b*(a-1))
  # 자유도 열*(행-1)을 따르는 비중심 카이스퀘어 통계량. 직접적으로 정의하였다
  v_stat <- (1/(b*(a-1)))*sum((x - colmean)^2,na.rm=TRUE)
  
  term1 <- t(k) %*% colmean
  term2 <- sum(t(k)^2)*(v_stat/a)
  
  # 각 관심 선형결합의 상한, 하한을 정의한다.
  upper_bound <- -(t_stat) + term1*(term2^(1/2))
  lower_bound <- (t_stat) + term1*(term2^(1/2))

  print(data.frame(stat = term1,upper_bound,lower_bound))
}

bonferoni_m(k,data,0.05)


