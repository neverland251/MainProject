#1. 우도비 원리에 따라 N(c1,c3), N(C2,C3)에 대하여 C1 = C3라는 가설을 검정하고자한다.(a = 0.05)

## 1) 우도비 통계량을 정의한 결과, 해당 통계량이 T분포를 따른다는 점이 밝혀졌다. 이 때 공식은 다음과 같다

t <- function(x,y){
  numb <- ((length(x)*length(y))/(length(x)+length(y)))^(1/2)
  mean <- mean(x) - mean(y)
  freedom <- (length(x)+length(y) - 2)
  combi_var <- sum(x-mean(x))^2 + sum(y - mean(y))^2
  formula <- (numb*mean)/(combi_var/freedom)^(1/2)
  return(formula)
}

## length(x) = length(y) = 16,mean(x) = 75.2, mean(y) = 78.6, combi_var = 71.2-54.8 일때 T통계량을 구하시오

numb <- (64/16)^(1/2)
mean <- 75.2 - 78.6
freedom <- 16-2
combi_var = 71.2+54.8

t_stat <- (numb*mean)/(combi_var/freedom)^(1/2)

### 이 때, 자유도 14의 t통계량은

abs(t_stat) > qt(0.95,df = 14)

### 통계량의 절댓값은 t 검증치를 초과하지 못하므로, 평균이 같다는 귀무가설을 기각하지 못한다.

## 2) 이 검정의 p값을 구하라

1 - pt(abs(t_stat),14)

## 2) 치우치도록 오염된 정규분포(rscn)에서 1000개를 무작위 선출하여 t통계량을 계산하는 몬테카를로 시뮬레이터를 작성하라

rscn <- function(n,eps,sd,mu){
  x1 <- rnorm(n)
  x2 <- rnorm(n,mu,sd)
  b1 <- rbinom(n,1,eps)
  rscn <- x1*(1-b1)+b1*x2
  return(rscn)
}
k <- c()
eps <- 0.2
num <- 30
cont_mu <- 5
cont_sd <- 25
for(i in seq(0,1000)){
  temp <- rscn(num,eps,cont_sd,cont_mu)
  value <- (mean(temp) - (eps*cont_mu))/(var(temp)^(1/2)/length(temp)^(1/2))
  if(value < qt(0.05,num-1)){
    k <- c(k,1)
  }
}

length(k)/1000
