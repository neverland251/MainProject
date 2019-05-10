# 구간속도의 카이스퀘어 검정 : H1 : 각 콘존별로, 날짜에 따른 시간대별 평균속도에 모평균에는 차이가 있나?
# 차이가 없다고 검증이 될 경우 : 각 콘존별로 모든 시간대를 다 통합해서 하나의 평균만 사용한다
# 차이가 있다고 검증이 될 경우 : 각 콘콘별로 차이가 있는 항목들을 도출한 후, 이들을 하나로 묶어준다.

row_names <- c("date","time","conzone","type","speed")

rawdata <- read.table("구간속도.csv",sep=",",stringsAsFactors = FALSE,encoding="utf-8",skip=1)

rawdata$V6 <- NULL

colnames(rawdata) <- row_names


hist(rawdata$speed)

## 평균 도출을 위한 부트스트랩
boot_theta <- c()
max_num <- length(rawdata$speed)

for(i in rep(1:30000)){
  temp <- rawdata$speed[round(runif(100,min = 0,max = max_num))]
  x_means <- sum(temp)/100
  boot_theta[i] <- x_means
}

hist(boot_theta)
mean(boot_theta)
## 콘존별 데이터 분리

times <- c(unique(rawdata$time))
dates <- c(unique(rawdata$date))
dates <- as.numeric(dates)
conzones <- c(unique(rawdata$conzone))

conzone_sep <- list()

for(i in conzones){
  temp <- rawdata[rawdata$conzone == i,c("speed","date","time")]
  conzone_sep[i] <- list(temp)
  }
}

## 카이스퀘어 검정을 위한 기댓값 구하기
a <- vector()
b <- vector()
conzones_exp <- list()

for(i in rep(1:length(conzones))){
  for(j in times){
    temp <- conzone_sep[[conzones[i]]][conzone_sep[[conzones[i]]]$time == j,c("speed")]
    a[as.character(j)] <- sum(temp)
    prob_a <- a/sum(a)
  }
  for(k in dates){
    temp <- conzone_sep[[conzones[i]]][conzone_sep[[conzones[i]]]$date == k,c("speed")]
    b[as.character(k)] <- sum(temp)
    prob_b <- b/sum(b)
  }
  exp_matrix <- data.frame()
  total <- sum(a)
  for(l in rep(1:length(prob_b))){
    exp_matrix[names(prob_b[l]),names(prob_a)] <- prob_b[l] * prob_a * total
  }
  conzones_exp[conzones[i]] <- list(exp_matrix)
}



  
}

# 관측값 행렬 도출

library("reshape2")

conzone_value <- list()
rep(1:length(conzones))
for(i in 8){
  test <- conzone_sep[[conzones[[i]]]][c("speed","date","time")]
  test <- dcast(test,date~time,value.var = "speed")
  rownames(test) <- test[[1]]
  test[1] <- NULL
  conzone_value[i] <- list(test)
}
# 속도를 주조(dcast)한다

chisq_value <- c()

for(i in rep(1: length(conzone_value))){
  temp <- sum((conzone_value[[i]] - conzones_exp[[i]])^2/conzones_exp[[i]])
  if(dchisq(temp , df = (length(conzone_value[[i]])-1)*(length(rownames(conzone_value[[i]]))-1)) < 0.05){
    chisq_value[i] <- TRUE
  }
  else{
    chisq_value[i] <- FALSE
  }
}

