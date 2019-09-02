library("haven")

rawdata <- read_sav("2017년 외래관광객 실태조사_DATA.sav")

write.csv("rawdata.csv",rawdata)

rawdata <- read.csv("rawdata.csv")


temp_1 <- rbind(rawdata[rawdata["chasu"] == 7,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","wt")],rawdata[rawdata["chasu"] == 8,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","wt")])
temp_2 <- rbind(rawdata[rawdata["chasu"] == 9,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","wt")],rawdata[rawdata["chasu"] == 10,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","wt")])
rawdata_visit <- rbind(temp_1,temp_2)

colnames(rawdata_visit) <- c("서울","인천","경기","강원","충청","경상","전라","제주","기타","wt")

rawdata_visit[is.na(rawdata_visit)] <- 0
##서울에서 서울에서만 관광활동 마무리

j = 1리
a <- rawdata_visit[rawdata_visit[j] == j,]

for(i in rep(2:9)){
  a <- a[a[i] == 0,]
}  

x <- sum(a[j] * a["wt"])/length(rawdata_visit[,1])

##타지역에서 해당 지역에서만 관광활동 마무리

for(j in rep(2:9)){
  a <- rawdata_visit[rawdata_visit[j] == j,]
  a[j] <- 1
  
  test <- c(rep(1:(j-1)),rep((j+1):9))
  
  for(i in test){
    a <- a[a[i] == 0,]
  }  
  temp <- sum(a[j] * a["wt"])/length(rawdata_visit[,1])
  x <- c(x,temp)
}


# 7~10월 중 단체관광을 실시한 사람의 비율

temp_1 <- rbind(rawdata[rawdata["chasu"] == 7,c("q17","wt")],rawdata[rawdata["chasu"] == 8,c("q17","wt")])
temp_2 <- rbind(rawdata[rawdata["chasu"] == 9,c("q17","wt")],rawdata[rawdata["chasu"] == 10,c("q17","wt")])
rawdata_type <- rbind(temp_1,temp_2)
rawdata_type <- rawdata_type[rawdata_type[1] == 2,]
rawdata_type[1] <- 1

## 가중치로 보정한 7~10월 전체 여행객 중 단체관광객 비율
tour_total <- sum(rawdata_type[1] * rawdata_type["wt"])/length(rawdata_visit[,1])

#조금 더 디테일하게, 7~10월 단체관광을 실시한 사람 중 지역 내부만이 아니라, 외부로 이동한 사람의 비율을 구하며

temp_1 <- rbind(rawdata[rawdata["chasu"] == 7,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","q17","wt")],rawdata[rawdata["chasu"] == 8,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","q17","wt")])
temp_2 <- rbind(rawdata[rawdata["chasu"] == 9,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","q17","wt")],rawdata[rawdata["chasu"] == 10,c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","q17","wt")])
rawdata_visit <- rbind(temp_1,temp_2)

colnames(rawdata_visit) <- c("서울","인천","경기","강원","충청","경상","전라","제주","기타","type","wt")

rawdata_visit[is.na(rawdata_visit)] <- 0

#서울에서 단체관광을 실시한 사람 중, 서울에서만 단체관광을 마친 사람의 비율을 구하면
j = 1
a <- rawdata_visit[rawdata_visit[j] == j&rawdata_visit["type"] == 2,]

for(i in rep(2:9)){
  a <- a[a[i] == 0,]
}  

x_package <- sum(a[j] * a["wt"])/length(rawdata_visit[rawdata_visit["type"] == 2,1])

#타 지역에서 단체관광을 실시한 사람 중, 그 지역에서만 단체관광을 마친 사람의 비율을 구하면
## NULL값이 생성될경우 오류가 발생하기 때문에, 오류가 발생하지 않는 4,6,7,8 지역만 골라 담는다.

for(j in c(4,6,7,8)){
  a <- rawdata_visit[rawdata_visit[j] == j&rawdata_visit["type"] == 2,]
  a[j] <- 1

  test <- c(rep(1:(j-1)),rep((j+1):9))
  
  for(i in test){
    a <- a[a[i] == 0,]
  }
  temp <- sum(a[j] * a["wt"])/length(rawdata_visit[rawdata_visit["type"] == 2,1])
  x_package <- c(x_package,temp)
}

# 7 ~ 10월 전체 방문객 중(n = 4356468) 9%가 단체관광을 했고
# 그 중 51%가 외부로 이동하였으므로

n * tour_total * (1-sum(x_package))

#는 7~10월 단체관광객 중 외부로 이동한 사람들의 기댓값이 된다.
