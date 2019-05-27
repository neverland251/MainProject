# 국가별/월별 입국비율

for_visit <- read.csv("국적별 입국.csv",stringsAsFactors = FALSE)
colnames(for_visit) <- c("국적",rep(1:12),"총계")
for_visit_710 <- for_visit[c("국적",rep(7:10))]

for_visit_710["합계"] <- rowSums(for_visit_710[2:5])

## 국가별 배분 원리

a <- for_visit_710["합계"]/sum(for_visit_710["합계"])
b <- (for_visit_710[2]/for_visit_710["합계"])*(for_visit_710["합계"]/sum(for_visit_710["합계"]))+(for_visit_710[3]/for_visit_710["합계"])*(for_visit_710["합계"]/sum(for_visit_710["합계"]))+(for_visit_710[4]/for_visit_710["합계"])*(for_visit_710["합계"]/sum(for_visit_710["합계"]))+(for_visit_710[5]/for_visit_710["합계"])*(for_visit_710["합계"]/sum(for_visit_710["합계"]))

print(head(c(a,b)))

## 국가별/월별 배분

for(i in rep(2:5)){
  for_visit_710[i] <- (for_visit_710[i]/for_visit_710["합계"])*(for_visit_710["합계"]/sum(for_visit_710["합계"]))
}  

## 조사 대상 국가만 선별
nationality <- for_visit_710[,"국적"]
nationality <- nationality[c(2,3,6,9,4,7,8,62,30,31,45,44,46,43,19,11,5,10,12,72)]

for_visit_710_sel <- data.frame()

for(i in nationality){
  temp <- for_visit_710[for_visit_710["국적"] == i]
  for_visit_710_sel <- rbind(for_visit_710_sel,temp,stringsAsFactors = FALSE)
}

colnames(for_visit_710_sel) <- c("nat",rep(7:10),"total")

# 유효표본수

temp_1 <- merge(table(rawdata[rawdata["chasu"] == 7,"nat"]),table(rawdata[rawdata["chasu"] == 8,"nat"]),by = c("Var1"),suffixes = c(7,8))
temp_2 <- merge(table(rawdata[rawdata["chasu"] == 9,"nat"]),table(rawdata[rawdata["chasu"] == 10,"nat"]),by = c("Var1"),suffixes = c(9,10))
for_visit_nat <- merge(temp_1,temp_2,by=c("Var1"))
colnames(for_visit_nat) <- c("nat",rep(7:10))
for_visit_nat["nat"] <- nationality[c(for_visit_nat[["nat"]])]

## 총 유효표뵨수

total <- sum(for_visit_nat[2:5])

## 가중치 도출

df_for_wt <- merge(for_visit_nat,for_visit_710_sel,by=c("nat"))
df_for_wt[6:10] <- as.numeric(unlist(df_for_wt[6:10]))
wt <- cbind(df_for_wt["nat"],(df_for_wt[6:9] * total)/df_for_wt[2:5])




# 응답과 가중치 결합

temp_1 <- rbind(rawdata[rawdata["chasu"] == 7,c("nat","grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","chasu")],rawdata[rawdata["chasu"] == 8,c("nat","grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","chasu")])
temp_2 <- rbind(rawdata[rawdata["chasu"] == 9,c("nat","grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","chasu")],rawdata[rawdata["chasu"] == 10,c("nat","grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","chasu")])
rawdata_visit <- rbind(temp_1,temp_2)

colnames(rawdata_visit) <- c("nat","서울","인천","경기","강원","충청","경상","전라","제주","기타","chasu")

rawdata_visit[is.na(rawdata_visit)] <- 0

## 출신국(nat) 코드호

a <- data.frame(nationality)
a[2] <- row.names(a)
colnames(a) <- c("nat","row")
wt <- merge(wt,a,by.x="nat")

head(rawdata_visit)

## 가중치 일괄 부여

for(i in rep(1:20)){
  for(j in rep(7:10)){
    rawdata_visit[rawdata_visit["nat"] == i&rawdata_visit["chasu"] == j,"wt"] <- wt[wt["row"] == i,j-5]
    
  }
}

rawdata_visit["nat"] <- NULL

##서울

j = 1
a <- rawdata_visit[rawdata_visit[j] == j,]
a <- na.omit(a)

for(i in rep(2:9)){
  a <- a[a[i] == 0,]
}  

x <- sum(a[j] * a["wt"])/length(rawdata_visit[,1])

#그외


for(j in rep(2:9)){
  a <- rawdata_visit[rawdata_visit[j] == j,]
  a <- na.omit(a)
  a[j] <- 1
  
  test <- c(rep(1:(j-1)),rep((j+1):9))
  
  for(i in test){
    a <- a[a[i] == 0,]
  }  
  temp <- sum(a[j] * a["wt"])/length(rawdata_visit[,1])
  x <- c(x,temp)
}

for(i in nationality){
  wt["nat"] == i
}

