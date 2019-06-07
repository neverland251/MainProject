# 민감도곡선

# 1. 다음 표본값을 이용하여 표본평균, 표본중앙값, 호지스-레만 추정량에 대한 민감도 곡선을 구하여라. -300부터 300까지 10씩 증가시켜 계산하고, 
# 그 결과를 그래프로 그려라

x <- c(-9,58,12,-1,-37,0,11,21,18,-24,-4,-59,-9,9,8)

## 1) 표본평균에 대한 민감도곡선

sensi <- c()

for(i in seq(-300,300,by=10)){
  x_abnom <- c(x,c(i))
  a <- (mean(x_abnom) - mean(x))/(1/(length(x)+1))
  sensi <- c(sensi,c(a))
}

sensi <- data.frame("sensi" = sensi,"abnom" = seq(-300,300,by=10))

library("ggplot2")

plot(sensi)

## 2) 표분중앙값에 대한 민감도곡선

sensi_median <- c()

for(i in seq(-300,300,by=10)){
  x_abnom <- c(x,c(i))
  a <- (median(x_abnom) - median(x))/(1/(length(x)+1))
  sensi_median <- c(sensi_median,c(a))
}

sensi_median <- data.frame("sensi" = sensi_median,"abnom" = seq(-300,300,by=10))

plot(sensi_median)

## 3) 호지스-레만 추정량에 대한 민감도 곡선

sensi_hodges <- c()

for(i in seq(-300,300,by=10)){
  x_abnom <- c(x,c(i))
  a <- c()
  for(j in seq(1,length(x_abnom))){
    for(k in seq(1,j)){
      a[j] <- (x_abnom[k] + x_abnom[j])/2
    }
    b_abnom <- median(a)
  }
  a1 <- c()
  for(j in seq(1,length(x))){
    for(k in seq(1,j)){
      a[j] <- (x[k] + x[j])/2
    }
    b <- median(a)  
  }
  a1 <- (b_abnom - b)/(1/(length(x)+1))
  sensi_hodges <- c(sensi_hodges,c(b))
}

sensi_hodges <- data.frame("sensi" = sensi_hodges,"abnom" = seq(-300,300,by=10))

plot(sensi_hodges)

h <- merge(merge(sensi,sensi_median,by=c("abnom")),sensi_hodges,by=c("abnom"))

ggplot(h) + geom_line(aes("x" = abnom,"y"=sensi)) + geom_line(aes("x" = abnom,"y" = sensi.x)) + geom_line(aes("x" = abnom,"y" = sensi.y))

                                                                                                                                                      