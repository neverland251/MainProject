# 적응적 방법

x <- round(runif(15,0,100))
y <- round(runif(15,0,100))


# a점수함수(순위함수(yj) / n_y + n_x + 1)(단, n_y와 n_x는 각각 x와 y의 표본수)
alpha_score <- function(a,b){
  rank(a)/(length(a) + length(b) + 1)
}

# 윌콕슨 점수 함수
score_1 <- function(b){
  data.frame("x" = b, "y" = 2*(b)-1)
}

# 부호점수함수
score_2 <- function(b){
  data.frame("x" = b, "y" = sign(2*b-1))
}

# 변형된 윌콕슨 점수함수(짧은 꼬리용)
score_3<- function(b){
  temp_1 <- data.frame("x" = b[b <= (1/4)], "y" = (4 * b[b <= (1/4)]) - 1)
  temp_2 <- data.frame("x" = b[(1/4) < b&b <= (3/4)], "y" = (b[(1/4) < b&b <= (3/4)]) * 0)
  temp_3 <- data.frame("x" = b[(3/4) < b&b < 1], "y" = (4 * b[(3/4) < b&b < 1]) - 3)
  rbind(temp_1,temp_2,temp_3)
}

#변형된 윌콕슨 점수함수2(긴 꼬리용)

score_4 <- function(b){
  temp_1 <- data.frame("x" = b[b <= (1/2)], "y" = (4*b[b <= (1/2)]) - (3/2))
  temp_2 <- data.frame("x" = b[(1/2)<b&b<1], "y" = (4*b[(1/2)<b&b<1])/(4*b[(1/2)<b&b<1]))
  rbind(temp_1,temp_2)
}

library('ggplot2')

# 각각의 점수함수들의 양태

ggplot(score_1(seq(0,1,by=0.01)),aes("x" = x, "y" = y)) + geom_line() + geom_hline(yintercept = 0, color = "red")
ggplot(score_2(seq(0,1,by=0.01)),aes("x" = x, "y" = y)) + geom_line() + geom_hline(yintercept = 0, color = "red")
ggplot(score_3(seq(0,1,by=0.01)),aes("x" = x, "y" = y)) + geom_line() + geom_hline(yintercept = 0, color = "red")
ggplot(score_4(seq(0,1,by=0.01)),aes("x" = x, "y" = y)) + geom_line() + geom_hline(yintercept = 0, color = "red")

n_1 <- length(x)
n_2 <- length(y)
number_for_a <- c(seq(1,n_1+n_2))

# 각각의 점수 함수에 기반한 분포 무관 순위통계량의 분산을 구한다.

var_w1 <- ((n_1 * n_2)/(n_1 + n_2 - 1)) * ((1/(n_1 + n_2)) * sum(score_1(alpha_score(number_for_a,NULL))[2]^2))
var_w2 <- ((n_1 * n_2)/(n_1 + n_2 - 1)) * ((1/(n_1 + n_2)) * sum(score_2(alpha_score(number_for_a,NULL))[2]^2))
var_w3 <- ((n_1 * n_2)/(n_1 + n_2 - 1)) * ((1/(n_1 + n_2)) * sum(score_3(alpha_score(number_for_a,NULL))[2]^2))
var_w4 <- ((n_1 * n_2)/(n_1 + n_2 - 1)) * ((1/(n_1 + n_2)) * sum(score_4(alpha_score(number_for_a,NULL))[2]^2))

# 통계량 w1, w2, w3, w4는 각각 norm(0,var_w)의 점근적 정규분포를 따른다. 이를 통해 가설을 검정하면

pnorm(sum(score_1(alpha_score(y,x))[2]) / sqrt(var_w1),0,var_w1)
pnorm(sum(score_2(alpha_score(y,x))[2]) / sqrt(var_w2),0,var_w2)
pnorm(sum(score_3(alpha_score(y,x))[2]) / sqrt(var_w3),0,var_w3)
pnorm(sum(score_4(alpha_score(y,x))[2]) / sqrt(var_w4),0,var_w4)
                                          
# 어떤 값을 사용할지는 IMS p. 619에 Rule이 나와있다.