# 수치적분


a <- 90000
b <- 100000
n <- b-a
x <- seq(a,b,by=((b-a)/n))

#함수 정의
func <- function(x){
  return(1/log(x))
}           
# 오른쪽 왼쪽 직사각형 법칙
Ln <- sum(((b-a)/n)*(func(x[1:length(x)-1])))
Rn<- sum(((b-a)/n)*(func(x[2:length(x)])))
print(c(Ln,Rn))

# 사다리꼴 법칙
x_n <- (1/2)*(Ln+Rn)
sum(x_n)

# 중점법칙
x_m <- ((b-a)/n)*((1/2)*(func(x[1:length(x)-1]) + func(x[2:length(x)])))
sum(x_m)


# 심프슨 법칙
l <- sum((1/3)*x_n) + sum((2/3)*x_m)
l
