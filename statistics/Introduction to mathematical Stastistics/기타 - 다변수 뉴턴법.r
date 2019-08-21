# 스테이너의 정류점 계산 알고리즘

func <- function(x,y,x1,x2,x3,y1,y2,y3,p){
  d1 <- ((x - x1)^2 + (y - y1)^2)^(1/2)
  d2 <- ((x - x2)^2 + (y - y2)^2)^(1/2)
  d3 <- ((x - x3)^2 + (y - y3)^2)^(1/2)
  df <- p*(d1)^(p-2)*(x-x1) + p*(d2)^(p-2)*(x-x2) + p*(d3)^(p-2)*(x-x3)
  dy <- p*(d1)^(p-2)*(y-y1) + p*(d2)^(p-2)*(y-y2) + p*(d3)^(p-2)*(y-y3)
  return(c(df,dy))
}

calc <- function(initx,inity,x,y,p){
  a <- 0.5
  vec <- c(initx,inity)
  for(i in seq(2000)){
    vec <- vec - a*(func(vec[1],vec[2],0,3,x,0,0,y,p))
    print(vec)
    if((is.nan(vec[1])) | (is.nan(vec[2]))){
      stop("complete")
    }
  }
}

calc(10,10,1,4,4)
