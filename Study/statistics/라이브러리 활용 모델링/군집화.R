# k-means

wineURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

wine <- read.table(wineURL,header=FALSE,sep=",",stringsAsFactors = FALSE, col.names = c("Cultivar","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids","Nonflavanoid.phenols","Proanthocyanin","Color.intensity","Hue","OD280.OD315.of.diluted.wines","Proline"))

head(wine)

## 품종 열은 제외해준다.
wineK3 <- kmeans(wineTrain,centers=3,nstart=25)
wineTrain <- wine[,which(names(wine) != "Cultivate")]

## 혼돈행렬을 보면 "cultivar"의 개수대로 적합한 결과는확실히 제대로 적합되지 않았다.
table(wine$Cultivar,wineK3$cluster)
## 하티건 규칙에 따라 K-means의 적정 클러스터 갯수를 결정한다.

library("useful")
wineBest <- FitKMeans(wineTrain, max.cluster= 20, nstart = 25, seed = 278613)
wineBest

## 하티건 규칙에 따르면, 군집 내 제곱합이 10이 넘어가는 클러스터 개수는 좋은 갯수가 아니다. 따라서 10을 넘어가지 않는 클러스터를 지정해준다.
set.seed(278613)
wineK13 <- kmeans(wineTrain,center=13,nstart=25)

## 하티건 규칙 외에, Gap 통계량을 비교하는 기법도 있다. 이는 부트스트랩으로 무작위 선출한 샘플들(기댓값)과 데이터셋의 상이도(dissimilirarity)를 비교한다.
library(cluster)
theGap <- clusGap(wineTrain,FUNcluster = pam, K.max= 20)
gapDF <- as.data.frame(theGap$Tab)

a <- ggplot(gapDF,aes(x=1:nrow(gapDF))) + geom_line(aes(y=logW),color="Blue") + geom_point(aes(y=logW),color="Blue") + geom_line(aes(y=E.logW),color="green") + geom_point(aes(y=E.logW),color="green")

b <- ggplot(gapDF,aes(x=1:nrow(gapDF))) + geom_line(aes(y=gap),color="Blue") + geom_point(aes(y=gap),color="Blue") + geom_errorbar(aes(ymin=gap-SE.sim,ymax=gap+SE.sim),color="green")

gridExtra::grid.arrange(a,b)

## 하티건 규칙에 따르면 군집은 13개가 가장 적당하고, GAP 분석에 따르면 5개가 가장 적당하다.

# PAM

## 군집의 평균값(k-means)를 쓰는것이 아닌, 군집의 중앙값을 쓰는 방법
## 실제 있는 값을 사용하기 때문에 이상치에 강건하다.

library("cluster")
library("WDI")
indicators <- c("NY.GDP.DEFL.KD.ZG","BX.KLT.DINV.WD.GD.ZS","NY.GDP.MKTP.CD","NY.GDP.MKTP.KD.ZG","NY.GDP.PCAP.CD","NY.GDP.PCAP.KD.ZG","TG.VAL.TOTL.GD.ZS")
WDIsearch("BX.KLT.DINV.WD.GD.ZS",field = "indicator")
wdInfo <- WDI(country="all",indicator = indicators, start=2011, end=2011, extra=TRUE)

wbInfo <- wbInfo[wbInfo$region != "Aggregates",]

for(i in indicators){
print(WDIsearch(i,field="indicator"))
}
