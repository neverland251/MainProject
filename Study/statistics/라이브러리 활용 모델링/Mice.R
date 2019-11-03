rawdata <- read.csv(".\\Project\\DataMining\\NFL yard\\rawdata_impute.csv")
head(rawdata)

library("mice")

num_col <- colnames(rawdata[sapply(rawdata,is.numeric)])

rawdata_imput <- mice(rawdata[num_col],method="cart")
summary(rawdata_imput)

imp <- with(rawdata_imput,lm(Humidity ~ .))

rawdata_imput <- polled(imp)
