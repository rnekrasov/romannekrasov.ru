data=read.csv("C:/QRG/result2.csv",
              header=TRUE,dec=".",sep=",")
plot.ts(data$ARS, col="blue")
priceARS=(log(data$ARS[2:length(data$ARS)])-log(data$ARS[1:length(data$ARS)-1]))*100
priceHDK=(log(data$HDK[2:length(data$HDK)])-log(data$HDK[1:length(data$HDK)-1]))*100
priceIDR=(log(data$IDR[2:length(data$IDR)])-log(data$IDR[1:length(data$IDR)-1]))*100
priceJPY=(log(data$JPY[2:length(data$JPY)])-log(data$JPY[1:length(data$JPY)-1]))*100
priceNOK=(log(data$NOK[2:length(data$NOK)])-log(data$NOK[1:length(data$NOK)-1]))*100
priceSEK=(log(data$SEK[2:length(data$SEK)])-log(data$SEK[1:length(data$SEK)-1]))*100
price=data.frame(priceARS,priceHDK,priceIDR,priceJPY,priceNOK,priceSEK)
cor(price)

#коррелограмма
plot.ts(oil,price,plot.type=c("multiple"))
par(mfrow=c(2,1))
plot.ts(data[,3],type="l",ylab="price")
plot.ts(data[,2],type="l",ylab="oil")
ccf(oil,price)

#строим график и подписываем его оси
data$newday = as.Date(data$date, "%Y-%m-%d")
plot(data$newday, data$ARS,type="l",col="blue",ylab="ARS")
plot(data$newday, data$HDK,type="l",col="blue",ylab="HDK")
plot(data$newday, data$IDR,type="l",col="blue",ylab="IDR")
plot(data$newday, data$JPY,type="l",col="blue",ylab="JPY")
plot(data$newday, data$NOK,type="l",col="blue",ylab="NOK")
plot(data$newday, data$SEK,type="l",col="blue",ylab="SEK")
plot(data$newday, data$RUB,type="l",col="blue",ylab="RUB")

#коинтеграция временных рядов, уточнить смысл параметра thresh в коинтеграционной регрессии
install.packages("apt")
library(apt)
y1=as.ts(priceNOK)
y2=as.ts(priceSEK)
lag1=ciTarLag(y=y1, x=y2, model = "tar", maxlag=3,thresh=0, adjust = TRUE)
f1=ciTarFit(y=y1, x=y2, model="tar",  lag=0, thresh=0)
f1