# —читываем тики из базы данных postgres
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '11:00:00' and '11:10:00' 
and ticker='USD000UTSTOM'
and date='08-02-2018'
order by time")

# —читываем дневные данные валютной пары из базы OANDA
#нефть из базы Google
library(quantmod)
ticker<-"USD/RUB"
getSymbols(ticker, src="oanda")
priceUSDRUB<-as.ts(USDRUB,start = 1, end = 179)
ticker<-"CL"
price<-getSymbols(ticker, src="google")
priceCL<-as.ts(CL)

# Ћог-приращени€ р€да
price<-as.numeric(data$price)
log_return<-diff(log(price))
hist(log_return)
plot.ts(data$price)
plot.ts(log_return)

#коррел€ционный анализ валют
data=read.csv("C:/QRG/result2.csv",
              header=TRUE,dec=".",sep=",")
plot.ts(data$ARS, col="blue")
priceARS<-diff(log(data$ARS))
priceHDK<-diff(log(data$HDK))
priceIDR<-diff(log(data$IDR))
priceJPY<-diff(log(data$JPY))
priceNOK<-diff(log(data$NOK))
priceSEK<-diff(log(data$SEK))
price<-data.frame(priceARS,priceHDK,priceIDR,priceJPY,priceNOK,priceSEK)
plot(price,col="blue")
cor(price)
cor.test(x=priceNOK, y=priceSEK)
#коррел€ци€ —пирмена
#corr <- cor.test(x=priceARS, y=priceHDK, method = 'spearman')

#автокоррел€ционна€ функци€
acf(priceARS)
acf(diff(priceCL[,4]))
acf(diff(as.numeric(data$price)))

#регрессионный анализ
data=read.csv("C:/QRG/result2.csv",
              header=TRUE,dec=".",sep=",")
lm.D01<-lm(priceSEK~priceNOK)
summary(lm.D01)
new<-data.frame(priceNOK)
z<-predict(lm.D01, new, se.fit = TRUE)
plot.ts(priceSEK,type="l")
lines(z$fit,col="red")

lm.D02<-lm(data$SEK~data$NOK)
summary(lm.D02)
new<-data.frame(data$NOK)
z<-predict(lm.D02, new, se.fit = TRUE)
plot.ts(data$SEK,type="l")
lines(z$fit,col="red")

lm.D03<-glm(data$SEK~data$NOK)
summary(lm.D03)
new<-data.frame(data$NOK)
z<-predict(lm.D03, new, se.fit = TRUE)
plot.ts(data$SEK,type="l")
lines(z$fit,col="red")

#логистическа€ регресси€
data=read.csv("C:/QRG/logit2.csv",
              header=TRUE,dec=",",sep=";")
lm.D04<-glm(data$USDRUB~data$BRENT,family=binomial(link = "logit"))
summary(lm.D04)
new<-data.frame(data$BRENT)
z<-predict(lm.D04, new)
plot(data$USDRUB,type="l")
lines(z,col="red")
data=read.csv("C:/QRG/logit3.csv",
              header=TRUE,dec=",",sep=";")
lm.D05<-glm(data$USDRUB~data$BRENT,family=binomial(link = "logit"))
summary(lm.D05)
new<-data.frame(data$BRENT)
z<-predict(lm.D05, new)
plot(data$USDRUB,type="l")
lines(z,col="red")

#многомерна€ линейна€ регресси€
library(lmtest)
data<-read.csv(file="c:/QRG/R/volat_usdrub.csv",stringsAsFactors=FALSE,header=TRUE, sep=";",dec=",")
y<-as.numeric(data$vol_usdrub)
x1<-as.numeric(data$total_debt)
x2<-as.numeric(data$net_debt)
x3<-as.numeric(data$vol_rgbi)
x4<-as.numeric(data$delta_rgbi)
x5<-as.numeric(data$vol_oil)
regr1<-lm(y~x1+x2+x3+x4+x5)
summary(regr1)
dwtest(regr1)
plot.ts(predict(regr1))
lines(y,col="red")
regr2<-lm(y~x5)
summary(regr2)
dwtest(regr2)
new <- data.frame(x5)
z<-predict(regr2, new, se.fit = TRUE)
plot.ts(y,type="l")
lines(z$fit,col="red")
axis(3,1:27,labels=as.numeric(data$date))

#коинтеграци€ временных р€дов
library(apt)
y1=as.ts(priceSEK)
y2=as.ts(priceNOK)
lag1=ciTarLag(y=y1, x=y2, model = "tar", maxlag=3,thresh=0, adjust = TRUE)
f1=ciTarFit(y=y1, x=y2, model="tar",  lag=0, thresh=0)
f1

#непараметрическа€ статистика
library(np)
x1<-1;x2<-length((ordered(data$time)))
x<-data$time[x1:x2];y<-as.numeric(data$price[x1:x2])
bw <- npregbw(xdat=ordered(x), ydat=y)
fit.lc <- npksum(txdat=x, tydat=y, bws=bw$bw)$ksum/npksum(txdat=x, bws=bw$bw)$ksum
plot(ordered(x),y,ylab="USDRUB")
lines(ordered(x),fit.lc,col="red")
summary(bw)
