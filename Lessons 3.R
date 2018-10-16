# Считываем тики из базы данных postgres
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '23:50:00' 
and ticker='USD000UTSTOM'
and date='15-05-2018'
order by time")

# Считываем дневные данные валютной пары из базы OANDA
library(quantmod)
ticker<-"USD/RUB"
getSymbols(ticker, src="oanda")
price<-as.ts(USDRUB,start = 1, end = 179)
log_ret_daily<-diff(price)

# Лог-приращения ряда
price<-as.numeric(data$price)
log_return<-diff(price)
hist(log_return)
plot.ts(data$price)
plot.ts(log_return)

# Геометрическое броуновское движение
library(sde)
plot(GBM(x=62,sigma=0.5))
plot(GBM(x=62,sigma=0.03))
plot(GBM(x=62,sigma=0.5))

# Прыжковая диффузионная волатильность на ВЧ-данных
library(highfrequency)
library(timeDate)
z1 <- data.frame("2017-01-09",data)
z2 <- data.frame(as.POSIXct(paste(z1[,1], z1$time), 
                            format="%Y-%m-%d %H:%M:%S",tz="GMT"),data$price)
z3 <- xts(z1$price, order.by=as.timeDate(z2[,1]))
medRV(rdata = z3, align.by ="seconds",align.period =5, makeReturns=TRUE)
rKernelCov(rdata = z3, period = 1, align.by ="seconds",align.period=1, makeReturns=TRUE)
rAVGCov(z3,period = 5,align.by ="seconds",align.period=5, makeReturns=TRUE)
rBPCov(z3, align.by ="seconds", align.period =5, makeReturns=TRUE)
rTPVar(rdata= z3, align.by= "seconds", align.period =5, makeReturns= TRUE)


# Стохастическая модель волатильности svsample/stochvol
# тики
library(stochvol)
res <- svsample(log_return, draws = 500, burnin = 100, priormu = c(-10, 1), priorphi = c(20, 1.1),priorsigma = 0.1)
par(mfrow = c(2, 1))
plot(price,type="l")
volplot(res, forecast = 0)

# дни
res <- svsample(log_ret_daily, draws = 500, burnin = 100, priormu = c(-10, 1), priorphi = c(20, 1.1),priorsigma = 0.1)
par(mfrow = c(2, 1))
plot(price,type="l")
volplot(res, forecast = 5)

# проверка на гомоскедастичность и гетероскедастичность
par(mfrow = c(2, 1))
acf(res$y)
pacf(res$y)

# Экспоненциально взвешенный метод скользящего среднего для вычисления волатильности
# тики
m1 <- EWMAvol(log_return)
par(mfrow = c(2, 1))
plot(data$price,type="l",ylab="USDRUB",col="blue")
plot(m1$Sigma.t,ylab="volatility",type="l")
legend("topright", inset=.02, 
       c(data$time[1],data$time[length(data$time)],date), 
       fill=topo.colors(4), horiz=TRUE, cex=0.8,bg="transparent")
max(m1$Sigma.t)

# дни
m1 <- EWMAvol(log_ret_daily)
par(mfrow = c(2, 1))
plot(price,type="l",ylab="USDRUB",col="blue")
plot(m1$Sigma.t,ylab="volatility",type="l")
max(m1$Sigma.t)

# GARCH
library(bayesGARCH)
usdrub_daily<-unclass(log_ret_daily)
y <- usdrub_daily[1:length(usdrub_daily)]
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = length(usdrub_daily)))
plot(MCMC)
