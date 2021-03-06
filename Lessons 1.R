# ��������� ���� �� ���� ������ postgres
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '23:50:00' 
and ticker='USD000UTSTOM'
and date='02-02-2018'
order by time")

# ���-���������� ����
price<-as.numeric(data$price)
#log_return<-log(price[2:length(price)])-log(price[1:length(price)-1])
log_return<-diff(price)
hist(log_return)
plot.ts(data$price)
plot.ts(log_return)

# ��������� ������� ������ �������� ���� �� ���� OANDA
library(quantmod)
ticker<-"USD/RUB"
getSymbols(ticker, src="oanda")
price<-as.ts(USDRUB,start = 1, end = 179)

## ������ ������-��������
#�������
fit <- HoltWinters(price, beta=FALSE, gamma=FALSE)
plot(fit)
#�������, �����
fit <- HoltWinters(price, gamma=FALSE)
plot(fit)
#�������, �����, ����������
#���� ������ ��� ������� ����������, ������ ������
fit <- HoltWinters(price)

#���������������� ����������� � �������������� ������
library(forecast)
fit<-ets(price)
plot(fit)
fit<-ets(gas)
plot(fit)

# ������ ARIMA ������� P, D, Q
library(forecast)
# ������ �����
p<-1; d<-1; q<-1
fit1 <- arima(price, order=c(p, d, q))
# �������������� �����
fit2 <- auto.arima(price)
# ��������� ���������
p<-0; d<-1; q<-0
fit3 <- arima(price, order=c(p, d, q))
# ��������� �������� � �������������� �������� ������, 
# �����-���� ���� �� �������������� �������� (p>0.05 - ��� ��������������)
accuracy(fit)
Acf(residuals(fit))
Box.test(residuals(fit), lag=24, fitdf=4, type="Ljung")
# ������������� 5 �����
forecast(fit2, 5)
par(mfrow = c(1, 1))
plot(forecast(fit2, 5))

