# Считываем тики из базы данных postgres
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '23:50:00' 
and ticker='USD000UTSTOM'
and date='22-02-2018'
order by time")

# Считываем дневные данные валютных пар из базы OANDA
library(quantmod)
ticker<-"USD/RUB"
getSymbols(ticker, src="oanda")
price1<-as.ts(USDRUB,start = 1, end = 179)
price1_diff<-diff(price1)
ticker<-"EUR/USD"
getSymbols(ticker, src="oanda")
price2<-as.ts(EURUSD,start = 1, end = 179)
price2_diff<-diff(price2)

# Лог-приращения ряда тиков
price<-as.numeric(data$price)
log_return<-diff(price)
hist(log_return)
plot.ts(data$price)
plot.ts(log_return)

# Машинное обучение
library(mlr);library(h2o)

# Регрессии glm, gbm
# исходные ряды
data<-data.frame(as.numeric(price1),as.numeric(price2)) 
colnames(data)<-c("USDRUB","EURUSD")
data<-as.h2o(data)
h2o.glm(y = "USDRUB",x="EURUSD", training_frame = data, family = "gaussian",
        nfolds = 0, alpha = 0.1, lambda_search = FALSE)
h2o.gbm(y = "USDRUB", x = "EURUSD", training_frame = data,
        ntrees = 3, max_depth = 3, min_rows = 2)

# приращения рядов
data<-data.frame(as.numeric(price1_diff),as.numeric(price2_diff)) 
colnames(data)<-c("USDRUB","EURUSD")
data<-as.h2o(data)
h2o.glm(y = "USDRUB",x="EURUSD", training_frame = data, family = "gaussian",
        nfolds = 0, alpha = 0.1, lambda_search = FALSE)
h2o.gbm(y = "USDRUB", x = "EURUSD", training_frame = data,
        ntrees = 3, max_depth = 3, min_rows = 2)

# кластеризация k-средние
x1<-data$price[2:length(data$price)]
x2<-diff(data$price)
x<-data.frame(x1,x2)
train.hex<-as.h2o(x)
test.hex<-as.h2o(x)
kmeans_tick<-h2o.kmeans(training_frame=train.hex,k=10,
                        x=c("x1","x2"),nfolds=3,seed=2)
summary(kmeans_tick)
kmeans_tick_predict<-h2o.predict(kmeans_tick,train.hex)
kmeans_tick_predict_test<-h2o.predict(kmeans_tick,test.hex)
result<-data.frame(as.data.frame(kmeans_tick_predict_test),
                   data$time[2:length(data$price)],
                   data$price[2:length(data$price)])
