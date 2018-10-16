# —читываем тики из базы данных postgres
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '10:05:00' 
and ticker='USD000UTSTOM'
and date='02-03-2018'
order by time")

library(h2o)
#h2o.init()

x1<-data$price
x2<-c(0, diff(data$price))
x3<-lag(data$price,1)
x4<-((data$price-mean(data$price))/max(abs(data$price-mean(data$price))))
y1<-data$price

train<-as.h2o(data.frame(x1,x2,x3,x4,y1))

#model1<-h2o.deeplearning(x=1, y=2, training_frame=train)
#model2<-h2o.deeplearning(x=1, y=2, training_frame=train,
#                 activation = "Rectifier", hidden = c(200,200), 
#                 epochs = 10,seed=123456)
model3<-h2o.deeplearning(x=1:4, training_frame=train,
                         activation = "Rectifier", hidden = c(200,200), 
                         epochs = 10,seed=123456,autoencoder = TRUE,
                         max_w2=0.01)
model3

