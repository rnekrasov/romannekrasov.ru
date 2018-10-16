#**********************************************************
#Модель коинтеграции, календарные спреды Brent, OFZ2-OFZ10
#**********************************************************
#-----------------------------------
#Спред Brent ближний и дальний
#-----------Brent1------------------
#data1 <- dbGetQuery(con, "SELECT number,time,price,date,date_trunc('minute', time) as dayli_gr from brent1 
#                    where (time between '10:00:00' and '12:00:00') 
#                    and date='2016-12-09' order by dayli_gr")

#-----------Brent2------------------
#data2 <- dbGetQuery(con, "SELECT number,time,price,date,date_trunc('minute', time) as dayli_gr from brent2 
#                    where (time between '10:00:00' and '12:00:00') 
#                    and date='2016-12-09' order by dayli_gr")
#-----------------------------------
#library(egcm);library(timeDate)
#продумать как грамотно объединить
#x <- data.frame("2016-12-09",data1)
#y <- data.frame(as.POSIXct(paste(x[,1], x$time), format="%Y-%m-%d %H:%M:%S",tz="GMT"),data1$price)
#z1=xts(x$price, order.by=as.timeDate(y[,1]))
#x <- data.frame("2016-12-09",data2)
#y <- data.frame(as.POSIXct(paste(x[,1], x$time), format="%Y-%m-%d %H:%M:%S",tz="GMT"),data2$price)
#z2=xts(y$data2.price, order.by=as.timeDate(y[,1]))
#spread=z2-z1
#plot(spread,main="Brent-WTI")
#y1=unclass(z1)
#y2=unclass(z2)
#egcm(y1,y2)#x,y
#plot(egcm(y1, y2))
#---------------------------------
#Спред OFZ2-OFZ10
#-----------OFZ2------------------
#data1 <- dbGetQuery(con, "SELECT * from ofz24018 
#                    where (time between '10:00:00' and '12:00:00') 
#                    and date='2016-12-09' order by time")

#-----------OFZ10-----------------
#data2 <- dbGetQuery(con, "SELECT * from ofz29009 
#                    where (time between '10:00:00' and '12:00:00') 
#                    and date='2016-12-09' order by time")
#---------------------------------
#Разложить временной ряд на частоты (гармоники) Фурье,вейвлеты