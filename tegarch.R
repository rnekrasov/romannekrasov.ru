#**************************************************
#START
#**************************************************
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '10:05:00'
                   and date='19-01-2018'
                   order by time")
y=diff(as.numeric(data$price))

##estimate a 1st. order Beta-t-EGARCH model and store the output in mymod:
library(betategarch)
mymod <- tegarch(y)
#print estimates and standard errors:
print(mymod)
#graph of fitted volatility (conditional standard deviation):
par(mfrow = c(2, 1))
plot(fitted(mymod))
plot(y,type="l")
#graph of fitted volatility and more:
plot(fitted(mymod, verbose=TRUE))
#plot forecasts of volatility 1-step ahead up to 20-steps ahead:
plot(predict(mymod, n.ahead=20))
#full variance-covariance matrix:
vcov(mymod)
#plot forecasts of volatility 1-step ahead up to 10-steps ahead:
plot(predict(mymod, n.ahead=10))

