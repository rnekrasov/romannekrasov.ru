#**************************************************
#START
#**************************************************
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '10:05:00'
and date='19-01-2018'
order by time")
library(timeDate);library(highfrequency)
z1 <- data.frame("2017-01-09",data)
z2 <- data.frame(as.POSIXct(paste(z1[,1], z1$time), 
                            format="%Y-%m-%d %H:%M:%S",tz="GMT"),data$price)
z3=xts(z1$price, order.by=as.timeDate(z2[,1]))
colnames(z3) <- c("PRICE")
ts = z3$PRICE;
#Previous tick aggregation to the 5-minute sampling frequency:
tsagg1sec = aggregatets(ts,on="seconds",k=1);

## !!! INCREASE THE NUMBER OF MCMC ITERATIONS !!!
## LOAD DATA
dem2gbp=unclass(z3$PRICE)#unclass(tsagg1sec)
library(bayesGARCH)
#data(dem2gbp)
#y <- dem2gbp[1:750]
y <- dem2gbp[1:length(dem2gbp)]
## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = length(dem2gbp)))
## MCMC ANALYSIS (using coda)
plot(MCMC)
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
## GARCH(1,1) WITH NORMAL INNOVATIONS
MCMC <- bayesGARCH(y, lambda = 100, delta = 500,
                   control = list(n.chain = 2, l.chain = length(dem2gbp)))
## GARCH(1,1) WITH NORMAL INNOVATIONS AND
## WITH COVARIANCE STATIONARITY CONDITION
addPriorConditions <- function(psi){psi[2] + psi[3] < 1}
MCMC <- bayesGARCH(y, lambda = 100, delta = 500,
                   control = list(n.chain = 2, l.chain = length(dem2gbp),
                                  addPriorConditions = addPriorConditions))

