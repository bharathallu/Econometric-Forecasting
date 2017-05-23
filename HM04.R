##
rm(list=ls())

##read data

cpi.data <- read.csv('C:/ECMT 674/HM04/CPI.csv',header = T,sep=",")
gdp.data <- read.csv('C:/ECMT 674/HM04/GDP.csv',header = T,sep=",")
ffr.data <- read.csv('C:/ECMT 674/HM04/FEDFUNDS.csv',header = T,sep=",")

## 
cpi.ts <- ts(cpi.data[52:281,2],start=c(1959,4),freq=4)
gdp.ts <- ts(log(gdp.data[52:281,2])*100,start=c(1959,4),freq=4)
ffr.ts <- ts(ffr.data[22:251,2],start=c(1959,4),freq=4)

plot(cpi.ts,type='l',main='CPI')
plot(gdp.ts,type='l',main='GDP scaled')
plot(ffr.ts,type='l',main='FFR')


### unit root tests
library(fUnitRoots)

adfTest(cpi.ts,4,type = c('c'))# fail to reject non - stationarity
adfTest(cpi.ts,4,type = c('ct'))# reject non-stationarity

adfTest(gdp.ts,4,type = c('c'))
adfTest(gdp.ts,4,type = c('ct'))

adfTest(ffr.ts,4,type = c('c')) # fail to reject


### de trending 

n <- length(cpi.ts)
t<- c(1:n)     
ltq <- residuals(lm(cpi.ts~t))
stat.cpi <- ts(ltq[2:n],start=c(1960,1),frequency=4)
plot(stat.cpi,type='l',main='cpi gap')
adfTest(stat.cpi,4,type = c('c'))

stat.gdp <- diff(gdp.ts)
plot(stat.gdp,type='l',main='diff(log(gdp))')
adfTest(stat.gdp,4,type = c('c'))


stat.ffr <- diff(ffr.ts)
plot(stat.ffr,type='l',main='diff(ffr)')
adfTest(stat.ffr,4,type = c('c'))

## constructing VAR
library(vars)

t1 <- length(ffr.ts)
tab <- cbind(stat.gdp[1:(t1-37)],stat.cpi[1:(t1-37)],stat.ffr[1:(t1-37)])
t2 <- dim(tab)[1]
varmod <- VAR(tab,type = "const", season = NULL, lag.max = 8, ic= "SC")
summary(varmod)

varpred <- predict(varmod, n.ahead = 37, ci = 0.95)

predffrdata = matrix(NA,nrow=37,ncol=4)
predffrdata[,1]= varpred$fcst$y3[,1]
predffrdata[,2]= varpred$fcst$y3[,2]
predffrdata[,3]= varpred$fcst$y3[,3]
predffrdata[,4]= stat.ffr[193:229]

xlab = seq(from=2008.25, to=2017.25, by = 0.25)

plot(xlab,predffrdata[,1],type="l",lwd=2,col="blue",ylab="Change in FFR",xlab="Forecast Horizon", main = "Forecasts",ylim=c(-2,2))
lines(xlab,predffrdata[,2],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predffrdata[,3],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predffrdata[,4],type="l")

predcpidata = matrix(NA,nrow=37,ncol=4)
predcpidata[,1]= varpred$fcst$y2[,1]
predcpidata[,2]= varpred$fcst$y2[,2]
predcpidata[,3]= varpred$fcst$y2[,3]
predcpidata[,4]= stat.cpi[193:229]

plot(xlab,predcpidata[,1],type="l",lwd=2,col="blue",ylab="Price Gap",xlab="Forecast Horizon", main = "Forecasts",ylim=c(-10,12))
lines(xlab,predcpidata[,2],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predcpidata[,3],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predcpidata[,4],type="l")

predgdpdata = matrix(NA,nrow=37,ncol=4)
predgdpdata[,1]= varpred$fcst$y1[,1]
predgdpdata[,2]= varpred$fcst$y1[,2]
predgdpdata[,3]= varpred$fcst$y1[,3]
predgdpdata[,4]= stat.gdp[193:229]


plot(xlab,predgdpdata[,1],type="l",lwd=2,col="blue",ylab="Output Growth",xlab="Forecast Horizon", main = "Forecasts",ylim=c(-2,3))
lines(xlab,predgdpdata[,2],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predgdpdata[,3],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predgdpdata[,4],type="l")

#mean squared error and median squared error
mse <- cbind(mean((predgdpdata[,4]-predgdpdata[,1])^2),mean((predcpidata[,4]-predcpidata[,1])^2),mean((predffrdata[,4]-predffrdata[,1])^2) )
mse
mse1 <- cbind(median((predgdpdata[,4]-predgdpdata[,1])^2),median((predcpidata[,4]-predcpidata[,1])^2),median((predffrdata[,4]-predffrdata[,1])^2) )
mse1


## structural VAR
amat <- matrix(NA,nrow=3,ncol=3)

amat[1, 2] <- 0 
amat[1, 3] <- 0
amat[2, 3] <- 0
amat

svarmod <- SVAR(x = varmod, Amat = amat, Bmat = NULL)
svarmod

res1 <- irf(svarmod, impulse = "y3", response = c("y1"), n.ahead = 5*4, boot = TRUE) 
plot(res1)

res2 <- irf(svarmod, impulse = "y3", response = c("y2"), n.ahead = 5*4, boot = TRUE)
plot(res2)

res3 <- irf(svarmod, impulse = "y3", response = c("y3"), n.ahead = 5*4, boot = TRUE)
plot(res3)


fevd(svarmod,n.head = 5*4)

## from 1980-2007
mat1 <- cbind(stat.gdp[81:192],stat.cpi[81:192],stat.ffr[81:192])
dim(mat1)[1]

varmod1 = VAR(mat1,type = "const", season = NULL, lag.max = 8, ic= "SC")
summary(varmod1)

svarmod1 = SVAR(x = varmod1, Amat = amat, Bmat = NULL)
svarmod1

ir1 = irf(svarmod1, impulse = "y3", response = c("y1"), n.ahead = 5*4, boot = TRUE) 
plot(ir1)

ir2 = irf(svarmod1, impulse = "y3", response = c("y2"), n.ahead = 5*4, boot = TRUE)
plot(ir2)

ir3 = irf(svarmod1, impulse = "y3", response = c("y3"), n.ahead = 5*4, boot = TRUE)
plot(ir3)

#from 1990-2007
mat2 <- cbind(stat.gdp[121:192],stat.cpi[121:192],stat.ffr[121:192])
dim(mat2)[1]

varmod2 = VAR(mat2,type = "const", season = NULL, lag.max = 8, ic= "SC")
summary(varmod2)

svarmod2 = SVAR(x = varmod2, Amat = amat, Bmat = NULL)
svarmod2

ir11 = irf(svarmod2, impulse = "y3", response = c("y1"), n.ahead = 5*4, boot = TRUE) 
plot(ir11)

ir22 = irf(svarmod2, impulse = "y3", response = c("y2"), n.ahead = 5*4, boot = TRUE)
plot(ir22)

ir33 = irf(svarmod2, impulse = "y3", response = c("y3"), n.ahead = 5*4, boot = TRUE)
plot(ir33)

## shadow rate instead of ffr.

shadow.data <- read.csv('C:/ECMT 674/HM04/shadowrate.csv',sep=',')
monthlysr <- ts(shadow.data[,2],start=c(1960,1),frequency=12)
sr.qtr <- aggregate(monthlysr, nfrequency=4, mean)
sr.qtr <- as.numeric(sr.qtr)
sr.qtr <- c(3.99,sr.qtr)
sr.qtr <-  ts(sr.qtr,start=c(1959,4),freq=4)

plot(sr.qtr,type='l',main='shadow rate')

adfTest(sr.qtr,lags = 8,type = 'c')
stat.sr <- diff(sr.qtr)
plot(stat.sr,type='l',main='diff(shadow rate')
adfTest(stat.sr,lags = 8,type = 'c')

t1 <- length(stat.sr)
tab1 <- cbind(stat.gdp[1:(t1-31)],stat.cpi[1:(t1-31)],stat.sr[1:(t1-31)])
t2 <- dim(tab1)[1]
varmod4 <- VAR(tab1,type = "const", season = NULL, lag.max = 8, ic= "SC")
summary(varmod4)

varpred4 <- predict(varmod4, n.ahead = 31, ci = 0.95)

predffrdata1 = matrix(NA,nrow=31,ncol=4)
predffrdata1[,1]= varpred4$fcst$y3[,1]
predffrdata1[,2]= varpred4$fcst$y3[,2]
predffrdata1[,3]= varpred4$fcst$y3[,3]
predffrdata1[,4]= stat.sr[193:223]

xlab = seq(from=2008.25, to=2015.75, by = 0.25)

plot(xlab,predffrdata1[,1],type="l",lwd=2,col="blue",ylab="Change in FFR",xlab="Forecast Horizon", main = "Forecasts",ylim=c(-3,3))
lines(xlab,predffrdata1[,2],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predffrdata1[,3],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predffrdata1[,4],type="l")

predcpidata1 = matrix(NA,nrow=31,ncol=4)
predcpidata1[,1]= varpred4$fcst$y2[,1]
predcpidata1[,2]= varpred4$fcst$y2[,2]
predcpidata1[,3]= varpred4$fcst$y2[,3]
predcpidata1[,4]= stat.cpi[193:223]

plot(xlab,predcpidata1[,1],type="l",lwd=2,col="blue",ylab="Price Gap",xlab="Forecast Horizon", main = "Forecasts",ylim=c(-10,17))
lines(xlab,predcpidata1[,2],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predcpidata1[,3],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predcpidata1[,4],type="l")

predgdpdata1 = matrix(NA,nrow=31,ncol=4)
predgdpdata1[,1]= varpred4$fcst$y1[,1]
predgdpdata1[,2]= varpred4$fcst$y1[,2]
predgdpdata1[,3]= varpred4$fcst$y1[,3]
predgdpdata1[,4]= stat.gdp[193:223]


plot(xlab,predgdpdata1[,1],type="l",lwd=2,col="blue",ylab="Output Growth",xlab="Forecast Horizon", main = "Forecasts",ylim=c(-2,3))
lines(xlab,predgdpdata1[,2],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predgdpdata1[,3],type="l", lty=2, lwd=2,col="blue")
lines(xlab,predgdpdata1[,4],type="l")

###using full data
tT <- length(sr.qtr)
mat5 = cbind(stat.gdp[1:tT-1],stat.cpi[1:tT-1],stat.sr[1:tT-1])
tTt = dim(mat5)[1]
monetaryVAR3 = VAR(mat5,type = "const", season = NULL, lag.max = 8, ic= "SC")
summary(monetaryVAR3)
varpred5 = predict(monetaryVAR3, n.ahead = 20, ci = 0.95)
x = seq(from=1960.00, to=2020.00, by = 0.25)
fanchart(varpred5,plot.type = c("single"))


