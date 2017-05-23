rm(list=ls())
library(quantmod)


data <- new.env()

getSymbols( 'PAYEMS'
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)

mydata <- data$PAYEMS
logpayems <- ts(log(mydata$PAYEMS),frequency = 12,start = c(1939,2),end = c(2017,3))
plot(logpayems,xlab='Time',ylab='log(PAYEMS)',main='Log level plot of PAYEMS')
legend('bottomright',c('Downloaded from website','read data'),lty=c(1,2),col= c('black','green'),cex=.7)



ts.data <- read.csv('C:/ECMT 674/HM03/PAYEMS.csv',sep=',',header=T)
payemslog.ts <- ts(log(ts.data$PAYEMS),frequency = 12,start = c(1939,2),end = c(2017,3))
lines(payemslog.ts,col='green',lty=2)

##question 2

time <- seq(1,length(logpayems),by=1)
mod1 <- lm(logpayems ~ time)
summary(mod1)

fitted <- ts(mod1$fitted.values,start=c(1939,1),freq=12)
gap <- ts(mod1$residuals,start=c(1939,1),freq=12)

plot(fitted,xlab='Time',ylab='fitted values',main='Trend plot',type='l',col='blue')
plot(gap,xlab='time',ylab='error',main='Gap',type='l')

par(mfrow=c(1,1))
acf(mod1$residuals)

###3 question
library(fUnitRoots)

adfTest(logpayems,9,type = c('c'))
adfTest(logpayems,9,type = c('ct'))

## the lag orede was chosen by the software as 9. We fail to reject the null that is the series has a unit root
## This series has a stochastic trend.


### 4th question
plot(diff(logpayems),type='l',main= 'Loglevel plot after first difference')
diff.logpayems <- diff(logpayems)
adfTest(diff.logpayems,type=c('c'))
adfTest(diff.logpayems,type=c('ct'))
adfTest(diff.logpayems,type=c('nc'))


## we fail to reject the null for stationarity around drift and stationarity around drift and trend
## This process is stochastic.


##problem 2
## question 1

data_epu <- read.csv("C:/ECMT 674/Hm01/epu_mpu.csv",header = T,sep=",")
mpu <- ts(c(data_epu$X2..Monetary.policy),start=c(1985,1),frequency = 12)

y=window(diff.logpayems,start=c(1985,1),end=c(2016,12),freq=12)
x=window(mpu,start=c(1985,1),end=c(2016,12),freq=12)

reg1 <- dynlm(y~lag(y,-1)+lag(x,-1))
summary(reg1)
AIC(reg1)
BIC(reg1)

reg2 <- dynlm(y~lag(y,-1)+lag(y,-2)+lag(x,-1))
summary(reg2)
AIC(reg2)
BIC(reg2)

reg3 <- dynlm(y~lag(y,-1)+lag(y,-2)+lag(y,-3)+lag(x,-1))
summary(reg3)
AIC(reg3)
BIC(reg3)

reg4 <- dynlm(y~lag(y,-1)+lag(y,-2)+lag(y,-3)+lag(y,-4)+lag(x,-1))
summary(reg4)
AIC(reg4)
BIC(reg4)

reg5 <- dynlm(y~lag(y,-1)+lag(y,-2)+lag(y,-3)+lag(y,-4)+lag(y,-5)+lag(x,-1))
summary(reg5)
AIC(reg5)
BIC(reg5)

##reg2 has the best BIC

# Question3 forecast
library(dynlm)

prediction=matrix(NA,nrow=length(y)-120,ncol=5)
colnames(prediction)=c("realize","nonchange","fixed","rolling","recursive")

yfix=window(y,start=c(1985,1),end=c(1994,12))
xfix=window(x,start=c(1985,1),end=c(1994,12))

### no change forecast
for(i in 1:(length(y)-120)){
  prediction[i,1] <- y[120+i]# realization
  prediction[i,2] <- y[119+i]
  }
  
### recursive
for(i in 1:(length(y)-120)){
  yrec=window(y,start=c(1985,1),end=c(1994,(11+i)))
  xrec=window(x,start=c(1985,1),end=c(1994,(11+i)))
  rec=dynlm(yrec~lag(yrec,-1)+lag(yrec,-2)+lag(xrec,-1))
  prediction[i,5]=rec$coefficients[1]+rec$coefficients[2]*y[119+i]+rec$coefficients[3]*y[118+i]+rec$coefficients[4]*x[119+i]
  
}
  
### rolling

for(i in 1:(length(y)-120)){
  yroll=window(y,start=c(1985,i),end=c(1994,(11+i)))
  xroll=window(x,start=c(1985,i),end=c(1994,(11+i)))
  roll=dynlm(yroll~lag(yroll,-1)+lag(yroll,-2)+lag(xroll,-1))
  prediction[i,4]=roll$coefficients[1]+roll$coefficients[2]*y[119+i]+roll$coefficients[3]*y[118+i]+roll$coefficients[4]*x[119+i]
  
}

### fixed

for (i in 1:(length(y)-120)){
  model <-dynlm(yfix~lag(yfix,-1)+lag(yfix,-2)+lag(xfix,-1))
  prediction[i,3]=model$coefficients[1]+model$coefficients[2]*y[119+i]+model$coefficients[3]*y[118+i]+model$coefficients[4]*x[119+i]
}

### 

plot(ts(prediction[,1],start=c(1995,1),frequency = 12),type='l',xlab='Time',ylab='diff(log(payems))',main='different forecasts vs realization')
lines(ts(prediction[,2],start=c(1995,1),frequency = 12),type='l',lty=2,col='blue')
lines(ts(prediction[,3],start=c(1995,1),frequency = 12),type='l',lty=3,col='green')
lines(ts(prediction[,4],start=c(1995,1),frequency = 12),type='l',lty=4,col='red')
lines(ts(prediction[,5],start=c(1995,1),frequency = 12),type='l',lty=5,col='yellow')  
legend('bottomright',c('Realization','nochange','fixed','rolling','recursive'),lty=c(1,2,3,4,5),col= c('black','blue','green','red','yellow'),cex=.7)

### MSFE
msfe <- matrix(c(rep(0,4)),ncol = 4)
colnames(msfe) <- c('nochange','fixed','rolling','recursive')
for(i in 1:4){
  msfe[i] <- mean((prediction[,i+1]-prediction[,1])^2)
}
min(msfe)

##don't consider this.
mape <- matrix(c(rep(0,4)),ncol = 4)
colnames(mape) <- c('nochange','fixed','rolling','recursive')
for(i in 1:4){
  mape[i] <- mean(abs((prediction[,1]-prediction[,i+1])/prediction[,1]))
}

### 4th question
## reg3 is the full sample model for overlapping period.

payems.impulse <- rep(0,length(y))
library(timeSeries)
mpu.new <- mpu + colSds(mpu)
#lnpayms[-1]<- 0;mpu.new[0]<- 0;lnpayms[-2]<- 0;lnpayms[0]<- 0



for (i in 4:(length(y)+3)){
 payems.impulse[i] <- reg2$coefficients[1]+ reg2$coefficients[2]*y[i-1]+ reg2$coefficients[3]*y[i-2]+ reg2$coefficients[4]*mpu.new[i-1]
}
payems.impulse[1] <- reg2$coefficients[1]
payems.impulse[2] <- reg2$coefficients[1]+ reg2$coefficients[2]*y[1]+ reg2$coefficients[4]*mpu.new[1]
payems.impulse[3] <- reg2$coefficients[1]+ reg2$coefficients[2]*y[2]+ reg2$coefficients[3]*y[1]+ reg2$coefficients[4]*mpu.new[2]

plot(ts(y[1:60],start=c(1985,1),frequency = 12),type='l',ylim=c(-.006,0.009),lty=1,ylab='response',main='Impulse for 5 years')
lines(ts(payems.impulse[1:60],start=c(1985,1),frequency = 12),col='blue',type='l',lty=2)
legend('topright',c('Realization','Impulse response'),lty=c(1,2),col=c('black','blue'),cex=.7)


plot(y,type='l',ylim=c(-.006,0.009),lty=1,ylab='response',main='Impulse for full time period')
lines(ts(payems.impulse,start=c(1985,1),frequency = 12),col='blue',type='l',lty=2)
legend('topright',c('Realization','Impulse response'),lty=c(1,2),col=c('black','blue'),cex=.7)


### 5 th question
library(strucchange)

dataQLR <- ts.intersect(y,lag(y,-1),lag(y,-2),lag(x,-1))
FQLR <- Fstats(reg2,data=dataQLR,from = c(1990, 1),to = c(2015,12))
scQLR <- sctest(FQLR,type="supF")
scQLR
plot(FQLR,alpha=0.05,main='Alpha = 0.05')
FQLR$breakpoint

mod <- dynlm(y ~ lag(x,-1))
dataQLR1 <- ts.intersect(y,lag(x,-1))
FQLR1<- Fstats(mod, data = dataQLR1, from = c(1990, 1),to = c(2015, 12))
scQLR1 <- sctest(FQLR1,type="supF")
scQLR1
plot(FQLR1,main='alpha=0.05')
FQLR1$breakpoint

