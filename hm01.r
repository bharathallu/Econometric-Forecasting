## problem-2 ###

rm(list=ls())
## read the data ##
data<- read.csv("C:/ECMT 674/Hm01/s&p500.csv",header = T,sep=",")
## adj.closing prices ##
p_t<- data.frame(data[,7])
n<- nrow(p_t)
## calculate returns ##
ret <- log(p_t[2:n, 1]) - log(p_t[1:(n-1), 1])
dates <- as.Date(as.character(data[,1]), "%m/%d/%Y")
w <- 0
ret <- c(ret,w)
## plot the returns ##
par(mfrow=c(1,1))
plot(dates,ret,type="l",main="Daily returns",ylab="returns",col="green")


## problem 3 ##

ret_1<- as.matrix(ret,nrow=1)
data_new <- cbind(data,ret_1)
data_new$Date <- as.Date(data_new$Date,"%m/%d/%Y")

##  Get months
data_new$Month <- months(data_new$Date)
## Get years
data_new$Year <- format(data_new$Date,format="%Y")

##  Aggregate 'returns' on months and year and get sd ##
sd_mon <- aggregate( ret_1 ~ Month + Year , data_new , sd )

library(zoo)
sd_mon$date <- paste(sd_mon$Year,sd_mon$Month, sep="-")
sd_mon$date1 <- as.yearmon(sd_mon$date,"%Y-%b")
sd_mon1 <- sd_mon[,c(3,5)]


sd_mon1$sdmonthly <- sd_mon1$ret_1
sd_mon1$date1 <- sd_mon1$date1[order(sd_mon1$date1)]
sd_mon1$sd_perc <- sd_mon1$sdmonthly*100

## final answer ##
sd_mon1 <- sd_mon1[,2:4]



##problem-4##
## read monthly VIX data ##
data_vix <- read.csv("C:/ECMT 674/Hm01/VIXCLS.csv",header = T,sep=",")
dates_vix <- as.Date(as.character(data_vix[,1]))
## plot VIX monthly values ##
plot(dates_vix,data_vix[,2],type="l",main="Monthly VIX",ylab="VIX",col="blue",xlab="Time")


## problem-5##
## read EPU index ##
data_epu <- read.csv("C:/ECMT 674/Hm01/epu_mpu.csv",header = T,sep=",")
data_epu <- data_epu[1:384,1:3]
dates_epu <- as.Date(as.character(data_epu[,1]),"%m/%d/%Y")
               
#df <- data.frame( Month = data_epu[,2] , Day = data_epu[,1], Year = data_epu[,3] )
#data_epu$Date <- as.Date( paste( df$Month , df$Day , df$Year, sep = "." )  , format = "%m.%d.%Y" )

## plot EPU and MPU ##

par(mfrow=c(1,1))
plot(dates_epu,data_epu[,2],type='l',main='Economic and Monetary Policy Uncertainty Index',col="blue",lty=6,xlab="Time",ylab="Index",ylim=c(0,300))
lines(dates_epu,data_epu[,3],type="l",col="red",lty=3)
legend("topright",c("EPU","MPU"),lty=c(6,3),col=c("blue","red"),cex=.5)


## problem - 6##

par(mfrow=c(1,1))
## standardize EPU data from 1/1990 to 12/2016##
data_epu_scale <- scale(data_epu[61:384,2],center = T,scale = T)
data_epu_mpscale <- scale(data_epu[61:384,3],center = T,scale = T)

## standardize the VIX Index from 1/1990 to 12/2016 ##
data_vix_scale <- scale(as.numeric(data_vix[1:324,2]),center = T,scale = T)

## standardize the monthly std dev of returns ##
data_sdmon_scale <- scale(sd_mon1$sdmonthly[361:684],center = T, scale=T)

## plot the standardized data ##
plot(dates_epu[61:384],data_epu_scale,type='l',main='Standardized volatility',col="blue",xlab="Time",ylab="Index",ylim=c(-2,6))
lines(dates_epu[61:384],data_epu_mpscale,type='l',col="green",lty=2)
lines(dates_epu[61:384],data_vix_scale,type="l",col="red",lty=3)
lines(dates_epu[61:384],data_sdmon_scale,type="l",lty=4)
legend("topright",c("EPU","MPU","VIX","mon_stdev"),lty=c(1,2,3,4),col=c("blue","green","red","black"),cex=.45,bty="n")


##answer##
big <- data.frame(cbind(data_epu_scale,data_epu_mpscale,data_vix_scale,data_sdmon_scale))
colnames(big) <- c("Epu","Mpu","Vix","mon_sd")

## 4x4 correlation matrix ##
cor(big)


## problem-7##
## read IP data ##
data_ip <- read.csv("C:/ECMT 674/Hm01/INDPRO.csv",header = T,sep=",")
ip=ts(data_ip[,2], frequency = 12, start=c(1919,1), end=c(2016,12))
n <- nrow(data_ip)

## calculate the IP growth rate ##
gro_rate <- c(rep(0,n))

for (i in 2:n) {
  gro_rate[i] <- ((ip[i] - ip[i-1])/(ip[i-1]))*100
  
}
data_ip <- cbind(data_ip,gro_rate)
dates_ip <- as.Date(as.character(data_ip[,1]))

## plot of IP growth rates ##
par(mfrow=c(1,1))
plot(dates_ip,gro_rate,type='l',main="Growth rate of IP",col="red",xlab="Time",ylab="Growth Rate")

## problem 8 ##

## Binding all indexes to a new dataframe##
##performing regression of growth rate of IP on all the standardized volatility indices##

data_reg1 <-  data.frame(cbind(data_ip$gro_rate[853:1116],data_vix_scale[1:264],data_epu_scale[1:264],data_epu_mpscale[1:264],data_sdmon_scale[1:264]))
colnames(data_reg1) <- c("Growth_rate","VIX_scale","EPU_scale","MPU_scale","SDMON_scale")

lms1 <- lm(Growth_rate ~ as.numeric(VIX_scale), data_reg1)
summary(lms1)

lms2 <- lm(Growth_rate ~ as.numeric(EPU_scale), data_reg1)
summary(lms2)

lms3 <- lm(Growth_rate ~ as.numeric(MPU_scale), data_reg1)
summary(lms3)

lms4 <- lm(Growth_rate ~ as.numeric(SDMON_scale) , data_reg1) 
summary(lms4)

### Sesitivity ###

lms1$coefficients[2]
lms2$coefficients[2]
lms3$coefficients[2]
lms4$coefficients[2]


## Note: I'm standardizing all the variables, it does not make a difference if you forecast with ##
## or without standardization ###
#### Problem 9 ####
#### create data set with variables in overlapping interval ####

data_fore <- data.frame(cbind(sd_mon1$sd_perc[361:684],as.numeric(as.character(data_vix$VALUE[1:324])),data_epu$X1..Economic.Policy.Uncertainty[61:384],data_epu$X2..Monetary.policy[61:384],data_ip$gro_rate[853:1176]))

## standardizing variables for RMSE comparision ##
sdmon_for=ts(data_fore[,1],start=c(1990,1),frequency=12)
sdmon_for <- scale(sdmon_for,center = T, scale = T)
vix_for =ts(data_fore[,2],start=c(1990,1),frequency=12)
vix_for <- scale(vix_for,center = T, scale = T)
epu_for = ts(data_fore[,3],start=c(1990,1),frequency=12)
epu_for <- scale(epu_for,center = T, scale = T)
mpu_for =  ts(data_fore[,4],start=c(1990,1),frequency=12)
mpu_for <- scale(mpu_for,center = T, scale = T)
ipgro_for =ts(data_fore[,5],start=c(1990,1),frequency=12)
#ipgro_for <- scale(ipgro_for,center = T, scale = T)

past=12*5
past

# Calculate the numbers of predicted value you would get
t<- past


#Creat vectors to store the forecasting results
fcast_sdmon <- matrix(NA,nrow=t,ncol=1)
fcast_vix <- matrix(NA,nrow=t,ncol=1)
fcast_epu <- matrix(NA,nrow = t,ncol=1)
fcast_mpu <- matrix(NA,nrow = t,ncol=1)

#  pseudo-out-of-sample forecasts, then save the results in vectors##

#### forecast with monthly sdev ####
for (i in 1:t)
{
  model=lm(ipgro_for[1:(264+i)]~ sdmon_for[1:(264+i)])
  fcast_sdmon[i]=model$coefficients[1]+model$coefficients[2]*sdmon_for[(264+i)]
}

rmse_RET <- sqrt(sum((fcast_sdmon - ipgro_for[265:324])^2/60))

#### forecast with VIX ####
for (i in 1:t)
{
  model=lm(ipgro_for[1:(264+i)]~ vix_for[1:(264+i)])
  fcast_vix[i]=model$coefficients[1]+model$coefficients[2]*vix_for[(264+i)]
}

rmse_VIX <- sqrt(sum((fcast_vix - ipgro_for[265:324])^2/60))

#### forecast with EPU ####
for (i in 1:t)
{
  model=lm(ipgro_for[1:(264+i)]~ epu_for[1:(264+i)])
  fcast_epu[i]=model$coefficients[1]+model$coefficients[2]*epu_for[(264+i)]
}

rmse_EPU <- sqrt(sum((fcast_epu - ipgro_for[265:324])^2/60))

#### forecast with MPU ####
for (i in 1:t)
{
  model=lm(ipgro_for[1:(264+i)]~ mpu_for[1:(264+i)])
  fcast_mpu[i]=model$coefficients[1]+model$coefficients[2]*mpu_for[(264+i)]
}

rmse_MPU <- sqrt(sum((fcast_mpu - ipgro_for[265:324])^2/60))


## no change forecast ##
nochange_for <- ipgro_for[264:323]
rmse_BM <- sqrt(sum((ipgro_for[265:324] - nochange_for)^2/60))


## die-bold tests ##
d_vix <- ((data_ip$gro_rate[1117:1176] - fcast_vix)^2) 
d_nc<-  ((data_ip$gro_rate[1117:1176]-nochange_for)^2)
d_epu <- ((data_ip$gro_rate[1117:1176] - fcast_epu)^2)
d_mpu <- ((data_ip$gro_rate[1117:1176] - fcast_mpu)^2)
d_sdmon <- ((data_ip$gro_rate[1117:1176] - fcast_sdmon)^2)

## t tests for various  models ##

t.test(d_vix - d_nc) ## vix is different when compared to no change
t.test(d_epu - d_nc) ## epu is different when compared to no change
t.test(d_mpu - d_nc)  ## mpu is different when compared to no change
t.test(d_sdmon - d_nc) ## sd_mon is different when compared to no change
