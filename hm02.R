rm(list=ls())

## generating a sequence of 100 observations ##
theta <- 0.9
set.seed(100)
e_t <- rnorm(100)
x <- rep(0,100)
x[1] <- e_t[1]
for(i in 2:100){
  x[i] <- (theta*x[i-1])+e_t[i]
}

## estimating the AR coefficient ##
 model <- arima(x,c(1,0,0))
model$coef[1]


### repeating it 5000 times ###
rm(list=ls())
n <- 5000
theta <- 0.9
coef <- rep(0,5000)
for(i in 1:n){
  set.seed(100+i)
  x <- rep(0,100)
  e_t <- rnorm(100)
  x[1]<- e_t[1]
  
  for(j in 2: 200){
    x[j] <- (theta*x[j-1])+e_t[j] 
  }
  
  model <- arima(x,c(1,0,0))
  coef[i] <- model$coef[1]
  
}

### Plotting the sample distribution of ar coefficient and calculate bias ###
par(mfrow=c(1,1))
myhist <- hist(coef)
multiplier <- myhist$counts / myhist$density
mydensity <- density(coef)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist,col="green",ylim=c(0,2050))
lines(mydensity)

bias <- sum(coef - theta)/5000
bias

## repeating with sample size of 500 ##

rm(list=ls())
n <- 5000
theta <- 0.9
coef <- rep(0,5000)
for(i in 1:n){
  set.seed(100+i)
  x <- rep(0,500)
  e_t <- rnorm(500)
  x[1]<- e_t[1]
  
  for(j in 2: 200){
    x[j] <- (theta*x[j-1])+e_t[j] 
  }
  
  model <- arima(x,c(1,0,0))
  coef[i] <- model$coef[1]
  
}


par(mfrow=c(1,1))
myhist <- hist(coef)
multiplier <- myhist$counts / myhist$density
mydensity <- density(coef)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist,col="green",ylim=c(0,1500))
lines(mydensity)

bias <- sum(coef - theta)/5000
bias
### problem 2 ###


rm(list=ls())


## generating a sequence ##
n <- 100
e_y <- rnorm(n)
y <- rep(0,n)
y[1] <- e_y[1]
for(i in 2:100){
  y[i] <- y[i-1]+e_y[i]
}

## generating a sequence ##

a_x <- rnorm(n)
x <- rep(0,n)
x[1] <- a_x[1]
for(i in 2:n){
  x[i] <- x[i-1]+a_x[i]
}

## regressing y onto x ##

lm1 <- lm(y ~ x)
summary(lm1)
summary(lm1)$coefficients[2,1] ##OLS estimate
summary(lm1)$r.squared ## r squared
summary(lm1)$coefficients[2,3] ## t statistic

## repeat the above 1000 times ##
r2 <- rep(0,1000)
tstat <- rep(0,1000)
for(i in 1:1000){

  e_y <- rnorm(n)
  a_x <- rnorm(n)
  y <- rep(0,n)
  y[1] <- e_y[1]
  x <- rep(0,n)
  x[1] <- a_x[1]
  
  for(j in 2:n){
    y[j] <- y[j-1]+e_y[j]
    x[j] <- x[j-1]+a_x[j]
  }
  
  lm1 <- lm(y ~ x)
  r2[i] <- summary(lm1)$r.squared
  tstat[i] <- summary(lm1)$coefficients[2,3]
  
}
hist(r2)
hist(tstat)
## 5,50,95 percentiles ##

quantile(r2,c(.05,.5,.95))
z <- quantile(tstat,c(.05,.5,.95))

## fraction where t stat exceeds 1.96 ##
sum(abs(tstat)>1.96)/1000

###6 repeat for different values of T ###

rm(list=ls())
fp <- NULL
tstatquant <- NULL
r2quant<- NULL

par(mfrow=c(3,2))

for(n in seq(from=50,to=1250,by=150)){
r2 <- rep(0,1000)
tstat <- rep(0,1000)
for(i in 1:1000){
  
  e_y <- rnorm(n)
  a_x <- rnorm(n)
  y <- rep(0,n)
  y[1] <- e_y[1]
  x <- rep(0,n)
  x[1] <- a_x[1]
  
  for(j in 2:n){
    y[j] <- y[j-1]+e_y[j]
    x[j] <- x[j-1]+a_x[j]
  }
  
  lm1 <- lm(y ~ x)
  r2[i] <- summary(lm1)$r.squared
  tstat[i] <- summary(lm1)$coefficients[2,3]
}

  
  frac <- sum(abs(tstat)>1.96)/1000
  fp <- rbind(fp,frac)
  
  

  quant.r2 <- c(quantile(r2,c(.05,.5,.95)),n)
  r2quant <- rbind(r2quant,quant.r2)
  
  quant.tstat <- c(quantile(tstat,c(.05,.5,.95)),n)
  tstatquant <- rbind(tstatquant,quant.tstat)

hist(r2,main=paste("T=",n))
hist(tstat,main = paste("T=",n,",frac=",frac))

  
}
row.names(tstatquant) <- NULL
row.names(r2quant) <- NULL
colnames(tstatquant)[4] <- "T"
colnames(r2quant)[4] <- "T"

tstatquant <- round(tstatquant,4)
r2quant <- round(r2quant,4)
par(mfrow=c(1,1))
plot(seq(50,1250,150),fp,type="b",ylim=c(.6,1),xlab="T",ylab="Fraction",main="Fraction approaches 0.95")

#### problem 7 ###

## repeat the above 1000 times ##
rm(list=ls())
n <- 100
r2 <- rep(0,1000)
tstat <- rep(0,1000)
for(i in 1:1000){
  
  e_y <- rnorm(n)
  a_x <- rnorm(n)
  y <- rep(0,n)
  y[1] <- e_y[1]
  x <- rep(0,n)
  x[1] <- a_x[1]
  
  for(j in 2:n){
    y[j] <- e_y[j]
    x[j] <- a_x[j]
  }
  
  lm1 <- lm(y ~ x)
  r2[i] <- summary(lm1)$r.squared
  tstat[i] <- summary(lm1)$coefficients[2,3]
  
}
hist(r2)
hist(tstat)
## 5,50,95 percentiles ##

quantile(r2,c(.05,.5,.95))
z <- quantile(tstat,c(.05,.5,.95))
z
## fraction where t stat exceeds 1.96 ##
sum(abs(tstat)>1.96)/1000

## repeat with different sample sizes ##
rm(list=ls())
fp <- NULL
tstatquant <- NULL
r2quant<- NULL

par(mfrow=c(3,2))

for(n in seq(from=50,to=1250,by=150)){
  r2 <- rep(0,1000)
  tstat <- rep(0,1000)
  for(i in 1:1000){
    
    e_y <- rnorm(n)
    a_x <- rnorm(n)
    y <- rep(0,n)
    x <- rep(0,n)
    
    for(j in 1:n){
      y[j] <- e_y[j]
      x[j] <- a_x[j]
    }
    
    lm1 <- lm(y ~ x)
    r2[i] <- summary(lm1)$r.squared
    tstat[i] <- summary(lm1)$coefficients[2,3]
  }
  
  
  frac <- sum(abs(tstat)>1.96)/1000
  fp <- rbind(fp,frac)
  
  
  
  quant.r2 <- c(quantile(r2,c(.05,.5,.95)),n)
  r2quant <- rbind(r2quant,quant.r2)
  
  quant.tstat <- c(quantile(tstat,c(.05,.5,.95)),n)
  tstatquant <- rbind(tstatquant,quant.tstat)
  
  hist(r2,main=paste("T=",n))
  hist(tstat,main = paste("T=",n,",frac=",frac))
  
  
}
row.names(tstatquant) <- NULL
row.names(r2quant) <- NULL
colnames(tstatquant)[4] <- "T"
colnames(r2quant)[4] <- "T"

tstatquant <- round(tstatquant,4)
r2quant <- round(r2quant,4)

par(mfrow=c(1,1))
plot(seq(50,1250,150),fp,type="b",ylim=c(0,.1),xlab="T",ylab="Fraction",main="Fraction approaches 0.05")

