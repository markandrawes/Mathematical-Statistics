
## ## Importing the data
wr <- read.table("finans1_data.csv", header=TRUE, sep=";", as.is=TRUE)

## ## a)
## Overview of the data
## 
## Dimension of HE (number of rows and columns)
dim(wr)
## Column names
names(wr)
## The first rows
head(wr)
## The last rows
tail(wr)
## Default summary
summary(wr)
## Another summary function also including the data type
str(wr)

wr <- read.table("finans1_data.csv",header=TRUE,sep=";",as.is=TRUE)
dim(wr)
head(wr[ ,1:5],n=2)
wr[c(1,454) ,1]
sum(is.na(wr))
names(wr)[1:4]
summary(wr)[ ,1:3]



## ## b)
sum(!is.na(wr$SPY))
mean(wr$IWN)
var(wr$IWN)
sd(wr$IWN)
quantile(wr$SPY, type=2)

#empirical density of the 4 ETFs
par(mfrow=c(2,2))
hist(wr$AGG, prob=TRUE, col=2, main="Empirical density AGG")
hist(wr$VAW, prob=TRUE, col=3, main="Empirical density VAW")
hist(wr$IWN, prob=TRUE, col=4, main="Empirical density IWN")
hist(wr$SPY, prob=TRUE, col=5, main="Empirical density SPY")

#boxplot of the four ETFs
boxplot(list(wr$AGG,wr$VAW,wr$IWN,wr$SPY),names=c("AGG","VAW","IWN","SPY"), col=2:6, main="Boxplots for 4 ETFs")


## define new data frame with the EFT's we will work with 
wr2 <- wr[ ,c("AGG","VAW","IWN","SPY")]
## The full table
round(cbind(No.obs=apply(wr2,2,function(x){sum(!is.na(x))}),
      avg=apply(wr2,2,mean),
      var=apply(wr2,2,var),sd=apply(wr2,2,sd),
      t(apply(wr2,2,quantile,prob=c(0.25,0.5,0.75)))),digits=5)




## ## d)
## Determination of the correlation between ETFs 

cov(wr[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")])
round(cov(wr[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")]),digits=6)


## ## e)
## and determination of portfolio

alpha <- seq(0,1,by=0.01)
alphafunction <- 0.001444*alpha^2+0.001659*(1-alpha)^2+2*alpha*(1-alpha)*0.001180

plot(alpha,alphafunction,type="l",ylab="Variance")


(beta2<-var(wr[ ,c("EWG")])+var(wr[ ,c("EWW")])-2*cov(wr[ ,c("EWG")],wr[ ,c("EWW")]))
(beta1 <- 2*(-var(wr[ ,c("EWW")])+cov(wr[ ,c("EWG")],wr[ ,c("EWW")])))
(beta0 <- var(wr[ ,c("EWW")]))

(alpham <- -beta1/(2*beta2))

 port <- function(EFT1,EFT2){
   beta2<-var(wr[ ,EFT1])+var(wr[ ,EFT2])-
   2*cov(wr[ ,c(EFT1)],wr[ ,EFT2])
   beta1 <- 2*(-var(wr[ ,EFT2])+cov(wr[ ,EFT1],wr[ ,EFT2]))
   beta0 <- var(wr[ ,c(EFT2)])
   alpham <- -beta1/(2*beta2)
   vm <- beta2*alpham^2+beta1*alpham+beta0
   retr <- alpham * mean(wr[ ,EFT1]) + 
           (1 - alpham) * mean(wr[ ,EFT2])
   c(alpham = alpham, vm = vm, retr = retr)
 }
 rbind(EWGEWW=port("EWG","EWW"),AGGSPY=port("AGG","SPY"),
       VAWIWN=port("VAW","IWN"),VAWEWG=port("VAW","EWG"),
       VAWEWW=port("VAW","EWW"),IWNEWG=port("IWN","EWG"))



  

## ## f)
## Validation of a model for AGG

tab <- cbind(m = apply(wr2,2,mean), var = apply(wr2,2,var), 
             sd = apply(wr2,2,sd))
rownames(tab) <- c("AGG","VAW","IWN","SPY")  
tab

par(mfrow=c(2,2))
qqnorm(wr2[ ,"AGG"], main="Normal QQ Plot for AGG")
qqline(wr2[ ,"AGG"])
qqnorm(wr2[ ,"VAW"], main="Normal QQ Plot for VAW")
qqline(wr2[ ,"VAW"])
qqnorm(wr2[ ,"IWN"], main="Normal QQ Plot for IWN")
qqline(wr2[ ,"IWN"])
qqnorm(wr2[ ,"SPY"], main="Normal QQ Plot for SPY")
qqline(wr2[ ,"SPY"])
  



## ## g)
## confidence intervals

qt(0.975, 453)

## The 95% confidence interval for AGG
t.test(wr$AGG, conf.level=0.95)$conf.int
## Do the same for the other ETFs


(CImAGG <- mean(wr[ ,"AGG"]) + c(-1, 1) * qt(0.975, df = 453) * 
sd(wr[ ,"AGG"])/sqrt(454))
(CImVAW <- mean(wr[ ,"VAW"]) + c(-1, 1) * qt(0.975, df = 453) * 
sd(wr[ ,"VAW"])/sqrt(454))
(CImIWN <- mean(wr[ ,"IWN"]) + c(-1, 1) * qt(0.975, df = 453) *
sd(wr[ ,"IWN"])/sqrt(454))
(CImSPY <- mean(wr[ ,"SPY"]) + c(-1, 1) * qt(0.975, df = 453) *
sd(wr[ ,"SPY"])/sqrt(454))

(CIsAGG <- var(wr[ ,"AGG"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))
(CIsVAW <- var(wr[ ,"VAW"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))
(CIsIWN <- var(wr[ ,"IWN"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))
(CIsSPY <- var(wr[ ,"SPY"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))

tab <- cbind(rbind(CImAGG, CImVAW, CImIWN, CImSPY), 
      rbind(CIsAGG, CIsVAW, CIsIWN, CIsSPY))
rownames(tab) <- c("AGG","VAW","IWN","SPY")
colnames(tab) <- c("lower CI.m","upper CI.m","lower CI.s^2",
"upper CI.s^2")
tab




## h)
##bootstrap confidence intervals

set.seed(3285913)
 
 k <- 10000
 simsamplesAGG<-replicate(k,sample(wr[ ,"AGG"], replace=TRUE))
 simmeansAGG<-apply(simsamplesAGG,2,mean)
 simvarsAGG<-apply(simsamplesAGG,2,var)
 (CImAGGboot <- quantile(simmeansAGG, c(0.025,0.975)))
 (CIsAGGboot <- quantile(simvarsAGG, c(0.025,0.975)))
 
 simsamplesVAW<-replicate(k,sample(wr[ ,"VAW"], replace=TRUE))
 simmeansVAW<-apply(simsamplesVAW,2,mean)
 simvarsVAW<-apply(simsamplesVAW,2,var)
 CImVAWboot <- quantile(simmeansVAW, c(0.025,0.975))
 CIsVAWboot <- quantile(simvarsVAW, c(0.025,0.975))
 
 simsamplesIWN<-replicate(k,sample(wr[ ,"IWN"], replace=TRUE))
 simmeansIWN<-apply(simsamplesIWN,2,mean)
 simvarsIWN <-apply(simsamplesIWN,2,var)
 CImIWNboot <- quantile(simmeansIWN, c(0.025,0.975))
 CIsIWNboot <- quantile(simvarsIWN, c(0.025,0.975))
 
 simsamplesSPY<-replicate(k,sample(wr[ ,"SPY"], replace=TRUE))
 simmeansSPY<-apply(simsamplesSPY,2,mean)
 simvarsSPY<-apply(simsamplesSPY,2,var)
 CImSPYboot <- quantile(simmeansSPY, c(0.025,0.975)) 
 CIsSPYboot <- quantile(simvarsSPY, c(0.025,0.975)) 

tab <- cbind(rbind(CImAGGboot, CImVAWboot, CImIWNboot, CImSPYboot), 
      rbind(CIsAGGboot, CIsVAWboot, CIsIWNboot, CIsSPYboot))
rownames(tab) <- c("AGG","VAW","IWN","SPY")
colnames(tab) <- c("lower CI.m","upper CI.m","lower CI.s^2",
"upper CI.s^2")
tab



## ## i)
## hypothesis testing

(t.obs <- mean(wr[ ,"AGG"]) / (sd(wr[ ,"AGG"])/sqrt(454) ))

2*(1-pt(abs(t.obs),df=453))

t.test(wr[ ,"AGG"])

qt(0.975,df=453)

t <- apply(wr[ ,c("AGG","VAW","IWN","SPY")],2,mean)/
apply(wr[ ,c("AGG","VAW","IWN","SPY")],2,sd)*sqrt(453)
df <- rep(453,4)
p <- 2*(1-pt(abs(t),df=453))
tab <- cbind(t,df,p)
rownames(tab) <- c("AGG","VAW","IWN","SPY")
colnames(tab) <- c("t.obs","df","P(T>|t.obs|)")
tab

 
 
 
  
## ## j)
## Import data finans2_data.csv
etfSum <- read.table("finans2_data.csv",header=TRUE, sep=";")
str(etfSum)

t.test(wr[ ,"AGG"],wr[ ,"VAW"])









## ## k)
##Determine the empirical correlation for the selected variables

cor(etfSum_analyse[,2:7], use="everything", method="pearson")

par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
par(mfrow=c(1,1))
plot(etfSum_analyse$Volatility, etfSum_analyse$CVaR, pch=16, cex=0.7,
xlab="Volatility [Weekly Pct.]",
ylab="Conditional Value at Risk [Weekly Pct.]",  cex.lab=0.8,
main="Relation between Volatility and CVaR", cex.main=0.8)

 
##For calculations of the correlation between Geo.mean and maxTuW

cov(etfSum_analyse$Geo.mean, etfSum_analyse$maxTuW)
var(etfSum_analyse$Geo.mean)
var(etfSum_analyse$maxTuW)


etfSum_analyse <- read.table("finans2_data.csv",header=TRUE, sep=";")
attach(etfSum_analyse)
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
par(mfrow=c(2,2))
plot(Volatility, CVaR, pch=16, cex=0.7,cex.main=0.8)
plot(Geo.mean, maxTuW, pch=16, cex=0.7,cex.main=0.8)
plot(Volatility, maxDD, pch=16, cex=0.7,cex.main=0.8)
plot(maxTuW, Volatility, pch=16, cex=0.7,cex.main=0.8)

round(cor(etfSum_analyse[,-1]),digits=2)

c(cov(etfSum_analyse[,"Geo.mean"],etfSum_analyse[,"maxTuW"]), 
sd(etfSum_analyse[,"Geo.mean"]), sd(etfSum_analyse[,"maxTuW"]))



