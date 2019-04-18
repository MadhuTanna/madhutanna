reliance = read.csv("C:/Users/mtanna108360/Downloads/Data Science/Proschool/Temporary R/reliance_NSE.csv")
#reliance = read.csv("C:/Users/mtanna108360/Downloads/Data Science/Proschool/Temporary R/500325.csv")

reliance = reliance[sort(reliance$Date,decreasing = TRUE),]
library(forecast)
library(caret)

set.seed(1)
rowids = sample(1:NROW(reliance),NROW(reliance)*0.70,replace = F)
relianceTrain = reliance[rowids,]
relianceTest= reliance[-rowids,]

# Take high price and plot

mydatahigh = relianceTrain$High.Price
plot(mydatahigh,main="Reliance High Prices",type="l")

#differencing original high prices
diffhigh = diff(mydatahigh)
plot(diffhigh,main="Reliance High Prices",type="l")

# log of original series and plot high prices
loghigh = log(mydatahigh)
plot(loghigh,main="Reliance High Prices",type="l")

# differencing logged price and plot
diffloghigh = diff(loghigh)
plot(diffloghigh,main="Reliance High Prices",type="l")


# ACF and PACF of logged price
acflog = acf(loghigh, main = "ACF Reliance Logged High Prices", lag.max = 100)
pacflog = pacf(loghigh, main = "PACF Reliance Logged High Prices", lag.max = 100)

# ACF and PACF of differencing log price
acfdifflog = acf(diffloghigh, main = "ACF Reliance Differenced Logged High Prices", lag.max = 100)
pacfdifflog = pacf(diffloghigh, main = "PACF Reliance Differenced Logged High Prices", lag.max = 100)

# build model with different p,q and check lowest AICs.

aicmat = matrix(nrow = 27,ncol = 2)
colnames(aicmat) = c("Model","AIC")
aicmat

i=1

  for (j in 0:2)
  {
    for (k in 0:2)
    {
      for (l in 0:2) 
        {
          vmodel = paste(j,k,l)
          aicmat[i,1] = vmodel
          myarima = arima(loghigh, order = c(j,k,l))
          aicmat[i,2] = myarima$aic
          i=i+1
         }
    }
  }

aicmat

aicmat1 = matrix(nrow = 27,ncol = 2)
colnames(aicmat1) = c("Model","AIC")
aicmat1

i=1

for (j in 0:2)
{
  for (k in 0:2)
  {
    for (l in 0:2) 
    {
      vmodel = paste(j,k,l)
      aicmat1[i,1] = vmodel
      myarima1 = arima(diffloghigh, order = c(j,k,l))
      aicmat1[i,2] = myarima1$aic
      i=i+1
    }
  }
}

aicmat1
aicmat

finalarima = arima(mydatahigh,order=c(1,2,1))
#auto.arima(ts(mydatahigh,frequency = 5),max.p = 5,max.q=5,max.D=5)
finalarima

plot(finalarima$residuals,main="Residuals of ARIMA Loghigh")

acfresidual = acf(finalarima$residuals,main="ACF Residual of Loghigh",lag.max = 100)
pacfresidual = pacf(finalarima$residuals,main="PACF Residual of Loghigh",lag.max = 100)

hist(finalarima$residuals)
Box.test(finalarima$residuals,lag=100, type="Ljung-Box")

pred=predict(finalarima,n.ahead=2,level=99)
pred$pred

squredresi = finalarima$residuals^2
plot(squredresi,type="l")

acfsqresidual = acf(squredresi,main="ACF Squared Residual of Loghigh",lag.max = 100)
pacfsqresidual = pacf(squredresi,main="PACF Squared Residual of Loghigh",lag.max = 100)


library(tseries)

archaic = matrix(15)

for (i in 1:15)
{
archmodel = garch(ts(finalarima$residuals),order=c(1,1),trace=F)
archaic[i] = AIC(archmodel)
}
archaic

spyGarch = garchFit(~arma(0,0,0) + garch(1, 1), data=as.ts(mydatahigh))
predict(spyGarch, n.ahead=1, doplot=F)


summary(archmodel)


htarch = archmodel$fitted.values[,1]^2
plot(htarch, type="l", main="Conditional Variance")


fitted = fitted.values(finalarima)
highside = fitted+1.96*sqrt(htarch)
lowside = fitted-1.96*sqrt(htarch)


plot(diffloghigh,type="l",main = "Actual,High and Low Fit")
par(new = T)
plot(highside,col="red")
par(new = T)
plot(lowside,col="green")

forecastRel = forecast(finalarima,h=1,level = 95)
forecastRel


ht=(archmodel$residuals^2)*(archmodel$coef)


forecast1=(archmodel)
forecast1




finalaic = rbind(aicmat,aicmat1)
finalaic

finalorder = finalaic[finalaic[,2]==min(finalaic[,2]),1]
finalorder

finalarima = arima()

mydata1high = ts(mydatahigh,frequency = 5)
plot(decompose(mydata1high))

ts.plot(mydata1high)
acf(mydata1high)
pacf(mydata1high)
diffdata = diff(mydata1high)
plot(diffdata)
acf(diffdata,lag.max = 10)
pacf(diffdata,lag.max = 10)
#diffdata2=diff(diffdata)
#plot(diffdata2)

myARIMAhigh = arima(mydata1high,order = c(p=1,d=1,q=4),xreg = relianceTrain[,c(4,5,7)])
#myARIMAhigh = arima(mydata1high,order = c(p=1,d=1,q=4),xreg = relianceTrain[,c(5,7,10,15)])
myARIMAhigh
summary(myARIMAhigh)
hist(myARIMAhigh$residuals)
acf(myARIMAhigh$residuals)

# Test for high price based on open,low
HighPriceTest1 = forecast(myARIMAhigh,xreg = relianceTest[,c(4,5,7)])
#HighPriceTest1 = forecast(myARIMAhigh,xreg = relianceTest[,c(5,7,10,15)])
#HighPriceTest1
tbHighPriceTest1=as.data.frame(HighPriceTest1)
predictedHigh1 = tbHighPriceTest1$`Point Forecast`
TestHigh1 = relianceTest$High.Price
diff = predictedHigh1-TestHigh1
logdiff=ifelse(abs(diff)>2,0,1)
table(logdiff)

# Low Price based on Open,High, WAP and Volume
mydatalow = relianceTrain$Low.Price
summary(mydatalow)
mydata1low = ts(mydatalow,frequency = 5)

ts.plot(mydata1low)
acf(mydata1low)
pacf(mydata1low)
diffdata = diff(mydata1low)
plot(diffdata)
acf(diffdata,lag.max = 20)
pacf(diffdata,lag.max = 20)
#diffdata2=diff(diffdata)
#plot(diffdata2)

myARIMAlow = arima(mydata1low,order = c(p=0,d=0,q=0),xreg = relianceTrain[,c(4,5,6)])

#myARIMAlow = arima(mydata1low,order = c(p=0,d=0,q=0),xreg = relianceTrain[,c(5,6,10,15)])
myARIMAlow
summary(myARIMAlow)
hist(myARIMAlow$residuals)
acf(myARIMAlow$residuals)

# Test on Low price based on High and open
LowPriceTest1 = forecast(myARIMAlow,xreg = relianceTest[,c(4,5,6)])
#LowPriceTest1 = forecast(myARIMAlow,xreg = relianceTest[,c(5,6,10,15)])

#LowPriceTest1
tbLowPriceTest1=as.data.frame(LowPriceTest1)
predictedLow1 = tbLowPriceTest1$`Point Forecast`
TestLow1 = relianceTest$Low.Price
diff = predictedLow1-TestLow1
logdiff=ifelse(abs(diff)>2,0,1)
table(logdiff)

# Close Price based on Open Price, High, Low, WAP and Volume
mydataclose = relianceTrain$Close.Price
mydata1close = ts(mydataclose,frequency = 5)

ts.plot(mydata1close)
acf(mydata1close)
pacf(mydata1close)
diffdata = diff(mydata1close)
plot(diffdata)
acf(diffdata,lag.max = 20)
pacf(diffdata,lag.max = 20)
#diffdata2=diff(diffdata)
#plot(diffdata2)

myARIMAclose = arima(mydata1close,order = c(p=1,d=1,q=3),xreg = relianceTrain[,c(5,6,7)])
#myARIMAclose = arima(mydata1close,order = c(p=1,d=1,q=3),xreg = relianceTrain[,c(5,6,7,10)])
myARIMAclose
summary(myARIMAclose)
hist(myARIMAclose$residuals)
acf(myARIMAclose$residuals)

# use test data
#closePriceTest = forecast(myARIMAclose,xreg = relianceTest[,c(5,6,7,10)])
closePriceTest = forecast(myARIMAclose,xreg = relianceTest[,c(5,6,7)])


#closePriceTest
tbclosePriceTest=as.data.frame(closePriceTest)
predictedclose = tbclosePriceTest$`Point Forecast`
Testclose = relianceTest$Close.Price
diff = predictedclose-Testclose
logdiff=ifelse(abs(diff)>2,0,1)
table(logdiff)

library("forecast")

x=data.frame('RELIANCE','EQ','23-Apr-18',927.9,930,944.35,928.5,935,936,936.28,3508515,3284954149,74884,1921427,54.76)

mypredOpen = forecast(myARIMAopen,xreg = x[,c(4,10,15)],level=c(10,20,30,40,50,60,70,80,90,99))
mypredOpen
predOpen = as.data.frame(mypredOpen)
predOpen$`Point Forecast`

#mypredHigh1 = forecast(myARIMAhigh,xreg = x[,c(5,7,10,15)],level=c(10,20,30,40,50,60,70,80,90,99))

mypredHigh1 = forecast(myARIMAhigh,xreg = x[,c(4,5,7)],level=c(10,20,30,40,50,60,70,80,90,99))

mypredHigh1
PredHigh1 = as.data.frame(mypredHigh1)
PredHigh1$`Point Forecast`

#mypredLow1 = forecast(myARIMAlow,xreg = x[,c(5,6,10,15)],level=c(10,20,30,40,50,60,70,80,90,99))

mypredLow1 = forecast(myARIMAlow,xreg = x[,c(4,5,6)],level=c(10,20,30,40,50,60,70,80,90,99))

mypredLow1
PredLow1 = as.data.frame(mypredLow1)
PredLow1$`Point Forecast`

#mypredClose = forecast(myARIMAclose,xreg = x[,c(5,6,7,10)],level=c(10,20,30,40,50,60,70,80,90,99))
mypredClose = forecast(myARIMAclose,xreg = x[,c(5,6,7)],level=c(10,20,30,40,50,60,70,80,90,99))


mypredClose
PredClose = as.data.frame(mypredClose)
PredClose$`Point Forecast`


predOpen
PredHigh1
PredLow1
PredClose
