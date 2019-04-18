# Check High Price with Holtswinter
# Check high price with only ARIMA
# Check high price with ARIMA-GARCH : Price+Volatility

library(forecast)
library(caret)
library(tseries)
library(rugarch)
library(tseries)
library(fGarch)
library(xlsx)
library(xlsxjars)

fname = "SAIL_NSE.CSV"
fpath1 = "C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Temporary R/dailyfiles/"
fpath = paste0(fpath1,fname)
reliance = read.csv(fpath)
reliance = reliance[sort(reliance$Date,decreasing = FALSE),]
head(reliance)

# Holtswinter

Holtts = ts(reliance$Open.Price,frequency = 5)
Holtsfit = HoltWinters(Holtts)
holtsprediopen = forecast(Holtsfit,h=1)


Holtts = ts(reliance$High.Price,frequency = 5)
Holtsfit = HoltWinters(Holtts)
holtspredihigh = forecast(Holtsfit,h=1)

Holtts = ts(reliance$Low.Price,frequency = 5)
Holtsfit = HoltWinters(Holtts)
holtspredilow = forecast(Holtsfit,h=1)

Holtts = ts(reliance$Close.Price,frequency = 5)
Holtsfit = HoltWinters(Holtts)
holtsprediclose = forecast(Holtsfit,h=1)

Holtts = ts(reliance$Average.Price,frequency = 5)
Holtsfit = HoltWinters(Holtts)
holtsprediavg = forecast(Holtsfit,h=1)

holtsprediopen
holtspredihigh
holtspredilow
holtsprediclose
holtsprediavg

wb = loadWorkbook(paste0(fpath1,"All.xlsx"))
sheet1 = getSheets(wb)[[1]]
nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"),fname,holtsprediopen$method,round(as.numeric(holtsprediopen$mean),2),round(as.numeric(holtspredihigh$mean),2),round(as.numeric(holtspredilow$mean),2),round(as.numeric(holtsprediclose$mean),2),round(as.numeric(holtsprediavg$mean),2))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
#saveWorkbook(wb,file = paste0(fpath1, "All.xlsx"))

# Auto ARIMA

autoarimafit = auto.arima(reliance$Open.Price,max.p = 5,max.q = 5,max.d = 5)
#,xreg = reliance[,c(4,10,11,15)])
autoarimafit
autoarimapredopen = forecast(autoarimafit,h=1)
#,xreg=reliance[NROW(reliance)-1,c(4,10,11,15)])
autoarimafit = auto.arima(reliance$High.Price,max.p = 5,max.q = 5,max.d = 5)
autoarimafit
autoarimapredhigh = forecast(autoarimafit,h=1)

autoarimafit = auto.arima(reliance$Low.Price,max.p = 5,max.q = 5,max.d = 5)
autoarimafit
autoarimapredlow = forecast(autoarimafit,h=1)

autoarimafit = auto.arima(reliance$Close.Price,max.p = 5,max.q = 5,max.d = 5)
autoarimafit
autoarimapredclose = forecast(autoarimafit,h=1)

autoarimafit = auto.arima(reliance$Average.Price,max.p = 5,max.q = 5,max.d = 5)
autoarimafit
autoarimapredavg = forecast(autoarimafit,h=1)

autoarimapredopen
autoarimapredhigh
autoarimapredlow
autoarimapredclose
autoarimapredavg

nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"),fname,autoarimapredopen$method,round(as.numeric(autoarimapredopen$mean),2),round(as.numeric(autoarimapredhigh$mean),2),round(as.numeric(autoarimapredlow$mean),2),round(as.numeric(autoarimapredclose$mean),2),round(as.numeric(autoarimapredavg$mean),2))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
#saveWorkbook(wb,file = paste0(fpath1,"All.xlsx"))

# ARIMA-GARCH

gopen = paste0("~arma",substr(autoarimapredopen$method,6,12)," + garch(1,1)")
arimagarchts = ts(reliance$Open.Price,frequency = 5)
arimagarchfit = garchFit(as.formula(gopen), data=as.ts(arimagarchts))
arimagarchpredopen = predict(arimagarchfit, n.ahead=1, doplot=F)

ghigh = paste0("~arma",substr(autoarimapredhigh$method,6,12)," + garch(1,1)")
arimagarchts = ts(reliance$High.Price,frequency = 5)
arimagarchfit = garchFit(as.formula(ghigh), data=as.ts(arimagarchts))
arimagarchpredhigh = predict(arimagarchfit, n.ahead=1, doplot=F)

glow = paste0("~arma",substr(autoarimapredlow$method,6,12)," + garch(1,1)")
arimagarchts = ts(reliance$Low.Price,frequency = 5)
arimagarchfit = garchFit(as.formula(glow), data=as.ts(arimagarchts))
arimagarchpredlow = predict(arimagarchfit, n.ahead=1, doplot=F)

gclose = paste0("~arma",substr(autoarimapredclose$method,6,12)," + garch(1,1)")
arimagarchts = ts(reliance$Close.Price,frequency = 5)
arimagarchfit = garchFit(as.formula(gclose), data=as.ts(arimagarchts))
arimagarchpredclose = predict(arimagarchfit, n.ahead=1, doplot=F)

arimagarchts = ts(reliance$Average.Price,frequency = 5)
diffarimagarch = diff(arimagarchts)
arimagarchfit = garchFit(~arma(0,1,0) + garch(1, 1), data=as.ts(arimagarchts))
arimagarchpredavg = predict(arimagarchfit, n.ahead=1, doplot=F)

nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"),fname,"Garch-Mean",round(arimagarchpredopen$meanForecast,2),round(arimagarchpredhigh$meanForecast,2),round(arimagarchpredlow$meanForecast,2),round(arimagarchpredclose$meanForecast,2),round(arimagarchpredavg$meanForecast,2))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
#saveWorkbook(wb,file = paste0(fpath1,"All.xlsx"))

nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"),fname,"Garch-SD",round(arimagarchpredopen$standardDeviation,2),round(arimagarchpredhigh$standardDeviation,2),round(arimagarchpredlow$standardDeviation,2),round(arimagarchpredclose$standardDeviation,2),round(arimagarchpredavg$standardDeviation,2))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
#saveWorkbook(wb,file = paste0(fpath1,"All.xlsx"))

nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
#saveWorkbook(wb,file = paste0(fpath1,"All.xlsx"))

nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
#saveWorkbook(wb,file = paste0(fpath1,"All.xlsx"))

nr = sheet1$getLastRowNum()
df = data.frame(format(as.Date(Sys.Date())+1,"%d-%b-%y"))
addDataFrame(df,sheet = sheet1,startRow = nr+2,startColumn = 1,row.names=FALSE,col.names=FALSE)
saveWorkbook(wb,file = paste0(fpath1,"All.xlsx"))