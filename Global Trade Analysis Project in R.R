# Forecasting future trade in USD, QTY for each commodities/categories/countries using SMA technique

library(xlsx)
library(xlsxjars)
library(readxl)
library(forecast)
library(caret)
library(tseries)
library(TTR)

TradeData = read_excel("C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Tableau/Dataset_Int_business.xlsx",sheet = "Sheet1")
head(TradeData)

TradeDataTot = data.frame()
dftot = data.frame()

for (varcountry in unique(TradeData$country_or_area)) 
  {
    paramcountry = varcountry
    for(varcategory in unique(TradeData$category)) 
      {
        paramcategory = varcategory
        for(varcommodity in unique(TradeData$commodity))
          {
            paramcommodity = varcommodity
            
            TradeData1 = TradeData[TradeData$country_or_area == paramcountry & TradeData$category == paramcategory & TradeData$commodity == paramcommodity,]
            
            if (NROW(TradeData1)>0) 
              {
                TradeUSDBlank = TradeData1[TradeData1$trade_usd == '',]
                NROW(TradeUSDBlank)
            
                TradeQTYBlank = TradeData1[TradeData1$quantity == '' | TradeData1$quantity == 0,]
                NROW(TradeQTYBlank)
            
                SUMUSD = sum(TradeData1$trade_usd[TradeData1$quantity != '' | TradeData1$quantity == 0],na.rm = T)
                SUMQTY = sum(TradeData1$quantity[TradeData1$quantity != '' | TradeData1$quantity == 0],na.rm = T)
                UnitPriceAvg = SUMUSD/SUMQTY
            
                TradeData1$quantity[is.na(TradeData1$quantity == T) | TradeData1$quantity == 0] = round(TradeData1$trade_usd[is.na(TradeData1$quantity == T) | TradeData1$quantity == 0]/UnitPriceAvg,digit=2)
                TradeData1$quantity
                TradeQTYBlank = TradeData1[TradeData1$quantity == '',]
                nrow(TradeQTYBlank)
            
                TradeDataUSD = aggregate(TradeData1$trade_usd, by = list(Cateogry = TradeData1$year), FUN = sum)
                mytsUSD = ts(TradeDataUSD$x,frequency = 1)
            
                TradeDataQTY = aggregate(TradeData1$quantity, by = list(Cateogry = TradeData1$year), FUN = sum)
                mytsQTY = ts(TradeDataQTY$x,frequency = 1)
            
                TradeSMAUSD = SMA(mytsUSD,n=3)
                SMAForecastUSD = forecast(TradeSMAUSD,h=3)
            
                TradeSMAQTY = SMA(mytsQTY,n=3)
                SMAForecastQTY = forecast(TradeSMAQTY,h=3)
            
                df2 = as.matrix(data.frame(unique(TradeData1$country_or_area),unique(TradeData1$category),unique(TradeData1$commodity),round(SMAForecastUSD$mean,digits = 2),round(SMAForecastQTY$mean,digits = 2)))
                dftot = rbind(dftot,df2) 
                
              }
            
            TradeDataTot = rbind(TradeDataTot,TradeData1)
            
            }
        }
  }

write.csv(dftot,file = "C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Tableau/dftot.csv")
write.csv(TradeDataTot,file = "C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Tableau/TradeDataTot.csv")