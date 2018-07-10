realestate = read.csv("C:/Users/mtanna108360/Downloads/Data Science/Proschool/Term 1 Project/HousingData.csv")
nrow(realestate)
# 506
head(realestate)
summary(realestate)

hist(realestate$MEDV)

tempData = realestate[complete.cases(realestate),]
nrow(tempData)
#394

# sort is for a vector. order is for data frames i.e. more than one variables. If we use sort here is showing wrong result
orderData = realestate[order(realestate$MEDV,decreasing = TRUE),]
head(orderData)

# Remove outliers for MEDV
ulimitMEDV = mean(realestate$MEDV,na.rm = T) + 3*sd(realestate$MEDV,na.rm = T)
ulimitMEDV

realestate$MEDV[realestate$MEDV>ulimitMEDV & is.na(realestate$MEDV) == F]
NROW(realestate$MEDV[realestate$MEDV>ulimitMEDV & is.na(realestate$MEDV) == F])

realestate$MEDV = powerTransform(realestate$MEDV)
hist(realestate$MEDV)


# Remove Outliers for CRIM Rate
ulimitCRIM = mean(realestate$CRIM,na.rm = T) + 3*sd(realestate$CRIM,na.rm = T)
ulimitCRIM

realestate$CRIM[realestate$CRIM>ulimitCRIM & is.na(realestate$CRIM) == F]
NROW(realestate$CRIM[realestate$CRIM>ulimitCRIM & is.na(realestate$CRIM) == F])
realestate$CRIM[realestate$CRIM>ulimitCRIM & is.na(realestate$CRIM) == F] = ulimitCRIM

# Treat CRIM Rate = NA. Replace with Average Crime Rate
sum(is.na(realestate$CRIM)==T)
#20
realestate$CRIM[is.na(realestate$CRIM)==T]=mean(realestate$CRIM,na.rm = T)

# Treating INDUS = "NA". Replace it with help of NOX field.

cor(realestate$INDUS[is.na(realestate$INDUS)==F],realestate$NOX[is.na(realestate$INDUS)==F])
ccf(realestate$INDUS[is.na(realestate$INDUS)==F],realestate$NOX[is.na(realestate$INDUS)==F],type = "correlation",plot=TRUE)
cor.test(realestate$INDUS[is.na(realestate$INDUS)==F],realestate$NOX[is.na(realestate$INDUS)==F],method = "kendall")

rows=realestate[is.na(realestate$INDUS)==T,]
rowids = row.names(rows)

sum(is.na(realestate$INDUS)==T)
# 20

LMR=lm(INDUS~NOX,data=data.frame(INDUS=realestate$INDUS,NOX=realestate$NOX))
LMR
summary(LMR)

realestate$INDUS[is.na(realestate$INDUS) == T] =  predict(LMR,newdata = data.frame(INDUS = realestate$INDUS[is.na(realestate$INDUS) == T],NOX=realestate$NOX[is.na(realestate$INDUS) == T]))
realestate[rowids,]

# Treat CHAS = "NA". Replace with mode
table(realestate$CHAS)
sum(is.na(realestate$CHAS)==T)
#20
realestate$CHAS[is.na(realestate$CHAS) == T] = 0
realestate$CHAS = as.factor(realestate$CHAS)

# Treat ZN = "NA" Replace with help of INDUS
cor(realestate$ZN[is.na(realestate$ZN)==F],realestate$INDUS[is.na(realestate$ZN)==F])
ccf(realestate$ZN[is.na(realestate$ZN)==F],realestate$INDUS[is.na(realestate$ZN)==F],type = "correlation",plot=TRUE)
cor.test(realestate$ZN[is.na(realestate$ZN)==F],realestate$INDUS[is.na(realestate$ZN)==F],method = "kendall")

rows=realestate[is.na(realestate$ZN)==T,]
rowids = row.names(rows)

sum(is.na(realestate$ZN)==T)
# 20

LMR=lm(ZN~INDUS,data=data.frame(ZN=realestate$ZN,INDUS=realestate$INDUS))
LMR
summary(LMR)

realestate$ZN[is.na(realestate$ZN) == T] = predict(LMR,newdata = data.frame(ZN = realestate$ZN[is.na(realestate$ZN) == T],INDUS=realestate$INDUS[is.na(realestate$ZN) == T]))
realestate[rowids,]

# replace predicated value if - ve to zero
NROW(realestate[realestate$ZN < 0,])
realestate$ZN[realestate$ZN < 0] = 0
summary(realestate)

# Treat AGE. Replace NA with Median
sum(is.na(realestate$AGE)==T)
#20
realestate$AGE[is.na(realestate$AGE)==T]=median(realestate$AGE,na.rm = T)

# Treat LSTAT. Replace NA with Median
sum(is.na(realestate$LSTAT)==T)
#20
realestate$LSTAT[is.na(realestate$LSTAT)==T]=median(realestate$LSTAT,na.rm = T)

summary(realestate)
str(realestate)

# Divide data in Train and Test
set.seed(4)
rowids = sample(1:nrow(realestate), 0.75*nrow(realestate))
DataModel=realestate[rowids,]
test =realestate[-rowids,]

#Model
LRM = lm(MEDV ~ ., data = DataModel)
summary(LRM)

# check multicolinearity in a different way than VIF

newLRM = step(LRM)
formula(newLRM)
summary(newLRM)

# To remove INDUS and AGE
LRM1 = update(LRM,~.-AGE, data = DataModel)
summary(LRM1)

LRM2 = update(LRM1,~.-AGE-INDUS, data = DataModel)
summary(LRM2)

#Check for Multicolinearity Keep for VIF<5. H0=IV has no effect on DV
library(car)
checkmulti=vif(LRM2)
checkmulti

# # To remove TAX
LRM3 = update(LRM2, ~ .-INDUS-AGE-TAX, data = DataModel)
summary(LRM3)

#assumption check
#1-Autocorrelation
durbinWatsonTest(LRM3) #Close to 2 is good
?durbinWatsonTest

#2 - Normality of errors with mean 0
hist(residuals(LRM3))

#3 - Homoskedasticity 
plot(fitted(LRM3),residuals(LRM3))

rmse_model = sqrt(mean(residuals(LRM3)^2))
rmse_model

# Using Test data
LRMTest = lm(MEDV~.-AGE-INDUS-TAX, data = test)
summary(LRMTest)
  
rmse_test = sqrt(mean(residuals(LRMTest)^2))
rmse_test

PredPrice = predict(LRM3,newdata = test)
PredPrice

NROW(PredPrice)
NROW(test$MEDV)

Outcome = cbind.data.frame(Actual=test$MEDV, PredPrice=PredPrice, Error = (test$MEDV-PredPrice))

plot(Outcome$Actual,Outcome$Pred)

rmse_test_predict = sqrt(mean(Outcome$Error^2))
rmse_test_predict