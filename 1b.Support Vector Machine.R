library(dplyr)
library(glmnet)
library(caret)
library(rminer)

#setwd("C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Term 3 Recordings/Project")
setwd("C:/Users/MADHU/Downloads/Data Science/Term 3 Project")
custdata=read.csv("term3.csv")

head(custdata)
glimpse(custdata) ## like str
summary(custdata)

cust_data_table = tbl_df(custdata)
cust_data_table = na.omit(cust_data_table)
cust_data_table = cust_data_table[,-1]
NROW(cust_data_table)
str(cust_data_table)

#Below plot shows mostly linear relationship between variables
#plot(cust_data_table[,-cust_data_table$Reached.on.Time_Y.N],col=c("red","green")[as.numeric(cust_data_table$Reached.on.Time_Y.N)], pch = 20, cex = .2)

set.seed(1)
train = sample_frac(cust_data_table, 0.7)
test = setdiff(cust_data_table,train)

predicators_train = model.matrix(train$Reached.on.Time_Y.N~.,train)[,-train$Reached.on.Time_Y.N] ## IV
dependent_train = train$Reached.on.Time_Y.N

predicators_test = model.matrix(test$Reached.on.Time_Y.N~.,test)[,-test$Reached.on.Time_Y.N] ## IV
dependent_test = test$Reached.on.Time_Y.N

#SVM
library(e1071)

dat_new = data.frame(predicators_train, dependent_train = as.factor(dependent_train))
head(predicators_test)

svm_mod = fit(dependent_train ~ .,data=dat_new, model = "svm",fdebug = T,search = list(scale = T,kernel = 'linear',cost=0.1,type="C-classification"))
i=Importance(svm_mod,data = dat_new)

#Below plot shows according to SVM, two most important variables are 14,15 i.e. Discount offered and weight in gram
plot(i$imp,xlab = "Variable ID",ylab="Importance")
i$sresponses[[14]][1]
i$sresponses[[15]][1]

#Below plot shows impact of two most important fields on class, in train dataset
plot(predicators_train[,c(14,15)], col = dependent_train + 3, pch = 19)

#Prepare Grid for SVM visualization
n=100
grange = apply(predicators_train,2, range)
Discount_offered =  seq(from = grange[1,14], to = grange[2,14], length = n)
Weight_in_gms =  seq(from = grange[1,15], to = grange[2,15], length = n)

xgrid = expand.grid(Discount_offered,Weight_in_gms)
NROW(xgrid)
head(xgrid)

n=NROW(xgrid)
Warehouse_blockB = seq(from = grange[1,1], to = grange[2,1], length = n)
Warehouse_blockC = seq(from = grange[1,2], to = grange[2,2], length = n)
Warehouse_blockD =  seq(from = grange[1,3], to = grange[2,3], length = n)
Warehouse_blockF = seq(from = grange[1,4], to = grange[2,4], length = n)
Mode_of_ShipmentRoad =  seq(from = grange[1,5], to = grange[2,5], length = n)
Mode_of_ShipmentShip =  seq(from = grange[1,6], to = grange[2,6], length = n)
Product_importancelow =  seq(from = grange[1,11], to = grange[2,11], length = n)
Product_importancemedium =  seq(from = grange[1,12], to = grange[2,12], length = n)
GenderM =  seq(from = grange[1,13], to = grange[2,13], length = n)
Customer_care_calls =  seq(from = grange[1,7], to = grange[2,7], length = n)
Customer_rating = seq(from = grange[1,8], to = grange[2,8], length = n)
Cost_of_the_Product =  seq(from = grange[1,9], to = grange[2,9], length = n)
Prior_purchases =  seq(from = grange[1,10], to = grange[2,10], length = n)

xgrid = cbind(xgrid,Warehouse_blockB,Warehouse_blockC, Warehouse_blockD, Warehouse_blockF, Mode_of_ShipmentRoad,Mode_of_ShipmentShip, Customer_care_calls, Customer_rating, Cost_of_the_Product, Prior_purchases,Product_importancelow, Product_importancemedium, GenderM)
head(xgrid)

names(xgrid)[1] = "Discount_offered"
names(xgrid)[2] = "Weight_in_gms"

#prediction for values in grid, to visualize svm
svm_mod = svm(dependent_train ~ .,data=dat_new, scale = T,kernel = 'linear',cost=0.1)
ygrid = predict(svm_mod,xgrid)
predtrain = predict(svm_mod,predicators_train)

#Below plot shows SVM for values in grid.Points in Train data set is predicted and highlighted, considering two most important colomns in 2D
plot(xgrid[,c(1,2)],col = c("red","green")[as.numeric(ygrid)], pch = 20, cex = .2)
points(predicators_train[,c(14,15)], col = c("red","green")[as.numeric(predtrain)], pch = 18)
#Below shows the support vector
points(predicators_train[svm_mod$index,c(14,15)], pch = 20, cex = .2,col="yellow")

func = predict(svm_mod,xgrid,decision.values = T)
#func = predict(svm_mod,predicators_train,decision.values = T)
func=attributes(func)$decision

x=seq(from = min(predicators_train[,14]), to = max(predicators_train[,14]), length = 100)
y=seq(from = min(predicators_train[,15]), to = max(predicators_train[,15]), length = 100)

#x=seq(from = min(xgrid[,1]), to = max(xgrid[,1]), length = 3)
#y=seq(from = min(xgrid[,2]), to = max(xgrid[,2]), length = 3)

contour(x,y,z=matrix(func,length(x),length(y)), add = TRUE)

dat_new_pred = data.frame(predicators_test, dependent_test = as.factor(dependent_test))
svm_pred = predict(svm_mod, newdata = dat_new_pred)

cm_svm = confusionMatrix(as.factor(dependent_test),svm_pred)
cm_svm

beta = drop(t(svm_mod$coefs)%*%(predicators_train[svm_mod$index,]))
beta0 = svm_mod$rho
# two most big coefficients are Discount offered (positive) and weight in gms (-negative)
beta=sort(beta)

abline(beta0/beta[15], -beta[1]/beta[15])
abline((beta0 - 1) / beta[15], -beta[1] / beta[15], lty = 2)
abline((beta0 + 1) / beta[15], -beta[1] / beta[15], lty = 2)

# predict with test dataset

testdata=read.csv("Term end Project.csv")
test_table = tbl_df(testdata)
test_table = test_table[,-1]
test_table$Reached.on.Time_Y.N[] = factor("True")

predicators_testdata = model.matrix(~.,test_table)
svm_pred1 = predict(svm_mod, newdata = as.data.frame(predicators_testdata))

table(svm_pred1)

