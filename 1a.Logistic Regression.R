library(dplyr)
library(glmnet)
library(caret)
library(ggplot2)
library(scatterplot3d)

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

set.seed(1)
train = sample_frac(cust_data_table, 0.7)
test = setdiff(cust_data_table,train)

NROW(train)
NROW(test)

predicators_train = model.matrix(train$Reached.on.Time_Y.N~.,train)[,-train$Reached.on.Time_Y.N] ## IV
dependent_train = train$Reached.on.Time_Y.N

predicators_test = model.matrix(test$Reached.on.Time_Y.N~.,test)[,-test$Reached.on.Time_Y.N] ## IV
dependent_test = test$Reached.on.Time_Y.N

typeof(predicators_test)
typeof(dependent_test)

#Ridge Regression alpha 1 = lasso 0=Ridge
ridge_mod=cv.glmnet(predicators_train, dependent_train,alpha=0)
ridge_mod$lambda.1se
ridge_mod$lambda.min

coef(ridge_mod,s=ridge_mod$lambda.1se)
sqrt(sum(coef(ridge_mod,s=ridge_mod$lambda.1se)^2))

ridge_mod_predict = predict(ridge_mod,newx = predicators_test,s=ridge_mod$lambda.1se,type="response")
ridge_mod_predict
ridgepred = ifelse(ridge_mod_predict>0.5,1,0)
cm_ridge = confusionMatrix(as.factor(dependent_test),as.factor(ridgepred))
cm_ridge

# 1 indicates product NOT reached on Time. 0 indicates reached on time.

ggplot(cust_data_table,aes(x=Discount_offered,y=Customer_rating,pch=as.factor(Reached.on.Time_Y.N),color = Warehouse_block)) + geom_point()

ggplot(cust_data_table,aes(x=Discount_offered,y=Warehouse_block,pch=as.factor(Reached.on.Time_Y.N),color = Customer_care_calls)) + geom_point()

scatterplot3d::scatterplot3d(x=test$Discount_offered,y=test$Prior_purchases,z=test$Weight_in_gms,color=test$Reached.on.Time_Y.N+3)

# predict with test dataset

testdata=read.csv("Term end Project.csv")
test_table = tbl_df(testdata)
test_table = test_table[,-1]
test_table$Reached.on.Time_Y.N[] = factor("True")
typeof(test_table$Reached.on.Time_Y.N)
summary(test_table)

predicators_testdata = model.matrix(test_table$Reached.on.Time_Y.N~.,test_table)[,-test_table$Reached.on.Time_Y.N] ## IV
#dependent_testdata = test_table$Reached.on.Time_Y.N

ridge_mod_predict1 = predict(ridge_mod,newx = predicators_testdata,s=ridge_mod$lambda.1se,type="response")
ridge_mod_predict1
ridgepred1 = ifelse(ridge_mod_predict1>0.5,1,0)
table(ridgepred1)
