library(dplyr)
library(glmnet)
library(caret)

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

str(cust_data_table)

NROW(train)
NROW(test)

predicators_train = model.matrix(train$Reached.on.Time_Y.N~.,train)[,-train$Reached.on.Time_Y.N] ## IV
dependent_train = train$Reached.on.Time_Y.N

predicators_test = model.matrix(test$Reached.on.Time_Y.N~.,test)[,-test$Reached.on.Time_Y.N] ## IV
dependent_test = test$Reached.on.Time_Y.N

#RF
library(randomForest)
modelrf <- randomForest(Reached.on.Time_Y.N ~ . , data = train, do.trace=T)
modelrf

importance(modelrf)
varImpPlot(modelrf)

predrf_test <- predict(modelrf, newdata = test)
predrf = ifelse(predrf_test>0.5,1,0)
cm_rf = confusionMatrix(test$Reached.on.Time_Y.N,predrf)
cm_rf

#XGBOOST
library(xgboost)
set.seed(1)
dependent_train_xgboost <- train$Reached.on.Time_Y.N 
dependent_test_xgboost <- test$Reached.on.Time_Y.N

dependent_train_xgboost <- as.numeric(dependent_train_xgboost)
dependent_test_xgboost <- as.numeric(dependent_test_xgboost)

train.mx <- sparse.model.matrix(Reached.on.Time_Y.N ~ ., train)
test.mx <- sparse.model.matrix(Reached.on.Time_Y.N ~ ., test)
dtrain <- xgb.DMatrix(train.mx, label = dependent_train_xgboost)
dtest <- xgb.DMatrix(test.mx, label = dependent_test_xgboost)

train.gdbt <- xgb.train(params = list(objective = "binary:logistic",
                                      #num_class = 2,
                                      eval_metric = "auc",
                                      eta = 0.3,
                                      max_depth = 5,
                                      subsample = 1,
                                      colsample_bytree = 0.5), 
                        data = dtrain, 
                        nrounds = 70, 
                        watchlist = list(train = dtrain, test = dtest))

# Generate predictions on test dataset
preds_xg <- predict(train.gdbt, newdata = dtest)

# Compute AUC on the test set
cvAUC::AUC(predictions = preds_xg, labels = dependent_test_xgboost)

#model prediction
xgbpred <- ifelse (preds > 0.5,1,0)

#confusion matrix
cm_xg = confusionMatrix (xgbpred, dependent_test_xgboost)
cm_xg

#view variable importance plot
mat <- xgb.importance (model = train.gdbt)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#===============

library(FactoMineR)
# to use PCA function
library(factoextra)

str(cust_data_table)
new_cust_table = cust_data_table[,-11]

new_cust_table$Warehouse_block = as.numeric(cust_data_table$Warehouse_block)
new_cust_table$Mode_of_Shipment = as.numeric(cust_data_table$Mode_of_Shipment)
new_cust_table$Product_importance = as.numeric(cust_data_table$Product_importance)
new_cust_table$Gender = as.numeric(cust_data_table$Gender)

str(new_cust_table)

pca = prcomp(new_cust_table)
summary(pca)


#summary of PC1 to PC3 individually

#for a colorful plot
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



eig.val <- get_eigenvalue(pca)
eig.val
pca$eig



data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Reached.on.Time_Y.N ~ ., data = train)

# alternatively the traditional interface:
x <- subset(train, select = -Reached.on.Time_Y.N)
y <- train$Reached.on.Time_Y.N
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)
NROW(pred)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(train[,-11])),
     col = as.integer(train[,11]),
     pch = c("o","+")[1:150 %in% model$index + 1])

newdata = data.frame(cust_data_table$Reached.on.Time_Y.N,pca$x)
head(newdata)

svm_mode <- svm(cust_data_table.Reached.on.Time_Y.N ~ PC1+PC2, data = newdata)


func = predict(svm_mod,xgrid,decision.values = T)
#func = predict(svm_mod,predicators_train,decision.values = T)
func=attributes(func)$decision

x=seq(from = min(predicators_train[,14]), to = max(predicators_train[,14]), length = 100)
y=seq(from = min(predicators_train[,15]), to = max(predicators_train[,15]), length = 100)

contour(x,y,z=matrix(func,length(x),length(y)), add = TRUE)

dat_new_pred = data.frame(predicators_test, dependent_test = as.factor(dependent_test))
svm_pred = predict(svm_mod, newdata = dat_new_pred)

cm_svm = confusionMatrix(as.factor(dependent_test),svm_pred)
cm_svm

beta = drop(t(svm_mod$coefs)%*%pca$x[svm_mod$index,c(1,2)])
beta0 = svm_mod$rho
# two most big coefficients are Discount offered (positive) and weight in gms (-negative)
beta=sort(beta)

abline(beta0/beta[2], -beta[2]/beta[1])
abline((beta0 - 1) / beta[15], -beta[1] / beta[15], lty = 2)
abline((beta0 + 1) / beta[15], -beta[1] / beta[15], lty = 2)


plot(xgrid[,c(1,2)],col = c("red","green")[as.numeric(ygrid)], pch = 20, cex = .2)

points(pca$x[,c(1,2)], col = c("red","green")[as.numeric(predtrain)], pch = 18)
#Below shows the support vector
points(predicators_train[svm_mod$index,c(14,15)], pch = 20, cex = .2,col="yellow")




============================

data(iris)
attach(iris)

str(iris)
## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])


