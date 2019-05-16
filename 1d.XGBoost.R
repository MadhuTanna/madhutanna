library(dplyr)
library(glmnet)
library(caret)
library(xgboost)
library(cvAUC)

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

set.seed(1)

predicators_train = model.matrix(train$Reached.on.Time_Y.N~.,train)[,-train$Reached.on.Time_Y.N] ## IV

predicators_test = model.matrix(test$Reached.on.Time_Y.N~.,test)[,-test$Reached.on.Time_Y.N] ## IV

dependent_train_xgboost <- train$Reached.on.Time_Y.N 
dependent_test_xgboost <- test$Reached.on.Time_Y.N

dtrain <- xgb.DMatrix(predicators_train, label = dependent_train_xgboost)
dtest <- xgb.DMatrix(predicators_test, label = dependent_test_xgboost)

searchGrid <- expand.grid(.objective = "binary:logistic",
                          .metric = "auc",
                          .booster= "gbtree",
                          .nfold = seq(5,20,5),
                          .subsample = c(0.5, 0.6), 
                          .colsample_bytree = c(0.5, 0.6),
                          .max_depth = c(3, 4),
                          .min_child_weight = seq(1), 
                          .eta = c(0.1),
                          .nround = c(10,20),
                          .nfold=c(1,10,1)
                          
                          )

rowids = sample(1:NROW(searchGrid),192)

searchGrid1 = searchGrid[rowids,]
searchGrid1

newdf = data.frame()
colnames(newdf) = c("metrics","nrounds","nfold","objective","subsample","colsample_bytree","max_depth","min_child_weight","eta","booster","accuracy")

for (i in seq(1,192,1))
{
  .objective = searchGrid1$.objective[i]
  .metric = searchGrid1$.metric[i]
  .subsample = searchGrid1$.subsample[i] 
  .colsample_bytree = searchGrid1$.colsample_bytree[i]
  .max_depth = searchGrid1$.max_depth[i]
  .min_child_weight = searchGrid1$.min_child_weight[i]
  .eta = searchGrid1$.eta[i]
  .nround = searchGrid1$.nround[i]
  .nfold=searchGrid1$.nfold[i]
  .booster=searchGrid1$.booster[i]

  xgbcvmolel <- xgb.train(data = dtrain,metrics="auc", nrounds = .nround,nfold=.nfold,
                       params = list(objective = "binary:logistic",
                                     subsample=.subsample,
                                     colsample_bytree=.colsample_bytree,
                                     max_depth=.max_depth,
                                     min_child_weight=.min_child_weight,
                                     eta=.eta,
                                     booster = "gbtree"))
  
  predrf_test <- predict(xgbcvmolel, newdata = dtest)
  predrf = ifelse(predrf_test>0.5,1,0)
  cm_rf = confusionMatrix(as.factor(test$Reached.on.Time_Y.N),as.factor(predrf))
  cm_rf$overall[["Accuracy"]]
  newdf = rbind(newdf,data.frame(metrics=.metric,nrounds = .nround,nfold=.nfold,
                                 objective = .objective,
                                 subsample=.subsample,
                                 colsample_bytree=.colsample_bytree,
                                 max_depth=.max_depth,
                                 min_child_weight=.min_child_weight,
                                 eta=.eta,
                                 booster = .booster,
                                 accuracy = cm_rf$overall[["Accuracy"]]))
}  

newdf

#view variable importance plot
mat <- xgb.importance (model = xgbcvmolel)
xgb.plot.importance (importance_matrix = mat[1:5])

#xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
#rmse <- tail(xvalidationScores$test_rmse_mean, 1)
#trmse <- tail(xvalidationScores$train_rmse_mean,1)
#output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))
