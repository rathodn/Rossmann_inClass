
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)

library(xgboost)
require(Matrix)
library(Metrics)
library(sqldf)

# str(f_train_store)
# str(f_test_store)

# ggplot(trainData[Sales != 0], 
#        aes(x = as.Date(Date), y = Sales, color = year)) + 
#   geom_smooth(size = 2)

# save(f_train_store,file="F1_train_store.RDATA")
# save(f_test_store,file="F1_test_store.RDATA")

str(f_train_store)

rmspe <- function(truth_y, pred_y) {
  nonzero <- truth_y > 0
  n <- sum(nonzero)
  diff <- (truth_y[nonzero] - pred_y[nonzero]) / truth_y[nonzero]
  diff <- diff**2
  diff <- sqrt(sum(diff)/n)
  return(diff)
}
# 
# load("F1_train_store.RDATA")
# load("F1_test_store.RDATA")

ftr = f_train_store

# ftr$Volume[is.na(ftr$Volume)] = 0
# ftr$Index[is.na( ftr$Index)] = 0

trainData = ftr

summary(ftr)

trainData$Customers = NULL
trainData$year = NULL
trainData$PromoInterval = NULL
trainData$month = NULL
trainData$Promo2Since = NULL
trainData$Promo2SinceWeek = NULL
trainData$Promo2SinceYear = NULL
trainData$CompetitionOpenSinceMonth = NULL
trainData$CompetitionOpenSinceYear = NULL
trainData$CompetitionOpenSince = NULL
#trainData$SchoolHoliday = NULL
#trainData$Promo2 = NULL
trainData$OpenonStateHoliday = NULL
#trainData$Promo = NULL
trainData$promo2Days = NULL
trainData$schoolOff = NULL
trainData$StateHoliday = NULL
# trainData$Index = NULL
# trainData$Volume = NULL
#trainData$StoreType = NULL
# trainData$comOpenSince = NULL
# trainData$compSinceDays = NULL
# trainData$promo2Run = NULL
# trainData$Promo2 =NULL
# trainData$CompetitionDistance = NULL



#############################################################################################   

trainData = trainData[trainData$Sales > 0, ]

trainData$Sales = log(trainData$Sales)

testD = trainData[trainData$Date >= "2015-05-05",]
trainD = trainData[trainData$Date < "2015-05-05",]

# index <- sample(nrow(trainD),nrow(trainD) * 0.10)
# trainD = trainD[index,]

Last6WSales = testD$Sales
testD$Sales <- NULL

trainD_sparse <- sparse.model.matrix(Sales~., data=trainD)

dtrain <- xgb.DMatrix(
  data=trainD_sparse, 
  label=as.numeric(trainD$Sales)
)

testD_sparse <- sparse.model.matrix(~., data=testD)

set.seed(21)
param <- list(
  objective="reg:linear",
  booster="gbtree",
  eta= 0.15,
  max.depth=7, # Maximum depth of the tree
  subsample= 0.9, # subsample ratio of the training instance
  colsample_bytree=0.75,
  min_child_weight = 4,
  #eval_metric = "rmse",
  #set.seed =21,
  early.stop.round=11,
  eval_metric = "rmse",
  verbose = 1
)

md <- xgb.train(data=dtrain, params=param, nrounds=1500, nthread=8)


pred_xgb=predict(md, testD_sparse)

# summary(exp(pred_xgb))
# summary(exp(Last6WSales))

summary(exp(pred_xgb))
summary(exp(Last6WSales))
rmse(pred_xgb,Last6WSales)

rmspe(exp(pred_xgb),exp(Last6WSales))


#####################################################################33333333333333333333333
#####################################################################33333333333333333333333


