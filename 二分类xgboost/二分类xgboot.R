#数据介绍参考决策树
#方法参考：https://www.bilibili.com/video/BV1x34y1B7sw?p=3&vd_source=bb4c4d428098cdeb66fdd2a88cd800b7
library(skimr)
library(DataExplorer)
library(caret)
library(pROC)
library(xgboost)
# 数据准备
spambase <- read.csv(file.choose(),header = F)
colnames(spambase) <- read.table(file.choose(),skip = 33,sep = ":",comment.char = "")[,1]
colnames(spambase)[ncol(spambase)] <- "spam"
str(spambase)
skim(spambase)
spambase$spam <- factor(spambase$spam)#应变量转化为因子类型
#创建训练集【分层抽样】
set.seed(101)
trains <- createDataPartition(
  y=spambase$spam,
  p=0.7,
  list = F
)
trains2 <- sample(trains,length(trains)*0.3)
valids <- setdiff(trains,trains2)
data_train <- spambase[trains2,]#训练集
data_valid <- spambase[valids,]#控制集
data_test <- spambase[-trains,]#验证集
#data_train <- data_test #后面测试用
# 训练集
dvfunc <- dummyVars(~., data = data_train[, 1:57], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:57])
data_trainy <- ifelse(data_train$spam == 0, 0, 1)
# 验证集
data_validx <- predict(dvfunc, newdata = data_valid[, 1:57])
data_validy <- ifelse(data_valid$spam == 0, 0, 1)
# 测试集
data_testx <- predict(dvfunc, newdata = data_test[, 1:57])
data_testy <- ifelse(data_test$spam == 0, 0, 1)
# 构建xgb.DMatrix格式数据
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)

# 训练模型
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 2,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 200
)

# 模型概要
fit_xgb_cls
# 变量重要性
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 5)


trainpredprob <- predict(fit_xgb_cls, newdata = dtrain)
#trainpredprob <- predict(fit_xgb_cls, newdata = traindata, type = "prob")
# 训练集ROC
trainroc <- roc(response = data_train$spam, predictor = trainpredprob)
# 训练集ROC曲线
plot(trainroc,
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = T,
     max.auc.polygon = T,
     auc.polygon.col = "skyblue",
     print.thres = T,
     legacy.axes = T,
     bty = "l")

testpredprob <- predict(fit_xgb_cls, newdata = dtest)
#trainpredprob <- predict(fit_xgb_cls, newdata = traindata, type = "prob")
# 验证集ROC
testroc <- roc(response = data_test$spam, predictor = testpredprob)
# 验证集ROC曲线
plot(testroc,
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = T,
     max.auc.polygon = T,
     auc.polygon.col = "skyblue",
     print.thres = T,
     legacy.axes = T,
     bty = "l")


