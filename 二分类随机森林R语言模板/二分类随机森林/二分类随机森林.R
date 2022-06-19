#代码介绍：https://www.bilibili.com/video/BV1pL4y1a7sq?p=3&spm_id_from=333.880.my_history.page.click
#数据介绍：http://archive.ics.uci.edu/ml/datasets/Spambase
library(skimr)
library(DataExplorer)
library(caret)
library(pROC)
library(randomForest)
library(tidyverse)
library(Hmisc)
library(varSelRF)
spambase <- read.csv(file.choose(),header = F)
colnames(spambase) <- read.table(file.choose(),skip = 33,sep = ":",comment.char = "")[,1]
colnames(spambase)[ncol(spambase)] <- "spam"
colnames(spambase)[49:54] <- c("A","B","C","D","E","F")#原变量名中有不合规字符

str(spambase)
spambase$spam <- factor(spambase$spam)#应变量转化为因子类型

table(spambase$spam)
#创建训练集【分层抽样】
set.seed(10)
trains <- createDataPartition(
  y=spambase$spam,
  p=0.7,
  list = F
)
traindata <- spambase[trains,]#训练集
testdata <- spambase[-trains,]#验证集
traindata <- testdata
form_cls <- as.formula("spam~.")


# 构建模型
set.seed(17) # 保证结果的可重复性
fit_rf_cls <- randomForest(
  form_cls,
  data = traindata,
  #x=traindata[,1:57], y=traindata[,58],
  ntree = 500, # 决策树棵数
  mtry = 6, # 每个节点可供选择的变量数目
  importance = T # 输出变量重要性
)

# 模型概况
fit_rf_cls
# ntree参数与error之间的关系图示
plot(fit_rf_cls, main = "ERROR & TREES")
legend("top",
       legend = colnames(fit_rf_cls$err.rate),
       lty = 1:3,
       col = 1:3,
       horiz = T)

# 变量重要性
imp <- importance(fit_rf_cls)
imp <- imp[order(imp[,4],decreasing = T),]
varImpPlot(fit_rf_cls, main = "varImpPlot")

#变量筛选
xd <- traindata[,1:57]
clx <- traindata[,58]
kernel <- detectCores()
cl <- makeCluster(kernel)
registerDoParallel(cl)
rf.rvi <- randomVarImpsRF(xd, clx,
                          fit_rf_cls,
                          numrandom = 100,
                          usingCluster = TRUE,
                          TheCluster = cl)
randomVarImpsRFplot(rf.rvi, fit_rf_cls)
stopCluster(cl)
varImpPlot(fit_rf_cls,n.var=10,cex=1)
imp <- importance(fit_rf_cls)
imp <- imp[order(imp[,4],decreasing = T),]

form_cls <- as.formula(
  paste0(
    "spam~",
    paste(rownames(imp)[1:10], collapse = " + ")
  )
)
form_cls

trainpredprob <- predict(fit_rf_cls, newdata = traindata, type = "prob")
# 训练集ROC
trainroc <- roc(response = traindata$spam, predictor = trainpredprob[, 2])
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
# 约登法则
bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities + trainroc$specificities - 1)
]
bestp






