#数据介绍参考决策树
#方法参考：https://www.bilibili.com/video/BV1gP4y187hg?p=3&vd_source=bb4c4d428098cdeb66fdd2a88cd800b7
#https://www.bilibili.com/video/BV1gP4y187hg?p=5&vd_source=bb4c4d428098cdeb66fdd2a88cd800b7
library(e1071)
library(caret)
library(pROC)
library(DataExplorer)
library(skimr)
library(foreach)
library(doParallel)
spambase <- read.csv(file.choose(),header = F)
colnames(spambase) <- read.table(file.choose(),skip = 33,sep = ":",comment.char = "")[,1]
colnames(spambase)[ncol(spambase)] <- "spam"
str(spambase)
skim(spambase)
spambase$spam <- factor(spambase$spam)#应变量转化为因子类型
table(spambase$spam)
#创建训练集【分层抽样】
set.seed(101)
trains <- createDataPartition(
  y=spambase$spam,
  p=0.5,
  list = F
)
traindata <- spambase[trains,]#训练集
testdata <- spambase[-trains,]#验证集
traindata <- testdata
form_cls <- as.formula("spam~.")
fit_svm_cls <- svm(form_cls,
                   data = traindata,
                   kernel = "radial",
                   cost = 2,
                   gamma = 0.1,
                   probability = T)
###超参数寻优
m <- c(-2:1)
n <- c(-2:1)
dft <- data.frame(x=rep(m,each=length(n)),y=rep(n,time=length(m)))
form_cls <- as.formula("spam~.")
testfun <- function(ind){
  fit_linear <- tune(
    svm,
    form_cls,
    data = traindata,
    kernel = "radial",
    ranges = list(cost = 2^dft[ind,1], gamma = 10^dft[ind,2]),
    probability = TRUE,
    tunecontrol = tune.control(random = 101,
                               sampling = "bootstrap",
                               nboot = 3,
                               boot.size = 1,
                               best.model = T,
                               performances = T)
  )
  fit_linear$best.parameters$erro <- fit_linear$best.performance
  shuchu <- as.matrix(fit_linear$best.parameters)
  return(shuchu)
}
kernel <- detectCores()
cl <- makeCluster(kernel)
registerDoParallel(cl)
clusterEvalQ(cl,library(e1071))

mod_choose <- foreach(x=1:16,.combine = "rbind",.errorhandling = "pass")%dopar%testfun(x)
stopCluster(cl)
mod_choose_df <-as.data.frame(mod_choose)
best_mod <- mod_choose_df[which.min(mod_choose_df$erro),]

# 模型概况
fit_svm_cls
#训练集预测概率
trainpred <- predict(fit_svm_cls, newdata = traindata, probability = T)
trainpredprob <- attr(trainpred, "probabilities")
# 训练集ROC
trainroc <- roc(response = traindata$spam, predictor = trainpred)
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

  
  




