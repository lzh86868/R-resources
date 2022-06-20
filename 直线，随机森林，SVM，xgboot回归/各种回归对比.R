library(caret)#计算性能
library(car)#计算VIF
library(lmtest)#回归诊断
#构造演示数据
set.seed(101)
x1 <- rnorm(1000,20,4)
x2 <- rnorm(1000,30,5)
x3 <- rnorm(1000,5,1)
x4 <- rnorm(1000,3,2)
x5 <- x2+rnorm(1000,1,0.1)
#y <- 2*x1+x2^2+5*x3^(-1.5)+1.5*x4-6*x5^3*x1^5+rnorm(1000,10,10)
y <- (2*x1+x2^2+5*x3^(-1.5)+1.5*x4-6*x5^3*x1^4.5)*10^(-10)
data_all <- data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,y=y)
#划分训练集和验证集
set.seed(102)
train_id <- sample(1:length(data_all[,1]),length(data_all[,1])*0.7)
traindata <- data_all[train_id,]#训练集
testdata <- data_all[-train_id,]#验证集
##直线回归----------------------------------------------------------------------
lm_fit <- lm(y~.,data = traindata)
summary(lm_fit)
vif(lm_fit)#共线性检查
#回归诊断
par(mfrow=c(2,2))
plot(lm_fit)
bptest(lm_fit)
dwtest(lm_fit)
#预测情况
pred_lm_train <- predict(lm_fit,newdata = traindata)
pred_lm_test <- predict(lm_fit,newdata = testdata)
#基础作图
plot(x=testdata$y,
     y=pred_lm_test,
     xlab="Aucual",
     ylab="Prediction",
     main="直线回归")
abline(a=0,b=1,col="red")
defaultSummary(data.frame(obs=testdata$y,pred=pred_lm_test))#预测性能
#ggplot2作图
lm_result <- data.frame(Aucual=c(traindata$y,testdata$y),
                        Prediction=c(pred_lm_train,pred_lm_test),
                        group=c(rep("train",each=length(pred_lm_train)),
                                rep("test",each=length(pred_lm_test))))
lm_pe<- ggplot(lm_result,
       aes(x=Aucual,y=Prediction,fill=group,colour=group))+
  geom_point(shape=21,size=1)+
  geom_smooth(se=F,size=0.8)+
  geom_abline(intercept = 0,slope = 1,size=0.8)+
  theme(axis.title = element_text(size=13),
        plot.title = element_text(size=16,hjust = 0.5))+
  labs(title="线性回归")
  
##随机森林回归------------------------------------------------------------------
library(randomForest)
form_reg <- as.formula("y~.")
set.seed(42)
fit_rf_reg <- randomForest(
  form_reg,
  data = traindata,
  ntree = 500, # 决策树棵数
  mtry = 3, # 每个节点可供选择的变量数目
  importance = T # 输出变量重要性
)
fit_rf_reg
# ntree参数与error之间的关系图示
plot(fit_rf_reg, main = "ERROR & TREES")
#自变量对因变量影响
partialPlot(x=fit_rf_reg,
            pred.data = traindata,
            x.var = x5)
#变量重要性
importance(fit_rf_reg)
varImpPlot(fit_rf_reg, main = "Variable Importance Plot")
#预测情况
pred_rf_train <- predict(fit_rf_reg,newdata = traindata)
pred_rf_test <- predict(fit_rf_reg,newdata = testdata)
#基础作图
plot(x=testdata$y,
     y=pred_rf_test,
     xlab="Aucual",
     ylab="Prediction",
     main="随机森林回归")
abline(a=0,b=1,col="red")
defaultSummary(data.frame(obs=testdata$y,pred=pred_rf_test))#预测性能
#ggplot2作图
rf_result <- data.frame(Aucual=c(traindata$y,testdata$y),
                        Prediction=c(pred_rf_train,pred_rf_test),
                        group=c(rep("train",each=length(pred_rf_train)),
                                rep("test",each=length(pred_rf_test))))
rf_pe<- ggplot(rf_result,
               aes(x=Aucual,y=Prediction,fill=group,colour=group))+
  geom_point(shape=21,size=1)+
  geom_smooth(se=F,size=0.8)+
  geom_abline(intercept = 0,slope = 1,size=0.8)+
  theme(axis.title = element_text(size=13),
        plot.title = element_text(size=16,hjust = 0.5))+
  labs(title="随机森林回归")

##支持向量机回归----------------------------------------------------------------
library(e1071)
form_reg <- as.formula("y~.")
fit_svm_reg <- svm(form_reg,
                   data = traindata,
                   kernel = "poly",
                   cost = 1,
                   gamma = 0.1,
                   degree=5,
                   coef0=1)
fit_svm_reg
#预测情况
pred_svm_train <- predict(fit_svm_reg,newdata = traindata)
pred_svm_test <- predict(fit_svm_reg,newdata = testdata)
#基础作图
plot(x=testdata$y,
     y=pred_svm_test,
     xlab="Aucual",
     ylab="Prediction",
     main="支持向量机回归")
abline(a=0,b=1,col="red")
defaultSummary(data.frame(obs=testdata$y,pred=pred_svm_test))#预测性能
#ggplot2作图
svm_result <- data.frame(Aucual=c(traindata$y,testdata$y),
                        Prediction=c(pred_svm_train,pred_svm_test),
                        group=c(rep("train",each=length(pred_svm_train)),
                                rep("test",each=length(pred_svm_test))))
svm_pe<- ggplot(svm_result,
               aes(x=Aucual,y=Prediction,fill=group,colour=group))+
  geom_point(shape=21,size=1)+
  geom_smooth(se=F,size=0.8)+
  geom_abline(intercept = 0,slope = 1,size=0.8)+
  theme(axis.title = element_text(size=13),
        plot.title = element_text(size=16,hjust = 0.5))+
  labs(title="支持向量机回归")

##xgboost回归-------------------------------------------------------------------
library(xgboost)
set.seed(103)
trains2 <- sample(train_id,length(train_id)*0.8)
valids <- setdiff(train_id,trains2)
data_train <- data_all[trains2,]#训练集
data_valid <- data_all[valids,]#控制集
data_test <- data_all[-train_id,]#验证集
# 训练集
dvfunc <- dummyVars(~., data = data_train[, 1:5], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:5])
data_trainy <- data_train$y
# 验证集
data_validx <- predict(dvfunc, newdata = data_valid[, 1:5])
data_validy <- data_valid$y
# 测试集
data_testx <- predict(dvfunc, newdata = data_test[, 1:5])
data_testy <- data_test$y
# 构建xgb.DMatrix格式数据
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
set.seed(104)
fit_xgb_reg <- xgb.train(
  data = dtrain,
  eta = 0.4,
  gamma = 0.001,
  max_depth = 2,
  subsample = 0.8,
  colsample_bytree = 0.6,
  objective = "reg:squarederror",
  nrounds = 10000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 10000
)
xgb.plot.shap(data = data_trainx, 
              model = fit_xgb_reg,
              top_n = 5)#意义不明
#变量重要性
importance_matrix <- xgb.importance(model = fit_xgb_reg)
xgb.plot.importance(importance_matrix=importance_matrix,
                   measure = "Gain",
                    main = "Variable Importance Plot")
#预测情况
pred_xg_train <- predict(fit_xgb_reg,newdata = dtrain)
pred_xg_test <- predict(fit_xgb_reg,newdata = dtest)
#基础作图
plot(x=testdata$y,
     y=pred_xg_test,
     xlab="Aucual",
     ylab="Prediction",
     main="xgboost回归")
abline(a=0,b=1,col="red")
defaultSummary(data.frame(obs=testdata$y,pred=pred_xg_test))
#ggplot2作图
xg_result <- data.frame(Aucual=c(data_train$y,testdata$y),
                         Prediction=c(pred_xg_train,pred_xg_test),
                         group=c(rep("train",each=length(pred_xg_train)),
                                 rep("test",each=length(pred_xg_test))))
xg_pe<- ggplot(xg_result,
                aes(x=Aucual,y=Prediction,fill=group,colour=group))+
  geom_point(shape=21,size=1)+
  geom_smooth(se=F,size=0.8)+
  geom_abline(intercept = 0,slope = 1,size=0.8)+
  theme(axis.title = element_text(size=13),
        plot.title = element_text(size=16,hjust = 0.5))+
  labs(title="xgboost回归")

#保存图片
library(ggpubr)
totle_pe <- ggarrange(lm_pe,rf_pe,svm_pe,xg_pe)
pdf(file="totle_pe(s).pdf",width=12,height=10,family = "GB1") 
totle_pe
dev.off()