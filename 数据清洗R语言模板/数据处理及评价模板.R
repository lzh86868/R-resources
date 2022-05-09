###清洗及描述部分包下载
library(Hmisc)#读取SPSS文件
library(openxlsx)#读取Excel文件
library(pastecs)#描述统计量
library(psych)#描述统计量
library(VIM)#总体评价
library(mice)#多重插补
library(haven)#写入spss(sav格式)
###数据读取及原始数据统计--------------------------------------------------------------------
health_data1 <- spss.get(file.choose())#读取SPSS数据(若含有中文字符，需保存为本地编码格式)
health_data1 <- read.xlsx(file.choose())#读取Excel数据
View(head(health_data1)) #检查读取(Rstudio中使用)
des1 <- summary(health_data1)
des2 <- stat.desc(health_data1,norm = T)
des3 <- Hmisc::describe(health_data1)
des4 <- psych::describe(health_data1)
describe_list <- list(des1,des2,des3,des4)#原始数据信息
health_data1 <- as.data.frame(apply(health_data1, 2, as.character))
#aggregate.data.frame(myda[,-c(1:4)],by=list(myda$性别,myda$吸烟情况),FUN = sd)#分组描述参考
#summary(health_data1)#检查转化（去掉第一个“#”执行）
###-----------------------------------------------------------------------------------------



###完整性评价-------------------------------------------------------------------------------
##人员差缺率------------------------------------------------------------
cqs <- length(health_data1[is.na(health_data1)])#总指标差缺数
zbr <- (length(health_data1))*length(health_data1[,1])#指标*人数
cqz <- cqs/zbr #人员差缺率
cqz
rm(cqs,zbr)
##指标差缺率------------------------------------------------------------
zcql <- c()#指标差缺率
zb <- colnames(health_data1)[-1]
for (i in 2:length(health_data1)) {
  zcql[i-1] <- length(health_data1[i][is.na(health_data1[i])])/length(health_data1[,i])
}
zcqld <- data.frame(zb, zcql)
colnames(zcqld) <- c("指标","指标差缺率")
zcqld #指标差缺率表格
rm(zb,zcql,i)
##缺失记录定位----------------------------------------------------------
ra <- which(is.na(health_data1))#整体位置
hang <- ifelse(ra %%length(health_data1[,1])==0,
               length(health_data1[,1]),
               ra %%length(health_data1[,1]))#行位置
lie <- ifelse(ra %%length(health_data1[,1])==0,
              ra%/%length(health_data1[,1]),
              ra%/%length(health_data1[,1])+1)#列位置
weizhi <- data.frame(hang,lie)#准确位置
colnames(weizhi) <- c("行","列")
weizhi
rm(ra,hang,lie)
###----------------------------------------------------------------------------------------



###准确性评价-------------------------------------------------------------------------------
##---------------------------------------------------------------------
#health_data1 <- spss.get(file.choose())#读取SPSS数据(若含有中文字符，需保存为本地编码格式)：测试用
#health_data1 <- read.xlsx(file.choose())#读取Excel数据:测试用
#health_data1 <- as.data.frame(apply(health_data1, 2, as.character)) ：测试用

##分类变量是否混入无效值检查；比如1表示男，2表示女，数据中出现3----------
View(head(health_data1)) #方便定位
effectv <- list(as.character(c(1,0)),as.character(c(1,0)),as.character(1:5))#有效值（按顺序输入，可扩展）
dcl <- c(2,3,6)#需检查列，要与有效值顺序对应
for (i in 1:length(dcl)) {
  autua <- unique(health_data1[,dcl[i]])
  uneffectv <- setdiff(autua,effectv[[i]])#无效值
  health_data1[,dcl[i]][health_data1[,dcl[i]] %in% uneffectv] <- "10086" #无效值赋值
}
##数字混进字符------------------------------------------------------------
dclt <- c(2,3,6)#需检查列
for (i in dclt) {
  zifu <- setdiff(health_data1[,i],as.character(as.numeric(health_data1[,i])))
  health_data1[,i][health_data1[,i] %in% zifu] <- "10086"
} #数字混进字符

##字符混进数字------------------------------------------------------------
dclm <- c(7,8)#需检查列
for (i in dclm) {
  shuzi <-intersect(health_data1[,i],as.character(as.numeric(health_data1[,i])))
  shuzi <- shuzi[!is.na(shuzi)]
  health_data1[,i][health_data1[,i] %in% shuzi] <- "10086"
}#字符混进数字

##收缩压小于舒张压检查----------------------------------------------------
higblood <- 5 #收缩压数据所在列
lowblood <- 4 #舒张压数据所在列
bloodd <- rownames(health_data1)[as.numeric(health_data1[,higblood])-as.numeric(health_data1[,lowblood]) <= 0] #血压无效值
bloodd <- as.integer(bloodd)
health_data1[,lowblood][bloodd] <- "10086"
health_data1[,higblood][bloodd] <- "10086"

##总指标无效值检查-------------------------------------------------------
wxzs <- length(health_data1[health_data1=="10086"])#总指标无效值数量
zbr <- (length(health_data1)-1)*length(health_data1[,1])#指标*人数
xwsz <- wxzs/zbr #总无效值比例
xwsz
rm(wxzs,zbr)

##单项指标无效值---------------------------------------------------------
dzcql <- c()#单项指标无效值比例
zb <- colnames(health_data1)[-1]
for (i in 2:length(health_data1)) {
  dzcql[i-1] <- length(health_data1[i][health_data1[i]=="10086"])/length(health_data1[,i])
}
dzcqld <- data.frame(zb, dzcql)
colnames(dzcqld) <- c("指标","指标无效值比例")
dzcqld #指标无效值比例表格
rm(zb,dzcql,i)

##无效值定位-------------------------------------------------------------
wra <- which(health_data1=="10086")#整体位置
whang <- ifelse(wra %%length(health_data1[,1])==0,
               length(health_data1[,1]),
               wra %%length(health_data1[,1]))#行位置
wlie <- ifelse(wra %%length(health_data1[,1])==0,
              wra%/%length(health_data1[,1]),
              wra%/%length(health_data1[,1])+1)#列位置
weizhi <- data.frame(whang,wlie)#准确位置
colnames(weizhi) <- c("行","列")
weizhi
rm(ra,hang,lie)
wweizhi <- data.frame(whang,wlie)#准确位置
colnames(wweizhi) <- c("行","列")
wweizhi
rm(wra,whang,wlie)
#无效值标记为NA---------------------------------------------------------
health_data1[health_data1=="10086"] <- NA

##唯一性检查--------------------------------------------------------------
id_u <- 1#唯一身份识别信息所在列
id_NA <- which(is.na(health_data1[,id_u])==T)#唯一身份识别信息缺失R_dataframe索引
id_NA
for (i in 1:length(id_NA)) {
  health_data1[id_NA,id_u][i] <- paste0("未知",i)
}#标记未知列
chongfux <- health_data1[duplicated(health_data1[,id_u]),id_u]#所有重复
health_data1[which(health_data1[,id_u] %in% chongfux),]#所有重复记录展示
chongfuy <- health_data1[duplicated(health_data1),id_u]#完全重复
health_data1[which(health_data1[,id_u] %in% chongfuy),]#完全重复记录展示
bwqchongfu <- setdiff(chongfux,chongfuy)#不完全重复
health_data1[which(health_data1[,id_u] %in% bwqchongfu),]#不完全重复记录展示
zcfl <- length(chongfux)/length(health_data1[,id_u])#总重复率
zcfl
wqcfl <- length(chongfuy)/length(health_data1[,id_u])#完全重复率
wqcfl
bwqcfl <- length(bwqchongfu)/length(health_data1[,id_u])#不完全重复率
bwqcfl
origin_len <- length(health_data1[,1])
health_data1 <- unique(health_data1[!(health_data1[,id_u] %in% bwqchongfu),])#去除重复值（包括不完全）
use_record1 <- length(health_data1[,1])/origin_len#可用值比例
##离群值检查--------------------------------------------------------------
#health_data1 <- spss.get(file.choose())#读取SPSS数据(若含有中文字符，需保存为本地编码格式)：测试用
#health_data1 <- read.xlsx(file.choose())#读取Excel数据:测试用
#health_data1 <- as.data.frame(apply(health_data1, 2, as.character)) ：测试用
dcv <- c(5:12)#待检测指标
zoutlie <- list()
lqzbl <- c()#离群值比例
for (i in 1:length(dcv)) {
  lie_rank <- dcv[i]
  jclz <- as.numeric(health_data1[,lie_rank])
  qx <- quantile(jclz,na.rm = T)
  IQR <- qx[4]-qx[2]
  names(IQR) <- "IQR"
  highlim <- qx[4]+1.5*IQR
  lowlim <- qx[2]-1.5*IQR
  outliem <- jclz[jclz>highlim | jclz<lowlim]#四分位间距法离群值
  indm <- which(jclz>highlim | jclz<lowlim)#离群值索引（四分位间距法）
  meand <- mean(jclz,na.rm = T)
  sdd <- sd(jclz,na.rm = T)
  minlim <- meand-1.96*sdd
  maxlim <- meand+1.96*sdd
  outlies <- jclz[jclz>maxlim | jclz<minlim]#均数标准差法离群值
  inds <- which(jclz>maxlim | jclz<minlim)#离群值索引（均数标准差法）
  zoutlie[[i]] <- jclz[intersect(indm,inds)]#两种方法的交集（最终离群值）
  lqzbl[i] <- length(zoutlie[[i]])/length(jclz) #离群值比例
}
names(zoutlie) <- colnames(health_data1)[dcv]
lqzbl_df <- data.frame(colnames(health_data1)[dcv],lqzbl)#离群值比例列表
zlqz <- mean(lqzbl)#总离群值比例
colnames(lqzbl_df) <- c("指标","离群值比例")  
lqzbl_df#离群值比例列表
zlqz #总离群值比例
#离群值标记--------------------------------------------------------------
health_data2 <- health_data1
for (i in 1:length(dcv)) {
  lie_rank <- dcv[i]
  health_data2[,lie_rank][health_data2[,lie_rank] %in% zoutlie[[i]]] <- "outlies"
  print(length(which(health_data2[,lie_rank] %in% "outlies"))==length(zoutlie[[i]]))
}
hangx <- (which(health_data2=="outlies")%%length(health_data2[,1]))
hangx[hangx==0] <- length(health_data2[,1])
health_data2 <- health_data2[-hangx,]#删除离群值的数据集
health_data1[,dcv] <- apply(health_data1[,dcv],2,as.numeric)
health_data2[,dcv] <- apply(health_data2[,dcv],2,as.numeric)

#health_data1为保留离群值数据；health_data2为删除离群值数

###总体数据质量评价-----------------------------------------------------------------------------------------
use_record1  #可用记录比例（保留离群值）
use_record2 <- length(health_data2[,1])/origin_len##可用记录比例（删除离群值）
#保留离群值
aggr_plot1 <- aggr( health_data1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(health_data1), cex.axis=.7, gap=3, ylab=c("Histogram of abnormal data","Pattern"))
parten1 <- md.pattern(health_data1)
#删除离群值
aggr_plot2 <- aggr( health_data2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(health_data2), cex.axis=.7, gap=3, ylab=c("Histogram of abnormal data","Pattern"))
parten2 <- md.pattern(health_data2)
###---------------------------------------------------------------------------------------------------------



###数据处理-------------------------------------------------------------------------------------------------
#health_data1 <- spss.get("D:/Desktop/jupfile/health_test2.sav")#读取SPSS数据(若含有中文字符，需保存为本地编码格式)：测试用
#health_data1 <- read.xlsx(file.choose())#读取Excel数据:测试用
#health_data1 <- as.data.frame(apply(health_data1, 2, as.character)) ：测试用
#health_data1 <- unique(health_data1[!(health_data1[,id] %in% bwqchongfu),]) #重复值处理:数据检查时已做
dec_health <- health_data1[,-c(1)] #需填补数据集部分（去掉非研究变量）
#须填补列中有因子型变量需将其转换成数字,若后续需要再自行转化成因子
iddz <- c(1:11)#需转化成数字的列
iddx <- c(1,4:11)#均数填充列
iddy <- c(2:3)#众数填充列
#计数资料无空值不需要执行
for(i in iddz){
  dec_health[,i] <- as.numeric(dec_health[,i])
}
#data：包含缺失值的数据框或矩阵。缺失值被编码为 NA。
#m：多重插补法的数量，默认为 5。
#method：指定数据中每一列的输入方法。1）默认 pmm；2）二分类数据适用 logreg；3）无序多类别数据适用 ployreg；4）有序多分类变量适用 polr。详细参考?mice
#maxit：迭代次数，默认为 5 。
#seed：一个整数，由set.seed()产生，作为偏移随机数生成器。默认情况下，不使用随机数生成器。
mt <- 5 #插补次数
tempData <- mice(dec_health,m=mt,maxit=50,seed=600,method = "pmm")
summary(tempData)
#寻找合适值
densityplot(tempData)
densityplot(tempData,~`空腹血糖`+`HDL` | .imp)
#自行选择合适值填充-------------------------------
id_dt <- 1#原数据
tic <- 2#打算填充次数
dait <- dec_health[,id_dt]
dait <- complete(tempData,action = tic)[,id_dt]
#平均值填充(计量资料)-----------------------------
ss <- 0
for(i in 1:mt){
  ddaa <- complete(tempData,action = i)
  ss <- ss + ddaa[,iddx]
}
dec_health[,iddx] <- ss/mt 

#众数填补（计数资料）-----------------------------
zhongshu <- iddy#计数资料填补列
health_data1y <- complete(tempData,action = 1)
sdt <- complete(tempData,action = 1)
for (i in 2:mt) {
  sdt <- rbind(sdt,complete(tempData,action = i))
}
sdt$group <- rep(c(1:length(complete(tempData,action = 1)[,1])),time=mt)
for (i in 1:length(complete(tempData,action = 1)[,1])) {
  lgde <- sdt[sdt$group==i,]
  for (m in zhongshu) {
    ifelse(is.na(as.integer(names(which.max(table(lgde[,m])))))==T ,
           health_data1y[i,m] <- names(which.max(table(lgde[,m]))),
           health_data1y[i,m] <- as.integer(names(which.max(table(lgde[,m])))))
  }
}
dec_health[,iddy] <- health_data1y[,iddy]
aggr_plot3 <- aggr(dec_health, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dec_health), cex.axis=.7, gap=3, ylab=c("Histogram of abnormal data","Pattern"))
save.image(file = "env_data_deal.RData") #保存所有变量
saveRDS(dec_health,"处理完成数据.RDS")

###结果描述-----------------------------------------------------------------------------








