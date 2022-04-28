###数据读取及原始数据统计--------------------------------------------------------------------
library(Hmisc)#读取SPSS文件
library(openxlsx)#读取Excel文件
library(pastecs)#描述统计量
library(psych)#描述统计量
library(VIM)#总体评价
library(mice)#多重插补
library(prettyR)#计算众数
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
zbr <- (length(health_data1)-1)*length(health_data1[,1])#指标*人数
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
hang <- ra%%length(health_data1[,1])#行位置
lie <- ra%/%length(health_data1[,1])#列位置
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
whang <- wra%%length(health_data1[,1])#行位置
wlie <- wra%/%length(health_data1[,1])#列位置
wweizhi <- data.frame(whang,wlie)#准确位置
colnames(wweizhi) <- c("行","列")
wweizhi
rm(wra,whang,wlie)
#无效值标记为NA---------------------------------------------------------
health_data1[health_data1=="10086"] <- NA

##唯一性检查--------------------------------------------------------------
id_unique <- health_data1[,1]#唯一身份识别信息所在列
chongfux <- id_unique[duplicated(id_unique)]#所有重复
health_data1[which(id_unique %in% chongfux),]#所有重复记录展示
chongfuy <- id_unique[duplicated(health_data1)]#完全重复
health_data1[which(id_unique %in% chongfuy),]#完全重复记录展示
bwqchongfu <- setdiff(chongfux,chongfuy)#不完全重复
health_data1[which(id_unique %in% bwqchongfu),]#不完全重复记录展示
zcfl <- length(chongfux)/length(id_unique)#总重复率
zcfl
wqcfl <- length(chongfuy)/length(id_unique)#完全重复率
wqcfl
bwqcfl <- length(bwqchongfu)/length(id_unique)#不完全重复率
bwqcfl
origin_len <- length(health_data1[,1])
health_data1 <- unique(health_data1[!(id_unique %in% bwqchongfu),])#去除重复值（包括不完全）
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
  print(length(health_data2[,lie_rank][is.na(health_data2[,lie_rank])])==length(zoutlie[[i]]))
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
#health_data1 <- unique(health_data1[!(health_data1[,id] %in% bwqchongfu),]) #重复值处理:数据检查时已做
dec_health <- health_data1[,-c(1,2)] #需填补数据集
iddx <- c(5:12)#需转化成数字的列
iddy <- c()#需转化成因子的列
dec_health[,iddx] <- as.numeric(dec_health[,iddx])
dec_health[,iddy] <- as.factor(dec_health[,iddy])#等级变量需要额外设置leves参数
#data：包含缺失值的数据框或矩阵。缺失值被编码为 NA。
#m：多重插补法的数量，默认为 5。
#method：指定数据中每一列的输入方法。1）数值型数据适用 pmm；2）二分类数据适用 logreg；3）无序多类别数据适用 ployreg；4）有序多分类变量适用 polr。默认方法为 pmm 。
#maxit：迭代次数，默认为 5 。
#seed：一个整数，由set.seed()产生，作为偏移随机数生成器。默认情况下，不使用随机数生成器。
mt <- 5 #插补次数
tempData <- tempData <- mice(dec_health,m=mt,maxit=mt0,seed=mt00)
#寻找合适值
densityplot(tempData)
densityplot(tempData,~`空腹血糖`+`高密度脂蛋白胆固醇` | .imp)
#自行选择合适值填充-------------------------------
id_dt <- 1
dait <- dec_health[,id_dt]
dait <- complete(tempData,action = mt)[,id_dt]
#平均值填充(计量资料)-----------------------------
ss <- 0
dfc <- c(3:10)
for(i in 1:mt){
  ddaa <- complete(tempData,action = 1)
  ss <- ss + ddaa[,dfc]
}
health_data1x <- ss/mt #平均值填充完需自行与原数据核对后考虑合并

#众数填补（计数资料）-----------------------------
zhongshu <- c(1,2)#计数资料填补列
health_data1y <- complete(tempData,action = 1)
sdt <- complete(tempData,action = 1)
for (i in 2:mt) {
  sdt <- rbind(sdt,complete(tempData,action = i))
}
sdt$group <- rep(c(1:length(complete(tempData,action = 1)[,1])),time=mt)
for (i in 1:length(complete(tempData,action = 1)[,1])) {
  lgde <- sdt[sdt$group==i,]
  for (m in zhongshu) {
    ifelse(as.integer(names(which.max(table(lgde[,1])))) == NA,
           health_data1y[i,m] <- names(which.max(table(lgde[,m]))),
           health_data1y[i,m] <- as.integer(names(which.max(table(lgde[,1])))))
  }
}

save.image(file = "env_data_deal.RData") #保存结果


###结果描述-----------------------------------------------------------------------------


library(reshape2)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
myda <- health_data1#数据清洗完成的数据集
aggregate.data.frame(myda[,-c(1:4)],by=list(myda$性别,myda$吸烟情况),FUN = sd)#分组描述
#myda$性别 <- factor(myda$性别,levels = c(1,2),labels = c("男","女"))#读取Excel文件需要，SPSS文件不需要
#myda$吸烟情况 <- factor(myda$吸烟情况,levels = c(0,1),labels = c("不吸烟","吸烟"))#同上
mydalo <- melt(myda,id=c("身份证号码","性别","吸烟情况","年龄"))#根据自己数据决定，需要了解reshape2包详细使用方法

#疾病诊断-----------------------------------------------------------------------------------------------------------
diagn <- data.frame(ID = myda[,1])#患病情况
zj <- length(myda[,1])
diagn$sex <- myda$性别
diagn$age <- myda$年龄
diagn$agepe <- cut(myda$年龄,2)
diagn$smoke <- myda$吸烟情况
diagn$hb <- rep(1,time=zj) #高血压
diagn$hb[myda$收缩压>=130 | myda$舒张压>=90] <- 2
diagn$hf <- rep(1,time=zj) #高血脂
diagn$hf[myda$总胆固醇>=6.2 | myda$低密度脂蛋白>=4.1 | myda$甘油三酯>=2.3 | myda$高密度脂蛋白<1] <- 2
diagn$hg <- rep(1,time=zj) #高血糖
diagn$hg[myda$空腹血糖>=7] <- 2
diagn$BMI <- rep(1,time=zj) #肥胖
diagn$BMI[myda$BMI>=28] <- 2
#以上可根据诊断标准变化自行更改
colnames(diagn) <- c("身份证","性别","年龄","年龄段","吸烟情况","高血压","高血脂","糖尿病","肥胖")
diagn_lon <- melt(diagn,id=c("身份证","性别","吸烟情况","年龄","年龄段"))
colnames(diagn_lon) <- c("身份证","性别","吸烟情况","年龄","年龄段","疾病类型","数值")
huanbin <- diagn_lon[diagn_lon$数值==2,]
#jib <- aggregate.data.frame(huanbin$疾病类型,by=list(huanbin$年龄段,huanbin$吸烟情况,huanbin$疾病类型,huanbin$性别),FUN = length)#分组描述
#zjib <- aggregate.data.frame(diagn_lon$疾病类型,by=list(diagn_lon$年龄段,diagn_lon$吸烟情况,diagn_lon$疾病类型,diagn_lon$性别),FUN = length)#分组描述
#山峦图绘制----------------------------------------------------------------------------------------------
pla1 <- ggplot(huanbin, aes(x = `年龄`, y = `疾病类型`,fill=`性别`)) +
  geom_density_ridges(alpha = .7, 
                      rel_min_height = 0.01,
                      scale = 1.2,
                      color = "black",
                      from = 30, 
                      to =90,
                      linetype = 1, 
                      lwd = 0.5) +
  theme_ridges(grid = T) +
  theme_ipsum()+xlab("年龄")+ylab(" ")+ 
  labs(title = "各疾病年龄分布")
#图表美化，选择性执行
pla1 + theme(plot.subtitle = element_text(family = "serif"),
             plot.caption = element_text(family = "mono"),
             axis.title = element_text(family = "serif"),
             axis.text = element_text(family = "mono"),
             plot.title = element_text(family = "serif"))
#---------------------------------------------------------------------------------------------------------
#保存结果
#saveRDS(huanbin,"huanbin_x.RDS")
#saveRDS(diagn,"diagn_x.RDS")
#saveRDS(diagn_lon,"diag_lon.RDS")
#saveRDS(jib,"jib_x.RDS")
#saveRDS(zjib,"zjib_x.RDS")
#saveRDS(pla,"pla.RDS")
#saveRDS(pla1,"pla1.RDS")


#setwd("D:/Desktop/描述结果")
#jib <- readRDS("jib_x.RDS")
#zjib <- readRDS("zjib_x.RDS")
preda <- left_join(zjib,jib,by=c("Group.1","Group.2", "Group.3" ,"Group.4")) #总结表(每种疾病患病率情况)
preda[is.na(preda)] <- 0
preda$hbl <- preda$x.y/preda$x.x
saveRDS(preda,"preda.RDS")
z_age <- dcast(preda,Group.1~Group.3,value.var = "x.x",sum)
h_age <- dcast(preda,Group.1~Group.3,value.var = "x.y",sum)
l_age <- cbind(h_age[,1],h_age[,-1]/z_age[,-1])
colnames(l_age) <- c( "分组","高血压","高血脂","糖尿病","肥胖")

z_smoke <- dcast(preda,Group.2~Group.3,value.var = "x.x",sum)
h_smoke <- dcast(preda,Group.2~Group.3,value.var = "x.y",sum)
l_smoke <- cbind(h_smoke[,1],h_smoke[,-1]/z_smoke[,-1])
colnames(l_smoke) <- c( "分组","高血压","高血脂","糖尿病","肥胖")

z_sex <- dcast(preda,Group.4~Group.3,value.var = "x.x",sum)
h_sex <- dcast(preda,Group.4~Group.3,value.var = "x.y",sum)
l_sex <- cbind(h_sex[,1],h_sex[,-1]/z_sex[,-1])
colnames(l_sex) <- c( "分组","高血压","高血脂","糖尿病","肥胖")

saveRDS(z_age,"z_age.RDS")
saveRDS(h_age,"h_age.RDS")
saveRDS(l_age,"l_age.RDS")#患病率（分组）可根据实际情况扩充


saveRDS(z_smoke,"z_smoke.RDS")
saveRDS(h_smoke,"h_smoke.RDS")
saveRDS(l_smoke,"l_smoke.RDS")

saveRDS(z_sex,"z_sex.RDS")
saveRDS(h_sex,"h_sex.RDS")
saveRDS(l_sex,"l_sex.RDS")

#雷达图和柱状图------------------------------------------------------------------------------
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)
library(reshape2)
l_age <- readRDS("D:/Desktop/描述结果/l_age.RDS")
l_age <- l_age*100
l_age[,1] <- c("中年人","老年人")
lad <- ggradar(
  l_age,
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1,
  group.point.size = 3,
  #group.colours = c("#ffd200", "#304156"),
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom"
)
l_age_long <- melt(l_age)
bar <- ggplot(l_age_long, aes(fill=`分组`, x=variable, y=value)) + 
  geom_bar(position="dodge", stat="identity")
library(ggpubr)
zhh <- ggarrange(lad,bar)
#--------------------------------------------------------------------------------------------

#桑基图（不建议尝试！！！）------------------------------------------------------------------
diaa <- readRDS("D:/Desktop/描述结果/diagn_x.RDS")
View(diaa)
write.xlsx(diaa,"diaa1.xlsx")
nodess <- read.xlsx("diaa1.xlsx",sheet = 2)
head(raw_link)
cnaa <- colnames(raw_link)
link_67 <- dcast(raw_link,raw_link[,6]~raw_link[,7],value.var = cnaa[6])
link_67 <- melt(link_67,id="raw_link[, 6]")
colnames(link_67) <- c("source","target","value")
link_67
link_final <- rbind(link_12,link_23,link_34,link_45,link_56,link_67)
write.xlsx(link_final,"link_final.xlsx")
link_final <- read.xlsx("link_final.xlsx")
link_final$target <- as.integer(link_final$target)
link_final$source <- link_final$source-1
link_final$target <- link_final$target-1
link_final$type <- c(1:24)
#length(raw_link$性别[raw_link$性别==1])
#sta <- data.frame(source = 0,target = 1,value = 1852)
#link_final <- rbind(sta,link_final)


# 节点分组的情况下：
sankeyNetwork(Links = link_final, Nodes =nodess, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'node',
              units = 'Twh', fontSize = 28, nodeWidth = 45,nodePadding = 30) 

# 节点分组的情况下：
sankeyNetwork(Links = energy$links, Nodes =energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'TWh', fontSize = 12, nodeWidth = 30)

link_final$type <- sub(' .*','',
                       nodess[link_final$source + 1, 'node'])

save.image(file = "env_data_describe.RData") #保存结果







