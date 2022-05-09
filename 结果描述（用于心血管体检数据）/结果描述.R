#加载包
library(openxlsx)
library(reshape2)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(dplyr)
dec_health <- readRDS("处理完成数据.RDS")
dec_health$性别 <- factor(dec_health$性别,levels = c(1,2),labels = c("男","女"))#需要转化成分类变量的指标
dec_health$吸烟情况 <- factor(dec_health$吸烟情况,levels = c(1,2),labels = c("不吸烟","吸烟"))
myda <- dec_health#数据清洗完成的数据集
aggregate.data.frame(myda[,-c(1:4)],by=list(myda$性别,myda$吸烟情况),FUN = sd)#分组描述
#myda$性别 <- factor(myda$性别,levels = c(1,2),labels = c("男","女"))#读取Excel文件需要，SPSS文件不需要
#myda$吸烟情况 <- factor(myda$吸烟情况,levels = c(0,1),labels = c("不吸烟","吸烟"))#同上
mydalo <- melt(myda,id=c("性别","吸烟情况","年龄"))#根据自己数据决定，需要了解reshape2包详细使用方法

#疾病诊断-----------------------------------------------------------------------------------------------------------
diagn <- data.frame(sex=myda$性别 )#患病情况
zj <- length(myda[,1])
diagn$sex <- myda$性别
diagn$age <- myda$年龄
diagn$agepe <- cut(myda$年龄,2)
diagn$smoke <- myda$吸烟情况
diagn$hb <- rep(1,time=zj) #高血压
diagn$hb[myda$收缩压>=130 | myda$舒张压>=90] <- 2
diagn$hf <- rep(1,time=zj) #高血脂
diagn$hf[myda$总胆固醇>=6.2 | myda$LDL>=4.1 | myda$甘油三酯>=2.3 | myda$HDL<1] <- 2
diagn$hg <- rep(1,time=zj) #高血糖
diagn$hg[myda$空腹血糖>=7] <- 2
diagn$BMI <- rep(1,time=zj) #肥胖
diagn$BMI[myda$体质指数>=28] <- 2
#以上可根据诊断标准变化自行更改
colnames(diagn) <- c("性别","年龄","年龄段","吸烟情况","高血压","高血脂","糖尿病","肥胖")#设置指标名称，方便作图
diagn_lon <- melt(diagn,id=c("性别","吸烟情况","年龄","年龄段"))#数据变形，自行查看reshape2包使用说明
colnames(diagn_lon) <- c("性别","吸烟情况","年龄","年龄段","疾病类型","数值")
huanbin <- diagn_lon[diagn_lon$数值==2,]
jib <- aggregate.data.frame(huanbin$疾病类型,by=list(huanbin$年龄段,huanbin$吸烟情况,huanbin$疾病类型,huanbin$性别),FUN = length)#分组描述
zjib <- aggregate.data.frame(diagn_lon$疾病类型,by=list(diagn_lon$年龄段,diagn_lon$吸烟情况,diagn_lon$疾病类型,diagn_lon$性别),FUN = length)#分组描述
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
zhh
#--------------------------------------------------------------------------------------------

#桑基图（不建议尝试！！！）------------------------------------------------------------------
diaa <- readRDS("D:/Desktop/描述结果/diagn_x.RDS")
View(diaa)

write.xlsx(diaa,"diaa1.xlsx")
nodess <- read.xlsx("diaa1.xlsx",sheet = 2)
#head(raw_link)
#cnaa <- colnames(raw_link)
#link_67 <- dcast(raw_link,raw_link[,6]~raw_link[,7],value.var = cnaa[6])
#link_67 <- melt(link_67,id="raw_link[, 6]")
#colnames(link_67) <- c("source","target","value")
#link_67
#link_final <- rbind(link_12,link_23,link_34,link_45,link_56,link_67)
#write.xlsx(link_final,"link_final.xlsx")
link_final <- read.xlsx("link_final.xlsx")
#link_final$target <- as.integer(link_final$target)
link_final$source <- link_final$source-1
link_final$target <- link_final$target-1
link_final$type <- c(1:24)
#length(raw_link$性别[raw_link$性别==1])
#sta <- data.frame(source = 0,target = 1,value = 1852)
#link_final <- rbind(sta,link_final)


# 节点分组的情况下：
link_final$type <- sub(' .*','',
                       nodess[link_final$source + 1, 'node'])
sankeyNetwork(Links = link_final, Nodes =nodess, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'node',
              units = 'Twh', fontSize = 28, nodeWidth = 45,nodePadding = 30) 
sankeyNetwork(Links = link_final, Nodes =nodess, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'node',
              units = 'Twh', fontSize = 28, nodeWidth = 45,nodePadding = 30,LinkGroup = "type") 

# 节点分组的情况下：



save.image(file = "描述+图表+图表参考数据.RData") #保存结果

