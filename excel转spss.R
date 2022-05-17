##将Excel的xlsx表格转化成适用于SPSS处理的sav文件
#infile:输入文件路径+文件名（文件在当前工作目录，路径可省略）
#se:sheetx
#type:表格类型——1：一维表 | 2：二维表(交叉表)
#outfile:输出文件路径+文件名（路径可省略，文件保存在当前工作目录）
xlsx_sav <- function(infile,se,type,outfile){
  library(openxlsx)
  library(reshape2)
  library(haven)
  ifelse(type==1,
         mt_learn <- melt(read.xlsx(infile,sheet = se)),
         mt_learn <- melt(read.xlsx(infile,sheet = se),id=1)
  )
  mt_learn <- mt_learn[!is.na(mt_learn$value),]
  mt_learn[,1] <- as.factor(mt_learn[,1])
  write_sav(mt_learn,outfile)
  rm(mt_learn)
}
#运行前没安装依赖包请自行安装，运行完该段代码后直接使用xlsx_sav函数
#如处理更复杂形式的excel表格可在函数源码上进行修改