#整理文件名----------------------------------------------------------------------------------------------
library(stringr)
library(openxlsx)
library(dplyr)
dir <- choose.dir()
fileneed <- list.files(dir,full.names = T,pattern = "[0-9].(nc)$",recursive = T)
fileout <- setdiff(list.files(dir,full.names = T,recursive = T),fileneed)
VAR <- c("TMP","SHU","WIN","PRS","PRE","SSRA","GST")
need_file <- data.frame(date=c(),var=c(),filename=c())
for (i in VAR) {
  filename=grep(i,fileneed,value = T)
  date <- as.integer(str_sub(filename,-13,-4))
  var=rep(i,each=length(date))
  tempdf <- data.frame(date,var,filename)
  tempdf <- tempdf[order(tempdf[,1]),]
  need_file <- rbind(need_file,tempdf)
}
#write.xlsx(need_file,paste0("filename(",format(Sys.time(), "%H-%M-%S"),").xlsx"))
#检查缺失文件-----------------------------------------------------------------------------------------
from <- as.Date(as.character(min(need_file$date)),format ="%Y%m%d")
to <- as.Date(as.character(max(need_file$date)),format ="%Y%m%d")
lendate <- seq.Date(from,to,by="day")
lendate <- as.character.Date(lendate,format="%Y%m%d")
lenhor <- rep(0:23,time=length(lendate))
lendate <- rep(lendate,each=24)
checkid <- as.integer(lendate)*100+lenhor
checkname <- data.frame(id=paste0(rep(checkid,time=length(VAR)),rep(VAR,each=length(checkid))),
                        check_name=paste0(rep(checkid,time=length(VAR)),rep(VAR,each=length(checkid))))
obsname <- data.frame(id=paste0(need_file$date,need_file$var),
                      obs_name=paste0(need_file$date,need_file$var))
check_result <- left_join(checkname,obsname,by="id")[,-1]
write.xlsx(list(check_result,fileout),paste0("CheckResult(",format(Sys.time(), "%H-%M-%S"),").xlsx"))
#-----------------------------------------------------------------------------------------------------





