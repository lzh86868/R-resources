library(ncdf4)
library(raster)
file_nc<-file.choose()#选择要读取的nc数据
ncfile=nc_open(file_nc,write=T)#打开文件
varname<-names(ncfile$var)[2]#变量名
print(ncfile)#查看详细信息
TMP2<-ncvar_get(nc=ncfile,varid=varname)#提取变量数据
TMP2 <- TMP2-273.15#对变量数据处理
longitude<-ncvar_get(nc=ncfile,varid="longitude")#精度
latidue<-ncvar_get(nc=ncfile,varid="latitude")#纬度
nc_close(ncfile)#关闭文件
#组织新文件---------------------------------------------------------------------
lon<-ncdim_def("lon","degrees_east",vals=longitude)
lat<-ncdim_def("lat","degrees_north",vals=latidue)
mv<--9999
TAIR<-ncvar_def(name='TAIR',units='C',dim=list(lon,lat),missval=mv,prec='float')#定义新变量
ncnew<-nc_create(filename='nfd.nc',vars=list(TAIR))#创建新nc文件
ncvar_put(nc=ncnew,varid=TAIR,vals=TMP2)#往变量写入值
nc_close(ncnew)#关闭新文件
#简单可视化
nc2raster = brick(file.choose(),varname = "TAIR")
levelplot(nc2raster)
