#nc数据提取长表格版
library(rasterVis)
library(raster)
library(ncdf4)
library(reshape2)
file_nc <- file.choose()
ncfile = ncdf4nc_open(file_nc,write = T)
varname <- names(ncfile$var)
ncd <- brick(file_nc,values=T,varname=varname)
lat <- ncfile$dim$LAT$vals
lon <- ncfile$dim$LON$vals
df <- data.frame(lon=rep(lon,each=length(lat)),lat=rep(lat,time=length(lon)))
dfsp <-  SpatialPoints(df)
pp <- raster::extract(ncd,dfsp)
ncdf <- cbind(df,pp)
ncdfcr <- dcast(ncdf,lat~lon)
ncdfcr <- ncfdcr*100
dim(ncdfcr)
write.xlsx(ncfdcr,"nc1.xlsx")
nc_close(ncfile)
nc2raster = stack(file_nc,varname = varname)
levelplot(nc2raster)




