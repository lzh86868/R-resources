library(raster)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(maptools)
library(dismo)
library(fields)

bound<-readOGR(file.choose())
plot(bound,col="grey")

dsp <- readOGR(file.choose())
plot(dsp,col="grey")

#layout(matrix(1:4, ncol=2, byrow=TRUE))
#res<-c(20,100,500,1000)
#上述代码测试栅格合适分辨率时开启
res<-c(1000)
for(r in res){
blank_raster<-raster(nrow=r,ncol=r,extent(bound))
values(blank_raster)<-1
bound_raster<-rasterize(bound,blank_raster)
bound_raster[!(is.na(bound_raster))] <- 1
plot(bound_raster,main=paste("Res: ",r,"*",r))
plot(bound,add=T)
}

v <- voronoi(dsp)
v1<-intersect(v,bound)
#spplot(v1, "C", col.regions=rev(get_col_regions()))
vr <- rasterize(v1,bound_raster,"C")
spplot(vr)

gs<-gstat(formula=C~1,location=dsp,nmax=5,set=list(idp=0))
nn<-interpolate(bound_raster,gs)
nnmask<-mask(nn,bound_raster)##掩膜提取
spplot(nnmask)

gs <- gstat(formula=C~1, locations=dsp)
idw <- interpolate(bound_raster, gs)
idwmask<-mask(idw,bound_raster)
spplot(idwmask)

#求变异函数,绘制半变异图
v<- variogram(log(C) ~ 1, data =dsp)
plot(v,plot.number=T)

#选择合适的拟合函数
show.vgms()

#拟合模型
v.fit<-fit.variogram(v,model=vgm(1,"Lin",0))
plot(v,v.fit)

#点位数值预测
Grid<-as(bound_raster,"SpatialGridDataFrame")#首先现将边界栅格转成空间网格
kri<-krige(formula=C~1,model=v.fit,locations=dsp,newdata=Grid,nmax=12, nmin=10)#location为已知点的坐标；newdata为需要插值的点的位置；nmax和nmin分别代表最多和最少搜索点的个数
spplot(kri["var1.pred"])

m <- Tps(coordinates(dsp), dsp$C)
tps <- interpolate(bound_raster, m)
tps <- mask(tps, bound_raster)
spplot(tps)
