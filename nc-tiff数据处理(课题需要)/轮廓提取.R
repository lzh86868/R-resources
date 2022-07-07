library(sf)
library(raster)
map_shape <- read_sf(file.choose())#读取shp文件
nc2raster = brick(file.choose(),varname = varname)#tiff文件可以不用varname
map_crop<- crop(nc2raster, map_shape)
map_mask<- mask(map_crop,map_shape)
writeRaster(map_mask,filename = "test.nc",format="CDF",overwrite=T)
varname<-names(ncfile$var)[2]
map_mask$layer
levelplot(map_mask)