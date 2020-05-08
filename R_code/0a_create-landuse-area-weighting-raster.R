#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

#lu = raster('./data/landuse/consensus_full_class_7.tif')
#lu = raster('./data/landuse/Hybrid_10042015v9.img')
lu = raster('./data/landuse/cropland.tif')

origin(lu)

lu = shift(lu, -origin(lu)[1], -origin(lu)[2])

origin(lu)

v = getValues(lu)

rx = raster(nrows=16666, ncols=43200, xmn=-180, xmx=180, ymn=-55.76667, ymx=83.11667)

ro = setValues(rx, v)

#writeRaster(ro, './data/landuse/cropland_new.tif', format = 'GTiff', overwrite = TRUE)

##############################################

#lu = raster('./data/landuse/cropland_new.tif')

ar = area(lu)

a = getValues(ar)

x = v * a

rx = raster(nrows=16666, ncols=43200, xmn=-180, xmx=180, ymn=-55.76667, ymx=83.11667)

ro = setValues(rx, x)

writeRaster(ro, './data/prepared/landuse_area_weighting.tif', format = 'GTiff', overwrite = TRUE)

