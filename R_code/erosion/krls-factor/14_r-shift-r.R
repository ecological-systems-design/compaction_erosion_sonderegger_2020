#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

#setwd('/home/sothomas/R_code/erosion')
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/erosion')


r_file = 'C:/Users/Sonderegger/Documents/GIS_data/ESDAC/GlobalR/GlobalR_NoPol.tif'
r = raster(r_file)
rs = shift(r, -origin(r)[1], -origin(r)[2])

rs

writeRaster(rs, './rasters/r_shifted.tif', format = 'GTiff', overwrite = TRUE)


