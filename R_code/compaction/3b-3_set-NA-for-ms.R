#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)


# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


ts = raster('./compaction/characterization_factors/mx_ts_yearly.tif')
ms = raster('./compaction/characterization_factors/mx_ms_yearly.tif')

r = ms
r[is.na(ts)] = NA

plot(r)

#writeRaster(r, paste0('./compaction/characterization_factors/mx_ms_yearly.tif'), format = 'GTiff', overwrite = TRUE)
