#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)



# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

start.time = Sys.time()


##############

# Load rasters

ms = raster('./compaction/characterization_factors/mx_ms_yearly.tif')
f = (0.272/400*50) / (0.326/40)
f
bs = ms * f

#writeRaster(bs, './compaction/characterization_factors/mx_bs_yearly.tif',
#            format = 'GTiff', overwrite = TRUE, options = c('COMPRESS=DEFLATE'))
