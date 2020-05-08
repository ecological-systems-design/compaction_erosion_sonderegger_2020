#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')

r_file = './ls-tiles/ls-factor_mean_gmted_250m_raw.tif'
r = raster(r_file)

r100 = r * 100

r100s = shift(r100, -origin(r)[1], -origin(r)[2])

writeRaster(r100s, './ls-tiles/ls-factor_1e2_mean_gmted_250m_raw.tif', format = 'GTiff', 
            datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
