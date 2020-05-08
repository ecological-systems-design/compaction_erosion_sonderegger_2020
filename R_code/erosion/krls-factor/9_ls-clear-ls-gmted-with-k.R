#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')


r1 = raster('./ls-tiles/ls-factor_1e2_mean_gmted_250m_raw.tif')
r2 = raster('./rasters/k-factor_1e5_250m.tif')

r2c = crop(r2, extent(r1))
rm(r2)
gc()

r1[is.na(r2c)] = NA

writeRaster(r1, paste('./ls-tiles/ls-factor_1e2_mean_gmted_250m.tif'), format = 'GTiff', overwrite = TRUE, 
            datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))

rm(r1, r2c)
gc()

time.taken = Sys.time() - start.time
print('Clean GMTED:'); print(time.taken)
