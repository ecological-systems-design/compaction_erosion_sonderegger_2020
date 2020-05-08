#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')


r1 = raster('./ls-tiles/ls-factor_1e2_mean_srtm_250m.tif')
r2 = raster('./ls-tiles/ls-factor_1e2_mean_gmted_250m.tif')

r1c = crop(r1, extent(extent(r1)[1], extent(r1)[2], extent(r1)[3], extent(r2)[3]))

rm(r1)
gc()

time.taken = Sys.time() - start.time
print('Cropping:'); print(time.taken)

raster_list = c(r1c, r2)

raster_mos.mosaicargs = raster_list
raster_mos.mosaicargs$fun = mean
mos = do.call(mosaic, raster_mos.mosaicargs)

time.taken = Sys.time() - start.time
print('Mosaicing:'); print(time.taken)

writeRaster(mos, './rasters/ls-factor_1e2_mean_250m.tif', format = 'GTiff', overwrite = TRUE, 
            datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))

rm(r1c, r2, mos)
gc()

time.taken = Sys.time() - start.time
print('Mean raster:'); print(time.taken)


# r1 = raster('./ls-tiles/ls-factor_1e2_max_srtm_250m.tif')
# r2 = raster('./ls-tiles/ls-factor_1e2_mean_gmted_250m.tif')
# 
# r1c = crop(r1, extent(extent(r1)[1], extent(r1)[2], extent(r1)[3], extent(r2)[3]))
# 
# rm(r1)
# gc()
# 
# time.taken = Sys.time() - start.time
# print('Cropping:'); print(time.taken)
# 
# raster_list = c(r1c, r2)
# 
# raster_mos.mosaicargs = raster_list
# raster_mos.mosaicargs$fun = max
# mos = do.call(mosaic, raster_mos.mosaicargs)
# 
# time.taken = Sys.time() - start.time
# print('Mosaicing:'); print(time.taken)
# 
# writeRaster(mos, './rasters/ls-factor_1e2_max_250m.tif', format = 'GTiff', overwrite = TRUE, 
#             datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))
# 
# rm(r1c, r2, mos)
# gc()
# 
# time.taken = Sys.time() - start.time
# print('Max raster:'); print(time.taken)
# 
# 
# r1 = raster('./ls-tiles/ls-factor_1e2_min_srtm_250m.tif')
# r2 = raster('./ls-tiles/ls-factor_1e2_mean_gmted_250m.tif')
# 
# r1c = crop(r1, extent(extent(r1)[1], extent(r1)[2], extent(r1)[3], extent(r2)[3]))
# 
# rm(r1)
# gc()
# 
# time.taken = Sys.time() - start.time
# print('Cropping:'); print(time.taken)
# 
# raster_list = c(r1c, r2)
# 
# raster_mos.mosaicargs = raster_list
# raster_mos.mosaicargs$fun = min
# mos = do.call(mosaic, raster_mos.mosaicargs)
# 
# time.taken = Sys.time() - start.time
# print('Mosaicing:'); print(time.taken)
# 
# writeRaster(mos, './rasters/ls-factor_1e2_min_250m.tif', format = 'GTiff', overwrite = TRUE, 
#             datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))
# 
# rm(r1c, r2, mos)
# gc()
# 
# time.taken = Sys.time() - start.time
# print('Min raster:'); print(time.taken)

