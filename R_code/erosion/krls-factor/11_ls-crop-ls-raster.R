#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')

ls_file = './ls-tiles/ls-factor_1e2_mean_250m.tif'

ls = raster(ls_file)


crop_extent = extent(extent(ls)[1], extent(ls)[2], -56, extent(ls)[4])

ls = crop(ls, crop_extent)

time.taken = Sys.time() - start.time
print('Cropping LS-raster:'); print(time.taken)

writeRaster(ls, './ls-tiles/ls-factor_1e2_mean_250m_cropped.tif', format = 'GTiff',
            datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)

time.taken = Sys.time() - start.time
print('Writing LS-raster:'); print(time.taken)


# rm(ls)
# gc()
# 
# ls_file = './ls-tiles/ls-factor_1e2_max_250m.tif'
# ls = raster(ls_file)
# 
# ls = crop(ls, crop_extent)
# 
# time.taken = Sys.time() - start.time
# print('Cropping LS-raster:'); print(time.taken)
# 
# writeRaster(ls, './ls-tiles/ls-factor_1e2_max_250m_cropped.tif', format = 'GTiff',
#             datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
# 
# time.taken = Sys.time() - start.time
# print('Writing LS-raster:'); print(time.taken)
# 
# 
# rm(ls)
# gc()
# 
# ls_file = './ls-tiles/ls-factor_1e2_min_250m.tif'
# ls = raster(ls_file)
# 
# ls = crop(ls, crop_extent)
# 
# time.taken = Sys.time() - start.time
# print('Cropping LS-raster:'); print(time.taken)
# 
# writeRaster(ls, './ls-tiles/ls-factor_1e2_min_250m_cropped.tif', format = 'GTiff',
#             datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
# 
# time.taken = Sys.time() - start.time
# print('Writing LS-raster:'); print(time.taken)
