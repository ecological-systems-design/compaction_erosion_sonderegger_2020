#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

#setwd('/home/sothomas/R_code/soil')
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

kls_file = './erosion/KRLS-factor/rasters/kls-factor_1e7_median_1km.tif'
r_file = 'C:/Users/Sonderegger/Documents/GIS_data/ESDAC/GlobalR/GlobalR_NoPol.tif'

kls = raster(kls_file)
r = raster(r_file)

krls = kls * r

rm(kls, r)
gc()

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

krls = krls / 1e7

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

krls = extend(krls, extent(-180, 180, -56, 84))

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

writeRaster(krls, './erosion/KRLS-factor/rasters/krls-factor_median_1km.tif', format = 'GTiff',
            options = c('COMPRESS=DEFLATE'), overwrite = TRUE)

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)


# kls_file = './kls-factor_1e7_max_1km.tif'
# r_file = 'C:/Users/Sonderegger/Documents/GIS_data/ESDAC/GlobalR/GlobalR_NoPol.tif'
# 
# kls = raster(kls_file)
# r = raster(r_file)
# 
# krls = kls * r
# 
# rm(kls, r)
# gc()
# 
# time.taken = Sys.time() - start.time
# print('Script time:'); print(time.taken)
# 
# krls = krls / 1e7
# 
# time.taken = Sys.time() - start.time
# print('Script time:'); print(time.taken)
# 
# writeRaster(krls, './erosion/KRLS-factor/rasters/krls-factor_max_1km.tif', format = 'GTiff',
#             options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
# 
# time.taken = Sys.time() - start.time
# print('Script time:'); print(time.taken)
# 
# 
# kls_file = './kls-factor_1e7_min_1km.tif'
# r_file = 'C:/Users/Sonderegger/Documents/GIS_data/ESDAC/GlobalR/GlobalR_NoPol.tif'
# 
# kls = raster(kls_file)
# r = raster(r_file)
# 
# krls = kls * r
# 
# rm(kls, r)
# gc()
# 
# time.taken = Sys.time() - start.time
# print('Script time:'); print(time.taken)
# 
# krls = krls / 1e7
# 
# time.taken = Sys.time() - start.time
# print('Script time:'); print(time.taken)
# 
# writeRaster(krls, './erosion/KRLS-factor/rasters/krls-factor_min_1km.tif', format = 'GTiff',
#             options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
# 
# time.taken = Sys.time() - start.time
# print('Script time:'); print(time.taken)
