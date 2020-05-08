#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')

in_folder = './k-tiles/tiles/'

strings = c('_-180', '_-150', '_-120', '_-90', '_-60', '_-30', '_0', '_30', '_60', '_90', '_120', '_150')


for (str in strings) {
  
  print(str)
  
  loopstart.time = Sys.time()

  raster_files = list.files(path = in_folder, pattern = str)
  
  print(paste('Number of files:', length(raster_files)))
  
  raster_list = list()
  
  for (i in 1:length(raster_files)) {
    r = raster(paste(in_folder, raster_files[i], sep = ''))
    raster_list = c(raster_list, r)
  }
  
  time.taken = Sys.time() - loopstart.time
  print('Collect rasters:'); print(time.taken)
  
  raster_mos.mosaicargs = raster_list
  raster_mos.mosaicargs$fun = mean
  mos = do.call(mosaic, raster_mos.mosaicargs)
  
  time.taken = Sys.time() - loopstart.time
  print('Mosaicing:'); print(time.taken)
  
  k = shift(mos, -origin(mos)[1], 0)
  
  ks = shift(k, 0, -origin(k)[2])
  
  ksrb = resample(k, ks, method='bilinear')
  
  k_int = ksrb * 1e5
  
  time.taken = Sys.time() - loopstart.time
  print('Multiplication:'); print(time.taken)
  
  k_int = round(k_int, 0)
  
  time.taken = Sys.time() - loopstart.time
  print('Rounding:'); print(time.taken)
  
  writeRaster(k_int, paste('./k-tiles/tiles_aggregated/k_1e5', str, '_250m.tif', sep = ''), format = 'GTiff', overwrite = TRUE,
              datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))
  
  time.taken = Sys.time() - loopstart.time
  print('Loop time:'); print(time.taken)
}

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)
