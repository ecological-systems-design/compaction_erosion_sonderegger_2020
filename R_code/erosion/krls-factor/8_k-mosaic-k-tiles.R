#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')


combine_tiles_folder = function(in_folder, out_folder, outfile_name) {
  
  loopstart.time = Sys.time()
  
  raster_files = list.files(path = in_folder)#, pattern = '')
  
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
  
  writeRaster(mos, paste(out_folder, outfile_name, sep = ''), format = 'GTiff', overwrite = TRUE, 
              datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))
  
  time.taken = Sys.time() - loopstart.time
  print('Loop time:'); print(time.taken)
  
}

combine_tiles_list = function(raster_list, out_folder, outfile_name) {
  
  loopstart.time = Sys.time()
  
  raster_mos.mosaicargs = raster_list
  raster_mos.mosaicargs$fun = mean
  mos = do.call(mosaic, raster_mos.mosaicargs)
  
  time.taken = Sys.time() - loopstart.time
  print('Mosaicing:'); print(time.taken)
  
  writeRaster(mos, paste(out_folder, outfile_name, sep = ''), format = 'GTiff', overwrite = TRUE, 
              datatype = 'INT2U', options = c('COMPRESS=DEFLATE'))
  
  time.taken = Sys.time() - loopstart.time
  print('Loop time:'); print(time.taken)
  
}


in_folder = './k-tiles/tiles aggregated/'
out_folder = './rasters/'
outfile_name = 'k-factor_1e5_250m.tif'
combine_tiles_folder(in_folder, out_folder, outfile_name)

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

