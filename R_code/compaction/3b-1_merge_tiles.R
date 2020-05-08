#install.packages('rgdal')
#install.packages('raster')
#install.packages('doParallel')
#install.packages('foreach')
library(rgdal)
library(raster)
library(doParallel)
library(foreach)

# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

start.time = Sys.time()


############################################################

# INPUT

soil_layer = 'ts'  # 'ts', 'ms' (top or mid, bottom is calculated from mid)
param_name = 'mx'  # 'mx' or 'sdx'
out_folder = './compaction/characterization_factors/'


###################

# For each month...

for (month in 1:12) {

  target_folder_name = month

  ############################################################
  
  main_folder = paste0('./compaction/characterization_factors_', soil_layer, '/')
  target_folder = paste0(main_folder, target_folder_name, '/')
  
  if (!dir.exists(paste0(main_folder, month, '_x_columns/'))) {
    dir.create(paste0(main_folder, month, '_x_columns/'))
  }
  col_folder = paste0(main_folder, month, '_x_columns/')
  
  if (!dir.exists(paste0(main_folder, month, '_x_col_groups/'))) {
    dir.create(paste0(main_folder, month, '_x_col_groups/'))
  }
  col_group_folder = paste0(main_folder, month, '_x_col_groups/')
  
  
  ##########################################
  
  # Define how many cores you want to use
  UseCores = round(detectCores() / 5 * 4) 
  
  # Register CoreCluster
  cl = makeCluster(UseCores)
  registerDoParallel(cl)
  
  # Calculate how many tiles in x-direction
  x_degrees = 360
  tile_degrees = 2
  x_tiles = x_degrees/tile_degrees
  n = x_tiles
  
  results = foreach(i=1:n, .packages=c('rgdal', 'raster')) %dopar% {
  #for (i in 1:n) {
    
    sink('log_merge.txt', append=TRUE)
    cat(paste0('Starting x-tile ', i, ' of ', n, '\n'))
    sink()
    
    in_pattern = paste0(param_name, '_', i, '_', '(.*)tif$')
    
    raster_files = list.files(path=target_folder, pattern=in_pattern)
    
    raster_list = list()
    
    for (j in 1:length(raster_files)) {
      r = raster(paste0(target_folder, raster_files[j]))
      raster_list = c(raster_list, r)
    }
    
    raster_mos.mosaicargs = raster_list
    raster_mos.mosaicargs$fun = mean
    mos = do.call(mosaic, raster_mos.mosaicargs)
    
    writeRaster(mos, paste0(col_folder, i), format = 'GTiff', overwrite = TRUE)
    
    gc()
  }
  
  stopCluster(cl)
  
  time.taken = Sys.time() - start.time
  print(paste0('Columns tile ', month, ' time: ')); print(time.taken)
  
  
  ##########################################
  
  # Define how many cores you want to use
  UseCores = round(detectCores() / 5 * 4) 
  
  # Register CoreCluster
  cl = makeCluster(UseCores)
  registerDoParallel(cl)
  
  col_group = 10
  n = x_tiles / col_group
  
  results = foreach(i=1:n, .packages=c('rgdal', 'raster')) %dopar% {
  #for (i in 1:n) {  
    
    sink('log_merge.txt', append=TRUE)
    cat(paste0('Starting x-tile-group ', i, ' of ', n, '\n'))
    sink()
    
    first = (i-1)*col_group+1
  
    raster_list = list()
    
    for (j in first:(first+9)) {
  
      r = raster(paste0(col_folder, j, '.tif'))
      raster_list = c(raster_list, r)
    }
    
    raster_mos.mosaicargs = raster_list
    raster_mos.mosaicargs$fun = mean
    mos = do.call(mosaic, raster_mos.mosaicargs)
    
    writeRaster(mos, paste0(col_group_folder, i), format = 'GTiff',
                overwrite = TRUE, options = c('COMPRESS=DEFLATE'))
    
    gc()
  }
  
  stopCluster(cl)
  
  time.taken = Sys.time() - start.time
  print(paste0('Column groups tile ', month, ' time: ')); print(time.taken)
  
}
  
  
##########################################

# For each month...

# Register CoreCluster
cl = makeCluster(12)
registerDoParallel(cl)

results = foreach(i=1:12, .packages=c('rgdal', 'raster')) %dopar% {
  
  col_folder = paste0(main_folder, i, '_x_columns/')
  col_group_folder = paste0(main_folder, i, '_x_col_groups/')
    
  raster_files = list.files(path=col_group_folder)
  
  raster_list = list()
  
  for (j in 1:length(raster_files)) {
    r = raster(paste0(col_group_folder, raster_files[j]))
    raster_list = c(raster_list, r)
  }
  
  raster_mos.mosaicargs = raster_list
  raster_mos.mosaicargs$fun = mean
  mos = do.call(mosaic, raster_mos.mosaicargs)
  
  if (i < 10) {
    j = paste0('0', as.character(i))
  } else {
    j = as.character(i)
  }
  
  writeRaster(mos, paste0(out_folder, param_name, '_', soil_layer, '_', j),
              format = 'GTiff', overwrite = TRUE, options = c('COMPRESS=DEFLATE'))
  
  unlink(col_folder, recursive=TRUE)
  unlink(col_group_folder, recursive=TRUE)

}

stopCluster(cl)


time.taken = Sys.time() - start.time
print('Total time:'); print(time.taken)

