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


clay_path = './compaction/data/Stoessel_2018/'
swc_path = './compaction/data/Stoessel_2018/soil_water_content_prepared/'


#########################################

# Clay content of top soil

clay = raster(paste0(clay_path, 'CLYPPT_ts_resampled.tif'))
clay = shift(clay, 0, -origin(clay)[2])
crs(clay) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '

writeRaster(clay, './compaction/data/prepared/clay_topsoil.tif', format = 'GTiff',
            datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)


#########################################

# Soil water content including irrigation

#clay = raster('./compaction/data/prepared/clay_topsoil.tif')

files = list.files(swc_path, pattern='_swc_corr(.*)tif$')

# Register CoreCluster
cl = makeCluster(12)
registerDoParallel(cl)

results = foreach(i=1:12, .packages=c('rgdal', 'raster')) %dopar% {

  r = raster(paste0(swc_path, files[[i]]))
  r = shift(r, 0, -origin(r)[2])
  r[is.na(clay)] = NA

  if (i < 10) {
    j = paste0('0', as.character(i))
  } else {
    j = as.character(i)
  }
  
  writeRaster(r, paste0('./compaction/data/prepared/swc_irrigated/', j,'_swc_irrigated.tif'), format = 'GTiff',
              datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
}

stopCluster(cl)

