#install.packages('rgdal')
#install.packages('raster')
#install.packages('doParallel')
#install.packages('foreach')
#install.packages('spatstat')
library(rgdal)
library(raster)
library(doParallel)
library(foreach)
library(spatstat)

start.time = Sys.time()

# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


####################################

# Load files and set paths and names

r = raster('./compaction/characterization_factors/mx_ts_yearly.tif')
#r = raster('./compaction/characterization_factors/mx_ms_yearly.tif')
#r = raster('./erosion/krls-factor/rasters/krls-factor_median_1km.tif')

r_w = raster('./data/prepared/landuse_area_weighting.tif')

out_folder = './agg_tiles_ts/'
#out_folder = './agg_tiles_ms/'
#out_folder = './agg_tiles_krls/'

out_raster_name = 'mx_ts_yearly_10km_weighted_median.tif'
#out_raster_name = 'mx_ms_yearly_10km_weighted_median.tif'
#out_raster_name = 'krls-factor_10km_weighted_median.tif'


# create tile folder
if (!dir.exists(out_folder)) {
  dir.create(out_folder)
}


#########################

# Setup for parallel loop

# Creat log file
if (file.exists('log_aggregation.txt')) {
  file.remove('log_aggregation.txt')
}
file.create('log_aggregation.txt')

# Define how many cores you want to use
UseCores = round(detectCores() / 5 * 4) 

# Register CoreCluster
cl = makeCluster(UseCores)
registerDoParallel(cl)

n_start = 1
n = length(y_mins)


#####################

# Define tile origins

x_mins = seq(60, 170, 10)
y_mins = seq(64, 74, 10)

results = foreach(i=n_start:n, .packages=c('rgdal', 'raster', 'spatstat')) %dopar% {
for (i in n_start:n) {
  y_min = y_mins[[i]]
  for (x_min in x_mins) {
    
    if (file.exists(paste0(out_folder, x_min, '_', y_min, '.tif'))) {
      
      sink('log_aggregation.txt', append=TRUE)
      cat(paste(x_min, y_min, 'file exists!', '\n'))
      sink()
      
    } else {
      x_max = x_min + 10
      y_max = y_min + 10
      
      r_a = crop(r, extent(x_min, x_max, y_min, y_max))
      r_b = crop(r_w, extent(x_min, x_max, y_min, y_max))
      
      if ((extent(r_a) == extent(r_b)) == FALSE) {
        r_b = extend(r_b, r_a, 0)
      }
      
      f = 10
      
      if (all(is.na(getValues(r_a))) == TRUE) {
        
        sink('log_aggregation.txt', append=TRUE)
        cat(paste(x_min, y_min, 'NO_VALUES', '\n'))
        sink()
        
        print(paste(x_min, y_min, 'NO_VALUES'))
        
        r_m = raster(crs=crs(r_a), ext=extent(r_a), resolution=f*res(r_a))
        r_m[] = NA
        writeRaster(r_m, paste0(out_folder, x_min, '_', y_min, '.tif'), format = 'GTiff', overwrite = TRUE)
        
      } else if (max(getValues(r_b), na.rm=TRUE) == 0) {
        
        sink('log_aggregation.txt', append=TRUE)
        cat(paste(x_min, y_min, 'NO_VALUES', '\n'))
        sink()
        
        print(paste(x_min, y_min, 'NO_VALUES'))
        
        r_m = raster(crs=crs(r_a), ext=extent(r_a), resolution=f*res(r_a))
        r_m[] = NA
        writeRaster(r_m, paste0(out_folder, x_min, '_', y_min, '.tif'), format = 'GTiff', overwrite = TRUE)
        
      } else {
        
        sink('log_aggregation.txt', append=TRUE)
        cat(paste(x_min, y_min, 'CALCULATING', '\n'))
        sink()
        
        print(paste(x_min, y_min, 'CALCULATING'))
      
        x_vals = seq(x_min, x_max-f*res(r)[1], f*res(r)[1])
        y_vals = seq(y_min, y_max-f*res(r)[1], f*res(r)[2])
        
        x_v = c()
        y_v = c()
        
        for (ymn in y_vals) {
          for (xmn in x_vals) {
            
            ext = extent(xmn, xmn+f*res(r)[1], ymn, ymn+f*res(r)[2])
            
            a = crop(r_a, ext)
            
            if (all(is.na(getValues(a))) == TRUE) {
              
              med = NA
              
            } else {
              
              b = crop(r_b, ext)
              b[is.na(a)] = 0
              b_v = getValues(b)
              
              if (all(is.na(b_v)) == TRUE) {
                med = NA
              } else if (max(b_v, na.rm=TRUE) == 0) {
                med = 0
              } else {
                tryCatch({
                  med = weighted.median(a,b)
                }, error=function(e){
                  sink('log_aggregation.txt', append=TRUE)
                  cat(paste(x_min, y_min, 'ERROR', xmn, ymn, '\n'))
                  sink()
                })
              }
              
            }
            x_v = c(x_v, med)
          }
          y_v = c(y_v, x_v)
          x_v = c()
        } 
        
        t = Sys.time() - start.time
        
        sink('log_aggregation.txt', append=TRUE)
        cat(paste(x_min, y_min, 'FINISHED', round(t, 3), units(t), '\n'))
        sink()
        
        r_m = raster(crs=crs(r_a), ext=extent(r_a), resolution=f*res(r_a), vals=y_v)
        r_m = flip(r_m, 2)
        writeRaster(r_m, paste0(out_folder, x_min, '_', y_min, '.tif'), format = 'GTiff', overwrite = TRUE)
      }
    }
  }
}

stopCluster(cl)

t = Sys.time() - start.time
print(paste('Total time:', round(t, 3), units(t)))


#############

# Merge tiles

in_pattern = paste0('(.*)tif$')

raster_files = list.files(path=out_folder, pattern=in_pattern)
raster_list = list()

for (i in 1:length(raster_files)) {
  r = raster(paste0(out_folder, raster_files[i]))
  raster_list = c(raster_list, r)
}

raster_mos.mosaicargs = raster_list
raster_mos.mosaicargs$fun = mean
mos = do.call(mosaic, raster_mos.mosaicargs)

writeRaster(mos, paste0('./data/prepared/', out_raster_name), format = 'GTiff', overwrite = TRUE)

# remove tile folder
#unlink(out_folder, recursive=TRUE)

time.taken = Sys.time() - start.time
print(paste0('Time: ')); print(time.taken)

