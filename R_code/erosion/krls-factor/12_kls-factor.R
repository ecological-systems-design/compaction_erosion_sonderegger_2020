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


##################################################

# Load rasters and calculate KLS-factor

k = raster('./erosion/krls-factor/rasters/k-factor_1e5_250m.tif')
ls = raster('./erosion/krls-factor/rasters/ls-factor_1e2_mean_250m.tif')

kls = overlay(k, ls, fun=function(x,y) {return(x*y)})

rm(k, ls)
gc()

writeRaster(kls, './erosion/KRLS-factor/rasters/kls-factor_250m.tif',
            format = 'GTiff', overwrite = TRUE)

#kls = raster('./erosion/krls-factor/rasters/kls-factor_250m.tif')

            
##################################################

# Cut raster in tiles for parallel aggregation to 1 km (median value)

# Get origins of raster tiles

# Enter x-, y-, and tile-width in degrees
x_degrees = 360
y_degrees = 140
tile_degrees = 2

# Calculate how many tiles in x- and y-direction
x_tiles = x_degrees/tile_degrees
y_tiles = y_degrees/tile_degrees

origins = list()

i = 1

for (col in 1:x_tiles) {
  for (row in 1:y_tiles) {
    
    origins[[i]] = c(col, row)
    i = i + 1
  }
}


# Get a list of results from foreach parallel loop

# Define how many cores you want to use
UseCores = round(detectCores() / 5 * 4) 

# Register CoreCluster
cl = makeCluster(UseCores)
registerDoParallel(cl)

if (file.exists('log_tiles.txt')) {
  file.remove('log_tiles.txt')
}
file.create('log_tiles.txt')


n_start = 1
n = length(origins)

results = foreach(i=n_start:n, .packages=c('rgdal', 'raster')) %dopar% {
  #for (i in n_start:n) {
  
  col = origins[[i]][1]
  row = origins[[i]][2]
  cn = (col-1)*tile_degrees-180
  rn = (row-1)*tile_degrees-56
  ext = extent(cn, cn+tile_degrees, rn, rn+tile_degrees)
  
  sink('log_tiles.txt', append=TRUE)
  cat(paste0('Starting iteration ', i, ' of ', n, ', tile ', col, '-', row, '\n'))
  sink()
  
  # Crop rasters
  klsc = crop(kls, ext)
  if (all(is.na(getValues(klsc))) == FALSE) {

    r = aggregate(klsc, 4, median)
    
    writeRaster(r, paste0('./erosion/KRLS-factor/kls-tiles/kls_median_', col, '_', row, '.tif'),
                format = 'GTiff', overwrite = TRUE)
    rm(r); gc()

    "r = aggregate(kls, 4, mean)
    
    writeRaster(r, paste0('./erosion/KRLS-factor/kls-tiles/kls_mean_', col, '_', row, '.tif'),
                format = 'GTiff', overwrite = TRUE)
    rm(r); gc()

    r = aggregate(kls, 4, min)
    
    writeRaster(r, paste0('./erosion/KRLS-factor/kls-tiles/kls_min_', col, '_', row, '.tif'),
                format = 'GTiff', overwrite = TRUE)
    rm(r); gc()

    r = aggregate(kls, 4, max)
    
    writeRaster(r, paste0('./erosion/KRLS-factor/kls-tiles/kls_max_', col, '_', row, '.tif'),
                format = 'GTiff', overwrite = TRUE)
    rm(r); gc()
    
    r = aggregate(kls, 4, function(x, na.rm) quantile(x, 0.975, na.rm=na.rm))
    
    writeRaster(r, paste0('./erosion/KRLS-factor/kls-tiles/kls_q-upper_', col, '_', row, '.tif'),
                format = 'GTiff', overwrite = TRUE)
    rm(r); gc()
    
    r = aggregate(kls, 4, function(x, na.rm) quantile(x, 0.025, na.rm=na.rm))
    
    writeRaster(r, paste0('./erosion/KRLS-factor/kls-tiles/kls_q-lower_', col, '_', row, '.tif'),
                format = 'GTiff', overwrite = TRUE)
    rm(r); gc()"
    
    output = paste0('Tile ', col, '-', row, ' done')
    
  } else {

    gc()
    
    output = paste0('Tile ', col, '-', row, ' has no values')
  }
}

# End cluster
stopCluster(cl)


time.taken = Sys.time() - start.time
print('Total time:'); print(time.taken)
