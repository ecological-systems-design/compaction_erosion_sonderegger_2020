#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')


for (utm_zone in 1:60) {  # 1 - 60
  
  tile_start.time = Sys.time()
  
  cat(paste('', '*********************************', '', '', sep = '\n'))
  
  if (utm_zone < 10) {
    is = paste('0', utm_zone, sep = '')
  } else {
    is = utm_zone
  }
  
  print(paste('UTM-GMTED-tile:', utm_zone))
  cat(paste('', '', sep = '\n'))
  
  name_out = paste('utm_', is, '_gmted', sep = '')
  
  lsdc = raster(paste('./ls-tiles/gmted/lsdc_', name_out, '.tif', sep = ''))
  
  if (utm_zone == 1) {
    
    a = as.matrix(lsdc)
    b = a[,1]
    c = cbind(b, a)
    rm(a, b)
    
    r = raster(c)
    rm(c)
    
    r_extent = extent(extent(lsdc)[1] - res(lsdc)[1], extent(lsdc)[2], extent(lsdc)[3], extent(lsdc)[4])
    extent(r) = r_extent
    crs(r) = crs(lsdc)
    
    lsdc = r
    rm(r)
  }
  
  ls = aggregate(lsdc, 2)
    
  writeRaster(ls, paste('./ls-tiles/gmted/ls-factor_', name_out, '.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
    
}

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

