#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

start.time = Sys.time()


#########################################

# Soil water content including irrigation

files = list.files('./compaction/data/prepared/swc_irrigated', pattern='.tif$')

r_stack = stack()

for (f in files) {
  r = raster(paste0('./compaction/data/prepared/swc_irrigated/', f))
  r_stack = stack(r_stack, r)
}

m = calc(r_stack, mean)
m = round(m, 0)

writeRaster(m, paste0('./compaction/data/prepared/swc_irrigated/swc_irrigated_mean.tif'), format = 'GTiff',
            datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)

time.taken = Sys.time() - start.time
print('Total time:'); print(time.taken)

