#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)


# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


r = raster('./compaction/characterization_factors/mx_ts_yearly.tif')
ra = aggregate(r, 10, median)
writeRaster(ra, paste0('./data/prepared/mx_ts_yearly_10km_median.tif'), format = 'GTiff', overwrite = TRUE)

r = raster('./compaction/characterization_factors/mx_ms_yearly.tif')
ra = aggregate(r, 10, median)
writeRaster(ra, paste0('./data/prepared/mx_ms_yearly_10km_median.tif'), format = 'GTiff', overwrite = TRUE)


r = raster('./erosion/KRLS-factor/rasters/krls-factor_median_1km.tif')
#re = extend(r, extent(-180, 180, -56, 84))
#writeRaster(re, paste0('./erosion/KRLS-factor/rasters/krls-factor_median_1km_extended.tif'), format = 'GTiff', overwrite = TRUE)
ra = aggregate(r, 10, median)
writeRaster(ra, paste0('./data/prepared/krls-factor_10km_median.tif'), format = 'GTiff', overwrite = TRUE)


