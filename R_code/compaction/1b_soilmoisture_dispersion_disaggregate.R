#install.packages('raster')
library(raster)

# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


# Disaggregate raster (from 0.5° to 30 arcseconds)

disp = raster('./compaction/data/prepared/dispersion/ry_dispersion_total.tif')
disp_hr = disaggregate(disp, 60)

writeRaster(disp_hr, './compaction/data/prepared/dispersion/ry_dispersion_total_disaggregated.tif', format = 'GTiff', overwrite = TRUE)
