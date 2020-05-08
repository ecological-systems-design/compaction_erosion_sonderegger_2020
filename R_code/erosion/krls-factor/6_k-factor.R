#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)

start.time = Sys.time()

setwd('/home/sothomas/R_code/soil/erosion/krls-factor')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')

data_path = '/home/sothomas/data/GIS_data/ISRIC/soilgrids/'
#data_path = 'C:/Users/Sonderegger/Documents/GIS_data/ISRIC/soilgrids/'

# Create temp-folder
if (!dir.exists('./temp')) {
  dir.create('./temp')
}
temp_dir = paste('./temp', sep = '')

# Create tiles-folder
if (!dir.exists('./k-tiles')) {
  dir.create('./k-tiles')
}


in_sand = raster(paste(data_path, 'SNDPPT_M_sl1_250m.tif', sep = ''))  # %
in_silt = raster(paste(data_path, 'SLTPPT_M_sl1_250m.tif', sep = ''))  # %
in_clay = raster(paste(data_path, 'CLYPPT_M_sl1_250m.tif', sep = ''))  # %
in_orgC = raster(paste(data_path, 'ORCDRC_M_sl1_250m.tif', sep = ''))  # g per kg

x_tiles = 12
y_tiles = 7  # raster goes from - 56° to 84° (140° total)

x_mins = seq(-180, 180 - 360 / x_tiles, 360 / x_tiles)
x_maxs = x_mins + 360 / x_tiles
y_mins = seq(-56, 84 - 140 / y_tiles, 140 / y_tiles)
y_maxs = y_mins + 140 / y_tiles

for (x in 1:12) {  
  for (y in 1:7) {  

    tile_start.time = Sys.time()
    
    name_out = paste(x_mins[x], '-', x_maxs[x], '_', y_mins[y], '-', y_maxs[y], sep = '')
    
    cat(paste('', '*********************************', '', '', sep = '\n'))
    cat(paste('Tile:', name_out))
    cat(paste('', '', sep = '\n'))
    
    crop_extent = extent(x_mins[x], x_maxs[x], y_mins[y], y_maxs[y])
    
    m_sand = crop(in_sand, crop_extent)  # %
    m_silt = crop(in_silt, crop_extent)  # %
    m_clay = crop(in_clay, crop_extent)  # %
    
    time.taken = Sys.time() - tile_start.time
    print('Crop rasters:'); print(time.taken)
    
    f_csand = 0.2 + 0.3 * exp(-0.256 * m_sand * (1 - m_silt/100))
    
    #writeRaster(f_csand, paste('./output/f_csand_', crop_extent[1], '-', crop_extent[3], '_',
    #                           crop_extent[3], '_', crop_extent[4], '.tif', sep = ''),
    #            format = 'GTiff', overwrite = TRUE)
    
    f_claysilt = (m_silt / (m_clay + m_silt)) ^0.3
    
    #writeRaster(f_claysilt, paste('./output/f_claysilt_', crop_extent[1], '-', crop_extent[3], '_',
    #                              crop_extent[3], '_', crop_extent[4], '.tif', sep = ''),
    #            format = 'GTiff', overwrite = TRUE)
    #rm(m_silt, m_clay)
    
    f_hsand = 1 - (0.7 * ( 1 - m_sand/100)) / (1 - m_sand/100 + exp(-5.51 + 22.9 * (1 - m_sand/100)))
    
    #rm(m_sand)
    #writeRaster(f_hsand, paste('./output/f_hsand_', crop_extent[1], '-', crop_extent[3], '_',
    #                           crop_extent[3], '_', crop_extent[4], '.tif', sep = ''),
    #            format = 'GTiff', overwrite = TRUE)
    
    orgC_g_per_kg = crop(in_orgC, crop_extent)  # g / kg
    orgC = orgC_g_per_kg / 10  # %
    #rm(orgC_g_per_kg)
    
    f_org = 1 - (0.0256 * orgC) / (orgC + exp(3.72 - 2.95 * orgC))
    
    #writeRaster(f_org, paste('./output/f_org_', crop_extent[1], '-', crop_extent[3], '_',
    #                         crop_extent[3], '_', crop_extent[4], '.tif', sep = ''),
    #            format = 'GTiff', overwrite = TRUE)
    rm(orgC)
    
    time.taken = Sys.time() - tile_start.time
    print('Calculate factors:'); print(time.taken)
    
    k = f_csand * f_claysilt * f_org * f_hsand * 0.1317
    
    time.taken = Sys.time() - tile_start.time
    print('Calculate K:'); print(time.taken)
    
    writeRaster(k, paste('./k-tiles/tiles/k-tile_', name_out, '_250m.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
    #datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
    
    #rm(k, k_tile_mean, k_tile_min, k_tile_max)
    
    time.taken = Sys.time() - tile_start.time
    print('Tile calculation time:'); print(time.taken)
  }
}

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)
