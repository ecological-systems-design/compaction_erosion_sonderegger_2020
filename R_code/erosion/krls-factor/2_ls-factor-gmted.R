#install.packages('rgdal')
#install.packages('raster')
#install.packages('RSAGA')
library(rgdal)
library(raster)
library(RSAGA)

start.time = Sys.time()

setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')

raster_path = 'C:/Users/Sonderegger/Documents/GIS_data/USGS/GMTED2010/be75_grd/be75_grd/'
raster_file = 'w001001.adf'
gmted = raster(paste(raster_path, raster_file, sep = ''))
gmted

if (!dir.exists('./ls-tiles/gmted')) {
  dir.create('./ls-tiles/gmted')
}

# FUNCTIONS

calc_ls_factor = function(raster_in, utm_zone) {
  
  # Define UTM-zone projection string
  proj4_string = paste('+proj=utm +zone=', utm_zone, ' +datum=WGS84 +units=m +no_defs', sep = '')
  
  # Set up rsaga environment:
  work_env = rsaga.env() 
  
  # Reproject raster
  library = 'pj_proj4'
  module = 4
  
  writeRaster(raster_in, './saga/in_file.tif', format = 'GTiff', overwrite = TRUE)
  in_file = './saga/in_file.tif'
  out_file = './saga/out_file_pj'
  
  rsaga.geoprocessor(lib = library, module = module, env = work_env,
                     param = list(SOURCE = in_file,
                                  CRS_METHOD = 0,  # [0] Proj4 Parameters, [1] EPSG Code,
                                  # [2] Well Known Text File, Default = 0
                                  CRS_PROJ4 = proj4_string,  
                                  RESAMPLING = 3,  # [0] Nearest Neighbour, [1] Bilinear Interpolation, 
                                  # [2] Bicubic Spline Interpolation
                                  # [3] B-Spline Interpolation, Default = 3
                                  TARGET_GRID = paste(out_file, '.sgrd', sep = ''))
  )
  
  
  # calculate LS-factor
  library = 'ta_hydrology'
  module = 25
  
  in_file = out_file
  out_file = './saga/out_file_ls'
  
  rsaga.geoprocessor(lib = library, module = module, env = work_env,
                     param = list(DEM = paste(in_file, '.sgrd', sep = ''),
                                  LS_FACTOR = paste(out_file, '.sgrd', sep = ''),
                                  METHOD = 1,
                                  METHOD_SLOPE = 0,
                                  METHOD_AREA = 1,
                                  STOP_AT_EDGE = 1,
                                  EROSIVITY = 1,
                                  STABILITY = 0)
  )
  
  
  # Reproject back
  library = 'pj_proj4'
  module = 4
  
  in_file = out_file
  out_file = './saga/out_file_pj_pj'
  
  rsaga.geoprocessor(lib = library, module = module, env = work_env,
                     param = list(SOURCE = paste(in_file, '.sgrd', sep = ''),
                                  CRS_METHOD = 0,  # [0] Proj4 Parameters, [1] EPSG Code, [2] Well Known Text File
                                  CRS_PROJ4 = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
                                  RESAMPLING = 3,
                                  TARGET_GRID = out_file,
                                  TARGET_USER_XMIN = extent(raster_in)[1],
                                  TARGET_USER_XMAX = extent(raster_in)[2],
                                  TARGET_USER_YMIN = extent(raster_in)[3],
                                  TARGET_USER_YMAX = extent(raster_in)[4],
                                  TARGET_USER_SIZE = res(raster_in)[1])
  )
  
  ls = raster(paste(out_file, '.sdat', sep = ''))
  
  return(ls)
}


# RUN FUNCTIONS

# Create input values for iteration
utm_mins = c(0:59) * 6 - 180 
utm_maxs = c(1:60) * 6 - 180


for (utm_zone in 1:2) {  # 1 - 60

  tile_start.time = Sys.time()
  
  cat(paste('', '*********************************', '', '', sep = '\n'))
  
  if (utm_zone < 10) {
    is = paste('0', utm_zone, sep = '')
  } else {
    is = utm_zone
  }
  
  print(paste('UTM-GMTED-tile:', utm_zone))
  cat(paste('', '', sep = '\n'))
  
  utm_min = utm_mins[utm_zone]
  utm_max = utm_maxs[utm_zone]
  
  border = 10 * res(gmted)[1]
  
  x_min = utm_min - border
  x_max = utm_max + border
  
  if (utm_zone == 1) {
    x_min = -180
  }
  if (utm_zone == 60) {
    x_max = 180  
  }
  
  name_out = paste('utm_', is, '_gmted', sep = '')
  
  raw_extent = extent(x_min , x_max, 60 - border, 84)

  rc = crop(gmted, raw_extent)
  
  if (all(is.na(as.matrix(rc)))) {
    print('Tile is empty!')
  } else {
    
    ls_tile_raw = calc_ls_factor(rc, utm_zone)
    
    time.taken = Sys.time() - tile_start.time
    print('Calculating LS-factor:'); print(time.taken)
    
    # Correct for cell shift from SAGA GIS
    lsd = disaggregate(ls_tile_raw, 2)
    crop_extent = extent(utm_min, utm_max, 60 - 2 * res(gmted)[1], extent(ls_tile_raw)[4] - 0.5 * res(gmted)[1])
    
    rm(ls_tile_raw)
    gc()
    
    # Crop to UTM-zone
    lsdc = crop(lsd, crop_extent)
    
    rm(lsd)
    gc()
    
    writeRaster(lsdc, paste('./ls-tiles/gmted/lsdc_', name_out, '.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
    
    rm(lsdc)
    gc()
  }
}

time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

