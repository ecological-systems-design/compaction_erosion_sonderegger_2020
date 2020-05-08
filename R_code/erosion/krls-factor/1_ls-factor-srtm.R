#install.packages('rgdal')
#install.packages('raster')
#install.packages('RSAGA')
library(rgdal)
library(raster)
library(RSAGA)

start.time = Sys.time()

setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/erosion/krls-factor')

data_path = 'C:/Users/Sonderegger/Documents/GIS_data/CSI-CGIAR/SRTMv4.1/6_5x5_TIFs/'

# Create temp-folder
if (!dir.exists('./temp')) {
  dir.create('./temp')
}
temp_dir = paste('./temp', sep = '')

# Create tiles_temp-folder
if (!dir.exists('./tiles_temp')) {
  dir.create('./tiles_temp')
}
tiles_temp_dir = paste('./tiles_temp', sep = '')

# Create tiles-folder
if (!dir.exists('./ls-tiles')) {
  dir.create('./ls-tiles')
}
if (!dir.exists('./ls-tiles/mean')) {
  dir.create('./ls-tiles/mean')
}
if (!dir.exists('./tiles/min')) {
  dir.create('./tiles/min')
}
if (!dir.exists('./ls-tiles/max')) {
  dir.create('./ls-tiles/max')
}



# Define functions

collect_surrounding_rasters = function(x, y, dx) {
  
  sr_list = list()
  
  for (i in c(x:(x+dx))) {
    for (j in c((y-1):(y+1))) {
      
      if (i < 10) {
        is = paste('0', i, sep = '')
      } else {
        is = i
      }
      if (j < 10) {
        js = paste('0', j, sep = '')
      } else {
        js = j
      }
      
      rzip = paste(data_path, 'srtm_', is, '_', js, '.zip', sep = '')
      
      if(file.exists(rzip)) {
        # Unzip file to temp-folder and add to list
        unzip(rzip, exdir = temp_dir)
        rasterfile = paste('./temp/srtm_', is, '_', js, '.tif', sep = '')
        r = raster(rasterfile)
        sr_list = c(sr_list, r)
      } else {
        # Create raster filled with 0 and add to list
        r = raster()
        x_min = (i * 5) - 185
        x_max = x_min + 5
        y_max = -j * 5 + 65
        y_min = y_max - 5
        r = crop(r, extent(x_min, x_max, y_min, y_max))
        r[] = NA
        r = disaggregate(r, 1200)
        sr_list = c(sr_list, r)
      }
    }
  }
  return(sr_list)
}


cut_tile = function(rasterlist, utm_zone, utm_min, utm_max) {
  
  r = rasterlist[[5]]
  resolution = res(r)[1]
  border = 10 * resolution
  
  if (utm_min == -180) {
    x_min = -180
  } else {
    x_min = utm_min - border
  }
  
  crop_extent_top = extent(x_min,
                           utm_max + border,
                           extent(r)[4],
                           extent(r)[4] + border)
  
  r1c = crop(rasterlist[[1]], crop_extent_top)
  r4c = crop(rasterlist[[4]], crop_extent_top)
  
  if (length(rasterlist) == 9) {
    r7c = crop(rasterlist[[7]], crop_extent_top)
  }
  
  crop_extent_bottom = extent(x_min,
                              utm_max + border,
                              extent(r)[3] - border,
                              extent(r)[3])
  
  r3c = crop(rasterlist[[3]], crop_extent_bottom)
  r6c = crop(rasterlist[[6]], crop_extent_bottom)
  if (length(rasterlist) == 9) {
    r9c = crop(rasterlist[[9]], crop_extent_bottom)
  }
  
  crop_extent_mid = extent(x_min,
                           utm_max + border,
                           extent(r)[3] - border,
                           extent(r)[4] + border)
  
  r2c = crop(rasterlist[[2]], crop_extent_mid)
  r5c = crop(rasterlist[[5]], crop_extent_mid)
  
  if (length(rasterlist) == 9) {
    r8c = crop(rasterlist[[8]], crop_extent_mid)
  }
  
  rasterlist2 = list(r1c, r2c, r3c, r4c, r5c, r6c)
  
  if (length(rasterlist) == 9) {
    rasterlist2 = list(r1c, r2c, r3c, r4c, r5c, r6c, r7c, r8c, r9c)
  }
  
  # Create mosaic of raster tiles
  raster_mos.mosaicargs = rasterlist2
  raster_mos.mosaicargs$fun = mean
  mos = do.call(mosaic, raster_mos.mosaicargs)
  
  # Crop extent of UTM-zone
  crop_extent = extent(utm_min, utm_max, extent(r)[3], extent(r)[4])
  mos_c = crop(mos, crop_extent)
  
  # Check if raster contains values
  if (all(is.na(as.matrix(mos_c)))) {
    return(c(mos_c, mos_c, 0))  
  }
  
  # Crop to UTM-zone extension plus margin and set NA to 0 in order to prevent edge-effects
  crop_extent_extra = extent(x_min,
                             utm_max + border,
                             extent(r3c)[3],
                             extent(r1c)[4])
  mos_extra = crop(mos, crop_extent_extra)
  mos_extra[is.na(mos_extra)] = 0
  
  return(c(mos_extra, mos_c, 1))
}


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
  
  rs = round(3600 * res(ls)[1], 1)
  
  return(ls)
}


aggregation_loop_correction = function(tile, fact, func) {
  
  rasters_agg = list()
  
  for (k in c(extent(tile)[1]:extent(tile)[2])) {  
    for (l in c(extent(tile)[3]:extent(tile)[4])) {
      
      if (k < - 179) {
        r_lsdc = crop(tile, extent(extent(tile)[1], extent(tile)[2], l, l+1))
        
        # Add missing first column
        r = raster()
        r = crop(r, extent(-180, -179, l, l+1))
        r[] = NA
        r = disaggregate(r, 1 / res(tile)[1])
        rc = crop(r, extent(-180, extent(tile)[1], l, l+1))
        
        raster_mos.mosaicargs = list(r_lsdc, rc)
        raster_mos.mosaicargs$fun = mean
        r = do.call(mosaic, raster_mos.mosaicargs)
        
      } else {
        k = round(k)
        crop_extent = extent(k, k+1, l, l+1)
        r = crop(tile, extent(k, k+1, l, l+1))
        
      }
      r_agg = aggregate(r, fact, fun = func)
      rasters_agg = c(rasters_agg, r_agg)
    }
  }
  
  raster_mos.mosaicargs = rasters_agg
  raster_mos.mosaicargs$fun = mean
  mos = do.call(mosaic, raster_mos.mosaicargs)
  
  return(mos)
}



aggregation_loop = function(tile, fact, func) {
  
  rasters_agg = list()
  
  for (k in c(extent(tile)[1]:extent(tile)[2])) {  
    for (l in c(extent(tile)[3]:extent(tile)[4])) {
      
      crop_extent = extent(k, k+1, l, l+1)
      r = crop(tile, extent(k, k+1, l, l+1))
      r_agg = aggregate(r, fact, fun = func)
      rasters_agg = c(rasters_agg, r_agg)
    }
  }
  
  raster_mos.mosaicargs = rasters_agg
  raster_mos.mosaicargs$fun = mean
  mos = do.call(mosaic, raster_mos.mosaicargs)
  
  return(mos)
}



# RUN FUNCTIONS

# Create input values for iteration
srtm = c(1:72)

srtm_cut = c(1:11)*6+1
srtm_tiles = srtm [! srtm %in% srtm_cut]

srtm_2x = c(c(1:11)*6-1, c(1:11)*6)
srtm_0x = c(1:11)*6+1
dx = srtm
dx [! srtm %in% srtm_2x] = 1
dx [srtm %in% srtm_2x] = 2
dx [srtm %in% srtm_0x] = 0
dx_tiles = dx [dx != 0]
dx_tiles = dx_tiles[1:60]

utm_mins = c(0:59) * 6 - 180 
utm_maxs = c(1:60) * 6 - 180


for (utm_x in 1:60) {  # 1 - 60
  for (y in 1:24) {  # 1 - 24

    x = srtm_tiles[utm_x]
    dx = dx_tiles[utm_x]
    utm_zone = utm_x
    utm_min = utm_mins[utm_x]
    utm_max = utm_maxs[utm_x]
    
    tile_start.time = Sys.time()
    
    cat(paste('', '*********************************', '', '', sep = '\n'))
    
    if (x < 10) {
      is = paste('0', x, sep = '')
    } else {
      is = x
    }
    
    x1 = x + 1
    
    if (x1 < 10) {
      is1 = paste('0', x1, sep = '')
    } else {
      is1 = x1
    }
    
    if (y < 10) {
      js = paste('0', y, sep = '')
    } else {
      js = y
    }
    
    rzip = paste(data_path, 'srtm_', is, '_', js, '.zip', sep = '')
    rzip1 = paste(data_path, 'srtm_', is1, '_', js, '.zip', sep = '')
    
    if(file.exists(rzip) | file.exists(rzip1)) {
      
      print(paste('UTM-SRTM-tile:', utm_x, y))
      cat(paste('', '', sep = '\n'))
      
      # Collect rasters and cut tile
      rasterlist = collect_surrounding_rasters(x, y, dx)
      
      rasters = cut_tile(rasterlist, utm_zone, utm_min, utm_max)
      rm(rasterlist)
      
      if (rasters[[3]] != 0) {
        
        tile_raw = rasters[[1]]
        tile_cut = rasters[[2]]
        crop_extent = extent(tile_cut)
        rm(rasters)
        
        ls_start.time = Sys.time()
        
        # Calculate LS-factor
        ls_tile_raw = calc_ls_factor(tile_raw, utm_zone)
        
        time.taken = Sys.time() - ls_start.time
        print('Calculating LS-factor:'); print(time.taken)
        
        # Save results for tile_cut
        
        if (utm_zone < 10) {
          utm_name = paste('0', utm_zone, sep = '')
        } else {
          utm_name = utm_zone
        }
        
        if (y < 10) {
          y_name = paste('0', y, sep = '')
        } else {
          y_name = y
        }
        
        
        name_out = paste('utm', utm_name, '_srtm', y_name, sep = '')
        #writeRaster(ls_tile_raw, paste('./tiles_temp/ls_tile_raw_', name_out, '.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
        writeRaster(tile_cut, paste('./tiles_temp/tile_cut_', name_out, '.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
        #ls_tile_raw = raster(paste('./tiles_temp/ls_tile_raw_', name_out, '.tif', sep = ''))
        #tile_cut = raster(paste('./tiles_temp/tile_cut_', name_out, '.tif', sep = ''))
        #crop_extent = extent(tile_cut)
        
        rm(tile_raw, tile_cut)
        gc()
        
        corr_start.time = Sys.time()
        
        # Correct for cell shift from SAGA GIS
        lsd = disaggregate(ls_tile_raw, 2)
        
        # Crop to UTM-zone
        lsdc = crop(lsd, crop_extent)
        
        rm(ls_tile_raw, lsd)
        gc()
        
        writeRaster(lsdc, paste('./output/lsdc_', name_out, '.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
        lsdc = raster(paste('./output/lsdc_', name_out, '.tif', sep = ''))
        
        # Reaggregate to original resolution
        ls_tile_corrected = aggregation_loop_correction(lsdc, 2, mean)
        rm(lsdc)
        gc()
        
        # Set water cells to NA
        tile_cut = raster(paste('./tiles_temp/tile_cut_', name_out, '.tif', sep = ''))
        ls_tile_corrected[is.na(tile_cut)] = NA
        rm(tile_cut)
        gc()
        
        time.taken = Sys.time() - corr_start.time
        print('Correcting and cropping tile:'); print(time.taken)
        
        # Save results
        #writeRaster(ls_tile_corrected, paste('./ls-tiles/ls_tile_corrected_', name_out, '.tif', sep = ''), format = 'GTiff', overwrite = TRUE)
        #ls_tile_corrected = raster(paste('./ls-tiles/ls_tile_corrected_', name_out, '.tif', sep = ''))
        
        y_max = -y * 5 + 65
        y_min = y_max - 5
        extent_int = extent(utm_min, utm_max, y_min, y_max)
        
        # Aggregate to 250m and get min and max values
        ls_tile_corrected = disaggregate(ls_tile_corrected, 2)
        
        ls_tile_mean = aggregation_loop(ls_tile_corrected, 5, mean)
        ls_tile_mean = ls_tile_mean * 100
        extent(ls_tile_mean) = extent_int
        
        writeRaster(ls_tile_mean, paste('./ls-tiles/mean/ls-tile-mean_', name_out, '_250m.tif', sep = ''), format = 'GTiff', 
                    datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
        
        # ls_tile_min = aggregation_loop(ls_tile_corrected, 5, min)
        # ls_tile_min = ls_tile_min * 100
        # extent(ls_tile_min) = extent_int
        # 
        # writeRaster(ls_tile_min, paste('./ls-tiles/min/ls-tile-min_', name_out, '_250m.tif', sep = ''), format = 'GTiff',
        #             datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
        # 
        # ls_tile_max = aggregation_loop(ls_tile_corrected, 5, max)
        # ls_tile_max = ls_tile_max * 100
        # extent(ls_tile_max) = extent_int
        # 
        # writeRaster(ls_tile_max, paste('./ls-tiles/max/ls-tile-max_', name_out, '_250m.tif', sep = ''), format = 'GTiff',
        #             datatype = 'INT2U', options = c('COMPRESS=DEFLATE'), overwrite = TRUE)
        # 
        # rm(ls_tile_corrected, ls_tile_mean, ls_tile_min, ls_tile_max)
        rm(ls_tile_corrected, ls_tile_mean)
        
      } else {
        print('After combining data: tile is empty.')
        cat(paste('', '', sep = '\n'))
      }
      
      # Empty temporary folders
      do.call(file.remove, list(list.files('./temp', full.names = TRUE)))
      do.call(file.remove, list(list.files('./tiles_temp', full.names = TRUE)))
      
    } else {
      print(paste('UTM-SRTM-tile', utm_x, y, 'is empty.'))
      cat(paste('', '', sep = '\n'))
    }
    time.taken = Sys.time() - tile_start.time
    print('Tile calculation time:'); print(time.taken)
  }
}


time.taken = Sys.time() - start.time
print('Script time:'); print(time.taken)

