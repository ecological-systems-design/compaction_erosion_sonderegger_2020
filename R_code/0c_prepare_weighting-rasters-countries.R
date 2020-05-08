#install.packages('rgdal')
#install.packages('raster')
#install.packages('doParallel')
#install.packages('foreach')
library(rgdal)
library(raster)
library(doParallel)
library(foreach)

start.time = Sys.time()


# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


######################

# Create main function

func_get_cf_comp_parallel = function(s, ids, r_w, out_folder) {
  
  # Define how many cores you want to use
  UseCores = round(detectCores() / 4 * 3) 
  
  # Register CoreCluster
  cl = makeCluster(UseCores)
  registerDoParallel(cl)
  
  if (file.exists('log_weights_countries.txt')) {
    file.remove('log_weights_countries.txt')
  }
  file.create('log_weights_countries.txt')
  
  
  # Get a list of results from foreach parallel loop
  n_start = 1
  n = length(s)
  
  results = foreach(i=n_start:n, .packages=c('rgdal', 'raster')) %dopar% {
    
    # Create functions
    
    func_split_shape_raster = function(s, r) {
      
      # Iterate through polygons and mask rasters
      raster_list = list()
      
      for (i in c(1:length(s@polygons[[1]]@Polygons))) {
        
        geom = s@polygons[[1]]@Polygons[[i]]
        poly = Polygons(list(geom), i)
        spp = SpatialPolygons(list(poly))
        
        rc = crop(r, spp)
        m = mask(rc, spp)
        m[m == 0] = NA
        
        if (all(is.na(getValues(m))) == FALSE) {
          raster_list = c(raster_list, m)
        }
        
      }
      
      # Merge rasters
      if (length(raster_list) > 0) {
        raster_mos.mosaicargs = raster_list
        raster_mos.mosaicargs$fun = mean
        mos = do.call(mosaic, raster_mos.mosaicargs)
      } else {
        mos = m
      }
      
      return(mos)
    }
    
    
    ##############
    
    # Calculations
    
    shp = s[i,]
    name = as.character(shp@data$name)
    id = ids[i]
    
    sink('log_weights_countries.txt', append=TRUE)
    cat(paste0('Starting iteration ', i, ' of ', n, ', id: ', id, ', name: ', name, '\n'))
    sink()
    
    prepare.time = Sys.time()
    
    # Check whether extents of shape and raster overlap
    if (((extent(shp)[4] < extent(r_w)[3]) | (extent(shp)[3] > extent(r_w)[4])) == FALSE) {
      
      # For large shapes iterate through polygons
      if ((extent(shp)[2]-extent(shp)[1])*(extent(shp)[4]-extent(shp)[3]) > 3000) {
        w = func_split_shape_raster(shp, r_w) 
      } else {
        r_wc = crop(r_w, shp)
        w = mask(r_wc, shp)
        w[w == 0] = NA
      }
      
      # Check if there are weights larger than 0
      if (((all(is.na(getValues(w)))) == FALSE) & (max(getValues(w), na.rm=TRUE) != 0)) {
        writeRaster(w, paste0(out_folder, id, '.tif'), format = 'GTiff', overwrite = TRUE)
      }
    } 
  }
  stopCluster(cl)
}


################################################
#                 FILL IN HERE                 #
################################################

# Select files

# Shapefile
s = readOGR(dsn = './data/ecoinvent/countries', layer = 'countries')
#s = readOGR(dsn = './data/prepared', layer = 'states')

# Choose shapefile ID column (name after the $-sign)...
ids = as.character(s@data$id)
#ids = as.character(s@data$ne_id)

# Raster used for weighting
r_w = raster('./data/prepared/landuse_area_weighting.tif')

out_folder = './data/prepared/weighting/countries/'
#out_folder = './data/prepared/weighting/states/'

################################################
#                   UNTIL HERE                 #
################################################


# Get characterization factors and uncertainties
func_get_cf_comp_parallel(s, ids, r_w, out_folder)

t = Sys.time() - start.time
print(paste('Total time:', round(t, 3), units(t)))

