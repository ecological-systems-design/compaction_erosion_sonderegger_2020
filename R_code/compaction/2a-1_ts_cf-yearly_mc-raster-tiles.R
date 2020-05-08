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


##############

# Load rasters

swc = raster('./compaction/data/prepared/swc_irrigated/swc_irrigated_mean.tif')

disp = raster('./compaction/data/prepared/dispersion/ry_dispersion_total_disaggregated.tif')

# Disaggregate raster (from 0.5° to 30 arcseconds)
#disp = disaggregate(disp, 60)

clay = raster('./compaction/data/prepared/clay_topsoil.tif')


##################

# Create functions

func_matrix_from_distribution = function(distribution, m, sd, minimum, maximum) {
  
  x = runif(n=length(getValues(m)), min=0, max=1)  # random generated number for each cell
  x[is.na(getValues(m))] = NA  # set random generated number to NA where there was NA before
  
  if (distribution == 'lognormal') {
    v = qlnorm(x, meanlog=log(getValues(m)), sdlog=log(getValues(sd)))  # get value from distribution
  }
  
  if (distribution == 'normal') {
    v = qnorm(x, mean=getValues(m), sd=getValues(sd))  # get value from distribution
  }
  
  v[v < minimum] = minimum
  v[v > maximum] = maximum
  
  v = t(matrix(v, ncol=nrow(m), nrow=ncol(m)))
  
  return(v)
}

func_get_lognormal_parameters = function(v) {
  
  # median
  mx = apply(v, c(1,2), median)
  
  # 97.5%-percentile
  func_qu = function(x) as.numeric(quantile(x, 0.975, na.rm=TRUE))
  q_upper = apply(v, c(1,2), func_qu)
  
  # 2.5%-percentiled
  func_ql = function(x) as.numeric(quantile(x, 0.025, na.rm=TRUE))
  q_lower = apply(v, c(1,2), func_ql)               
  
  # dispersion-factor
  sdx = (q_upper / q_lower)^(1/4)
  
  # dispersion-factor where 2.5%-percentile is 0
  sdx_umx = sqrt(q_upper / mx)
  sdx_umx[mx==0] = 1
  a = array(0L, dim=dim(mx))
  a[q_lower==0] = 1
  a[mx==0] = 0
  sdx[a==1] = sdx_umx[a==1]
  
  # dispersion-factor where median is 0
  m = apply(v, c(1,2), mean)
  sdx_um = sqrt(q_upper / m)
  sdx_um[mx==0] = 1
  a = array(0L, dim=dim(mx))
  a[mx==0] = 1
  sdx[a==1] = sdx_um[a==1]
  
  # dispersion-factor where mean is 0
  sdx[m==0] = 1
  
  return(list(mx, sdx))
}


##################################################

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
  cc = crop(clay, ext)
  if (all(is.na(getValues(cc))) == FALSE) {
    mx = crop(swc, ext)
    if (all(is.na(getValues(mx))) == FALSE) {
      sdx = crop(disp, ext)
        
      # Adjust NA of rasters
      mx[is.na(cc)] = NA
      cc[is.na(mx)] = NA
      sdx[is.na(mx)] = NA
      
      # Set sdx to 1 where missing
      a = mx
      a[] = 0
      a[!is.na(mx) & is.na(sdx)] = 1
      sdx[a==1] = 1
      
      # Run Monte Carlo
      
      simulations = 1000
      
      cf_mc = list()
      
      for (sim in seq(simulations)) {
        
        swc_dist = func_matrix_from_distribution('lognormal', mx, sdx, 10, 50)
        
        disp_cc = cc
        disp_cc[!is.na(cc)] = 2.5
        
        clay_dist = func_matrix_from_distribution('normal', cc, disp_cc, 0, 100)
        
        cf = (swc_dist * 4.0425e-05 - 8.624e-05) * clay_dist  # (swc / 10 * 0.2625 - 0.056) * 0.00154 * clay content
        
        cf_mc[[sim]] = cf
        
      }
      
      cf_mc = array(unlist(cf_mc), dim=c(ncol=nrow(mx), nrow=ncol(mx), simulations))
      
      params = func_get_lognormal_parameters(cf_mc)
      
      # Write rasters
      
      cf_mx = params[[1]]
      cf_sdx = params[[2]]
      
      r = raster(cf_mx)
      extent(r) = ext
      crs(r) = crs(swc)
      writeRaster(r, paste0('./compaction/characterization_factors_ts/yearly/yearly_mx_', col, '_', row, '.tif'),
                  format = 'GTiff', overwrite = TRUE)
      
      r = raster(cf_sdx)
      extent(r) = ext
      crs(r) = crs(swc)
      writeRaster(r, paste0('./compaction/characterization_factors_ts/yearly/yearly_sdx_', col, '_', row, '.tif'),
                  format = 'GTiff', overwrite = TRUE)
      
      #rm(swc, disp, clay)
      gc()
      
      output = paste0('Tile ', col, '-', row, ' done')

    } else {
      
      #rm(swc, disp, clay)
      gc()
      
      output = paste0('Tile ', col, '-', row, ' has no values')
    }
  } else {
    
    #rm(swc, disp, clay)
    gc()
    
    output = paste0('Tile ', col, '-', row, ' has no values')
  }
}

# End cluster
stopCluster(cl)


time.taken = Sys.time() - start.time
print('Total time:'); print(time.taken)
