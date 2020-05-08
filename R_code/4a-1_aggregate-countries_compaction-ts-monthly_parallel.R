#install.packages('rgdal')
#install.packages('raster')
#install.packages('doParallel')
#install.packages('foreach')
#install.packages('spatstat')
library(rgdal)
library(raster)
library(doParallel)
library(foreach)
library(spatstat)

start.time = Sys.time()

# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


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

# ...and provide the name for later processing
id_name = 'id'
#id_name = 'ne_id'

weighting_path = './data/prepared/weighting/countries/'
files = list.files(path = weighting_path)

# Order files by size
df = data.frame(file=numeric(length(files)), size=numeric(length(files)))

for (i in 1:length(files)) {
  size = file.size(paste0(weighting_path, files[i]))
  df$file[i] = files[i]
  df$size[i] = size
}

files = df[with(df, order(-size)), 'file']

output_path = './compaction/characterization_factors/aggregated/cf_ts_countries_'

# For each month...

for (month in 1:12) {
  
  ##############
  
  # Load rasters
  
  if (month < 10) {
    i = paste0('0', as.character(month))
  } else {
    i = as.character(month)
  }
  
  # Raster with the median CF
  r_mx = raster(paste0('./compaction/characterization_factors/mx_ts_', i, '.tif'))
  # Raster with the multiplicative standard deviation (sd*)
  r_sdx = raster(paste0('./compaction/characterization_factors/sdx_ts_', i, '.tif'))
  
################################################
#                   UNTIL HERE                 #
################################################

  
  # Define how many cores you want to use
  UseCores = round(detectCores() / 5 * 4) 
  
  # Register CoreCluster
  cl = makeCluster(UseCores)
  registerDoParallel(cl)
  
  if (file.exists('log_comp_geounits.txt')) {
    file.remove('log_comp_geounits.txt')
  }
  file.create('log_comp_geounits.txt')
  
  if (file.exists('output_comp_geounits.txt')) {
    file.remove('output_comp_geounits.txt')
  }
  file.create('output_comp_geounits.txt')
  
  # Get a list of results from foreach parallel loop
  n_start = 1
  n = length(files)
  
  results = foreach(i=n_start:n, .packages=c('rgdal', 'raster', 'spatstat')) %dopar% {
    
    ##################
    
    # Create functions
    
    func_lognorm_vector = function(m, sd) {
      
      x = runif(n=length(m), min=0, max=1)  # random generated number for each element
      
      v = qlnorm(x, meanlog=log(m), sdlog=log(sd))  # get value from distribution
      
      return(v)
    }
    
    func_get_lognormal_parameters = function(v) {
      
      mx = median(v)
      q_upper = as.numeric(quantile(v, 0.975))
      q_lower = as.numeric(quantile(v, 0.025))                
      sdx = (q_upper / q_lower)^(1/4)
      
      # dispersion-factor where 2.5%-percentile is 0
      if (q_lower == 0) {
        sdx = sqrt(q_upper / mx)
      }
      
      # dispersion-factor where median is 0
      m = mean(v)
      if (mx == 0) {
        sdx = sqrt(q_upper / m)
      }
      
      # dispersion-factor where mean is 0
      if (m == 0) {
        sdx = 1
      }
      
      return(c(mx, sdx))
    }
    
    func_get_lognormal_weighted_parameters = function(v, w) {
      
      mx = weighted.median(v, w)
      q_upper = as.numeric(weighted.quantile(v, w, probs=0.975))
      q_lower = as.numeric(weighted.quantile(v, w, probs=0.025))                
      sdx = (q_upper / q_lower)^(1/4)
      
      # dispersion-factor where 2.5%-percentile is 0
      if (q_lower == 0) {
        sdx = sqrt(q_upper / mx)
      }
      
      # dispersion-factor where median is 0
      x = v * w
      m = sum(x, na.rm=TRUE) / sum(w, na.rm=TRUE)
      if (mx == 0) {
        sdx = sqrt(q_upper / m)
      }
      
      # dispersion-factor where mean is 0
      if (m == 0) {
        sdx = 1
      }
      
      return(c(mx, sdx))
    }
    
    ##############
    
    # Calculations
    
    id = gsub('.tif', '', files[i])
    
    sink('log_comp_geounits.txt', append=TRUE)
    cat(paste0('Starting iteration ', i, ' of ', n, ' , id: ', id, '\n'))
    sink()
    
    prepare.time = Sys.time()
    
    rm_w = raster(paste0(weighting_path, files[i]))
    
    tryCatch({
      
      if(id == 162) {
        
        ext1 = extent(extent(rm_w)[1],-66,extent(rm_w)[3],extent(rm_w)[4])
        ext2 = extent(172,extent(rm_w)[2],extent(rm_w)[3],extent(rm_w)[4])
        
        rm_mx1 = crop(r_mx, ext1)
        rm_w1 = crop(rm_w, ext1)
        rm_mx1[is.na(rm_w1)] = NA
        rm_w1[is.na(rm_mx1)] = NA
        
        rm_mx2 = crop(r_mx, ext2)
        rm_w2 = crop(rm_w, ext2)
        rm_mx2[is.na(rm_w2)] = NA
        rm_w2[is.na(rm_mx2)] = NA
        
        rm_sdx1 = crop(r_sdx, ext1)
        rm_sdx1[is.na(rm_mx1)] = NA
        
        rm_sdx2 = crop(r_sdx, ext2)
        rm_sdx2[is.na(rm_mx2)] = NA
        
        # Set sdx to 1 where missing
        rm_sdx1[rm_sdx1 == 0] = 1
        rm_sdx2[rm_sdx2 == 0] = 1
        
        mx = c(getValues(rm_mx1), getValues(rm_mx2))
        mx = mx[!is.na(mx)]
        sdx = c(getValues(rm_sdx1), getValues(rm_sdx2))
        sdx = sdx[!is.na(sdx)]
        w = c(getValues(rm_w1), getValues(rm_w2))
        w = w[!is.na(w)]
        
      } else {
        
        rm_mx = crop(r_mx, rm_w)
        
        rm_mx[is.na(rm_w)] = NA
        
        # Check if there are CF values
        if (all(is.na(getValues(rm_mx))) == FALSE) {
          rm_sdx = crop(r_sdx, rm_w)
          rm_sdx[is.na(rm_mx)] = NA
          
          # Adjust NA of rasters
          rm_w[is.na(rm_mx)] = NA
          w = getValues(rm_w)
          
          # Check again if there are weights larger than 0
          if ((all(is.na(w)) == FALSE) & (max(w, na.rm=TRUE) != 0)) {
            
            rm_sdx[is.na(rm_w)] = NA
            
            # Set sdx to 1 where missing
            a = rm_mx
            a[] = 0
            a[!is.na(rm_mx) & is.na(rm_sdx)] = 1
            rm_sdx[a==1] = 1
            
            # Get vectors with values
            mx = getValues(rm_mx)
            mx = mx[!is.na(mx)]
            sdx = getValues(rm_sdx)
            sdx = sdx[!is.na(sdx)]
            w = w[!is.na(w)]
          } else {
            mx_w = NA
            sdx_temp = NA
            sdx_spat = NA
            sdx_tot = NA
            w_tot = NA
          }
        } else {
          mx_w = NA
          sdx_temp = NA
          sdx_spat = NA
          sdx_tot = NA
          w_tot = NA
        }
      }
      
      t = Sys.time()-prepare.time
      
      sink('log_comp_geounits.txt', append=TRUE)
      cat(paste('Prepared values for id', id, ', time:', round(t, 3), units(t), '\n'))
      sink()
      
      mc.time = Sys.time()
      
      # Run Monte Carlo
      simulations = 1000
      cf_mc = c()
      
      for (sim in seq(simulations)) {
        
        cf = func_lognorm_vector(mx, sdx)
        cf_wm = weighted.median(cf, w)
        cf_mc = c(cf_mc, cf_wm)
      }
      
      t = Sys.time()-mc.time
      
      sink('log_comp_geounits.txt', append=TRUE)
      cat(paste('Finished Monte Carlo for id', id, ', time:', round(t, 3), units(t), '\n'))
      sink()
      
      params_temp = func_get_lognormal_parameters(cf_mc)
      sdx_temp = params_temp[2]
      
      params_spat = func_get_lognormal_weighted_parameters(mx, w)
      sdx_spat = params_spat[2]
      
      # Temporal and spatial dispersion aggregated
      sdx_tot = sqrt(exp(sqrt(log(sdx_temp^2)^2+log(sdx_spat^2)^2)))
      
      mx_w = weighted.median(mx, w)
      
      w_tot = sum(w, na.rm=TRUE)
      
      sink('output_comp_geounits.txt', append=TRUE)
      cat(paste0(paste(id, mx_w, sdx_temp, sdx_spat, sdx_tot, w_tot, sep=','), '\n'))
      sink()
      
      result = c(id, mx_w, sdx_temp, sdx_spat, sdx_tot, w_tot)
      
    }, error=function(e){
      sink('log_comp_geounits.txt', append=TRUE)
      cat(paste('Error in', id, '\n'))
      sink()
      
      sink('output_comp_geounits.txt', append=TRUE)
      cat(paste0(paste(id, 'Error', sep=','), '\n'))
      sink()
    })
  }
  
  stopCluster(cl)
  
  # Read results
  result = read.csv('./output_comp_geounits.txt', sep=',', header=0)
  colnames(result) = c(id_name, 'median', 'sd_temp', 'sd_spat', 'sd_tot', 'weight_tot')
  
  write.csv(result, paste0(output_path, month, '.csv'))
  
  t = Sys.time() - start.time
  print(paste('Month', month,'time:', round(t, 3), units(t)))
  
}

t = Sys.time() - start.time
print(paste('Total time:', round(t, 3), units(t)))


