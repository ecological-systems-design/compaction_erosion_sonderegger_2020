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

#s = readOGR(dsn = './data/ecoinvent/countries', layer = 'countries')
s = readOGR(dsn = './data/prepared', layer = 'states')

# Choose shapefile ID column (name after the $-sign)...
#ids = as.character(s@data$id)
ids = as.character(s@data$ne_id)

# ...and provide the name for later processing
#id_name = 'id'
id_name = 'ne_id'

#weighting_path = './data/prepared/weighting/countries/'
weighting_path = './data/prepared/weighting/states/'
files = list.files(path = weighting_path)

# Order files by size
df = data.frame(file=numeric(length(files)), size=numeric(length(files)))

for (i in 1:length(files)) {
  size = file.size(paste0(weighting_path, files[i]))
  df$file[i] = files[i]
  df$size[i] = size
}

files = df[with(df, order(-size)), 'file']

#output_path = './erosion/krls-factor/aggregated/krls_countries_yearly'
output_path = './erosion/krls-factor/aggregated/krls_states_yearly'

##############

# Load rasters

# Raster with the median CF
r_mx = raster('./erosion/krls-factor/rasters/krls-factor_median_1km.tif')

################################################
#                   UNTIL HERE                 #
################################################


# Define how many cores you want to use
UseCores = round(detectCores() / 5 * 4) 

# Register CoreCluster
cl = makeCluster(UseCores)
registerDoParallel(cl)

if (file.exists('log_krls_geounits.txt')) {
  file.remove('log_krls_geounits.txt')
}
file.create('log_krls_geounits.txt')

if (file.exists('output_krls_geounits.txt')) {
  file.remove('output_krls_geounits.txt')
}
file.create('output_krls_geounits.txt')

# Get a list of results from foreach parallel loop
n_start = 1
n = length(files)

#results = foreach(i=n_start:n, .packages=c('rgdal', 'raster', 'spatstat')) %dopar% {
for (i in n_start:n) {
  
  ##################
  
  # Create functions
  
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
  
  sink('log_krls_geounits.txt', append=TRUE)
  cat(paste0('Starting iteration ', i, ' of ', n, ' , id: ', id, '\n'))
  sink()
  
  prepare.time = Sys.time()
  
  rm_w = raster(paste0(weighting_path, files[i]))
  
  tryCatch({
    rm_mx = crop(r_mx, rm_w)
    
    rm_mx[is.na(rm_w)] = NA
    
    # Check if there are CF values
    if (all(is.na(getValues(rm_mx))) == FALSE) {
      
      # Adjust NA of rasters
      rm_w[is.na(rm_mx)] = NA
      w = getValues(rm_w)
      
      # Check if there are weights larger than 0
      if ((all(is.na(w)) == FALSE) & (max(w, na.rm=TRUE) != 0)) {
        
        # Get vectors with values
        mx = getValues(rm_mx)
        mx = mx[!is.na(mx)]
        w = w[!is.na(w)]
        
        t = Sys.time()-prepare.time
        
        sink('log_krls_geounits.txt', append=TRUE)
        cat(paste('Prepared values for id', id, ', time:', round(t, 3), units(t), '\n'))
        sink()
        
        params_spat = func_get_lognormal_weighted_parameters(mx, w)
        sdx_spat = params_spat[2]
        
        mx_w = weighted.median(mx, w)
        
        w_tot = sum(w, na.rm=TRUE)
        
      } else {
        mx_w = NA
        sdx_spat = NA
        w_tot = NA
      }
    } else {
      mx_w = NA
      sdx_spat = NA
      w_tot = NA
    }
    
    t = Sys.time()-prepare.time
    
    sink('log_krls_geounits.txt', append=TRUE)
    cat(paste('Finished', id, ', time:', round(t, 3), units(t), '\n'))
    sink()
    
    sink('output_krls_geounits.txt', append=TRUE)
    cat(paste0(paste(id, mx_w, sdx_spat, w_tot, sep=','), '\n'))
    sink()
    
    result = c(id, mx_w, sdx_spat, w_tot)
    
  }, error=function(e){
    sink('log_krls_geounits.txt', append=TRUE)
    cat(paste('Error in', id, '\n'))
    sink()
    
    sink('output_krls_geounits.txt', append=TRUE)
    cat(paste0(paste(id, 'Error', sep=','), '\n'))
    sink()
  })
}

stopCluster(cl)


# Read results
result = read.csv('./output_krls_geounits.txt', sep=',', header=0)
colnames(result) = c(id_name, 'median', 'sd_spat', 'weight_tot')

write.csv(result, paste0(output_path, '.csv'))


t = Sys.time() - start.time
print(paste('Total time:', round(t, 3), units(t)))


