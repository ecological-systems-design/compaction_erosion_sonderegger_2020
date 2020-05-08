#install.packages('raster')
#install.packages('ncdf4')
#install.packages('fitdistrplus')
library(raster)
library(ncdf4)
library(fitdistrplus)


setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


## Prepare data

# Read soil moisture data
nc = nc_open('./compaction/data/WaterGAP_availability/watergap_wfdhum_soilmoist_monthly_1971_1980.nc')
sm1 = ncvar_get(nc, 'SoilMoist')
nc

nc = nc_open('./compaction/data/WaterGAP_availability/watergap_wfdhum_soilmoist_monthly_1981_1990.nc')
sm2 = ncvar_get(nc, 'SoilMoist')

nc = nc_open('./compaction/data/WaterGAP_availability/watergap_wfdhum_soilmoist_monthly_1991_2000.nc')
sm3 = ncvar_get(nc, 'SoilMoist')

lon <- ncvar_get(nc, 'lon')
lat <- ncvar_get(nc, 'lat')

# Create a list of 360 months
sm = list()
for (i in 1:120) {
  sm[[i]] = sm1[,,i]
  sm[[i+120]] = sm2[,,i]
  sm[[i+240]] = sm3[,,i]
}

# Create a list with 3D-arrays (lon x lat x 30 years) for each month
sm_monthly_data = list()

for (m in 1:12) {
  month = list()
    for (y in 1:30) {
      j = y*12-(12-m)
      month[[y]] = sm[[j]]
    }
  sm_monthly_data[[m]] = array(unlist(month), dim=c(720, 280, 30))
}

# Create a list with 3D-arrays (lon x lat x 12 months) for each year
sm_yearly_data = list()

for (y in 1:30) {
  year = list()
  for (m in 1:12) {
    j = y*12-(12-m)
    year[[m]] = sm[[j]]
  }
  sm_yearly_data[[y]] = array(unlist(year), dim=c(720, 280, 12))
}


# Create functions

func_qu = function(x) as.numeric(quantile(x, 0.975, na.rm=TRUE))
func_ql = function(x) as.numeric(quantile(x, 0.025, na.rm=TRUE))


#############################################################

## Total dispersion (over 30 years) for monthly soil moisture

for(i in 1:12) {
  # Apply functions to rows and columns (z-axis)

  # median
  mx = apply(sm_monthly_data[[i]], c(1,2), median)
  
  # 97.5%-percentile
  q_upper = apply(sm_monthly_data[[i]], c(1,2), func_qu)
  
  # 2.5%-percentiled
  q_lower = apply(sm_monthly_data[[i]], c(1,2), func_ql)               
  
  # dispersion-factor
  sdx = (q_upper / q_lower)^(1/4)
  
  # dispersion-factor where 2.5%-percentile is 0
  sdx_umx = sqrt(q_upper / mx)
  a = array(0L, dim=c(720,280))
  a[q_lower==0] = 1
  a[mx==0] = 0
  sdx[a==1] = sdx_umx[a==1]
  
  # dispersion-factor where median is 0
  m = apply(sm_monthly_data[[i]], c(1,2), mean)
  sdx_um = sqrt(q_upper / m)
  a = array(0L, dim=c(720,280))
  a[mx==0] = 1
  sdx[a==1] = sdx_um[a==1]
  
  # dispersion-factor where mean is 0
  sdx[m==0] = 1
  
  
  # Write raster
  r_ext = extent(-180, 180, -56, 84)
  r_crs = crs('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  
  if (i < 10) {
    j = paste0('0', as.character(i))
  } else {
    j = as.character(i)
  }

  r = raster(apply(sdx,1,rev))
  extent(r) = r_ext
  crs(r) = r_crs
  writeRaster(r, paste0('./compaction/data/prepared/dispersion/rm', j, '_dispersion.tif'), format = 'GTiff', overwrite = TRUE)
}


############################################

## Total dispersion for yearly soil moisture

# Monthly (median of 30 years) dispersion

mx_monthly = list()

for(i in 1:12) {
  # Apply functions to rows and columns (z-axis)
  mx_monthly[[i]] = apply(sm_monthly_data[[i]], c(1,2), median)
}

mx_monthly = array(unlist(mx_monthly), dim=c(720, 280, 12))  # Median of 30 years for each month

# median
mx = apply(mx_monthly, c(1,2), median)  # Median of months

# 97.5%-percentile
q_upper = apply(mx_monthly, c(1,2), func_qu)

# 2.5%-percentiled
q_lower = apply(mx_monthly, c(1,2), func_ql)               

# dispersion-factor
sdx = (q_upper / q_lower)^(1/4)

# dispersion-factor where 2.5%-percentile is 0
sdx_umx = sqrt(q_upper / mx)
a = array(0L, dim=c(720,280))
a[q_lower==0] = 1
a[mx==0] = 0
sdx[a==1] = sdx_umx[a==1]

# dispersion-factor where median is 0
m = apply(mx_monthly, c(1,2), mean)
sdx_um = sqrt(q_upper / m)
a = array(0L, dim=c(720,280))
a[mx==0] = 1
sdx[a==1] = sdx_um[a==1]

# dispersion-factor where mean is 0
sdx[m==0] = 1

sdx_monthly = sdx

# Write raster
r = raster(apply(sdx_monthly,1,rev))
extent(r) = r_ext
crs(r) = r_crs
writeRaster(r, './compaction/data/prepared/dispersion/ry_dispersion_monthly.tif', format = 'GTiff', overwrite = TRUE)


# Yearly dispersion (over 30 years)

p_yearly = list()

for(i in 1:30) {
  # Apply functions to rows and columns (z-axis)
  # Yearly precipitation
  p_yearly[[i]] = apply(sm_yearly_data[[i]], c(1,2), sum)
}

p_yearly = array(unlist(p_yearly), dim=c(720, 280, 30))

# median
mx = apply(p_yearly, c(1,2), median)

# 97.5%-percentile
q_upper = apply(p_yearly, c(1,2), func_qu)

# 2.5%-percentiled
q_lower = apply(p_yearly, c(1,2), func_ql)               

# dispersion-factor
sdx = (q_upper / q_lower)^(1/4)

# dispersion-factor where 2.5%-percentile is 0
sdx_umx = sqrt(q_upper / mx)
a = array(0L, dim=c(720,280))
a[q_lower==0] = 1
a[mx==0] = 0
sdx[a==1] = sdx_umx[a==1]

# dispersion-factor where median is 0
m = apply(p_yearly, c(1,2), mean)
sdx_um = sqrt(q_upper / m)
a = array(0L, dim=c(720,280))
a[mx==0] = 1
sdx[a==1] = sdx_um[a==1]

# dispersion-factor where mean is 0
sdx[m==0] = 1

sdx_yearly = sdx

# Write raster
r = raster(apply(sdx_yearly,1,rev))
extent(r) = r_ext
crs(r) = r_crs
writeRaster(r, './compaction/data/prepared/dispersion/ry_dispersion_yearly.tif', format = 'GTiff', overwrite = TRUE)


# Monthly and yearly dispersion aggregated
sdx_total = sqrt(exp(sqrt(log(sdx_monthly^2)^2+log(sdx_yearly^2)^2)))

# Write raster
r = raster(apply(sdx_total,1,rev))
extent(r) = r_ext
crs(r) = r_crs
writeRaster(r, './compaction/data/prepared/dispersion/ry_dispersion_total.tif', format = 'GTiff', overwrite = TRUE)


###################################

x_val = 400  # 400
y_val = 240  # 240

# Plot histograms and distributions
for (x in x_val) {
  for (y in y_val) {
    v = sm_monthly_data[[1]][x,y,]
    
    if (all(is.na(v)) == FALSE) {
      mx = median(v)
      q_upper = as.numeric(quantile(v, 0.975))
      q_lower = as.numeric(quantile(v, 0.025))                
      sdx = (q_upper / q_lower)^(1/4)
      sdx = sqrt(q_upper / mx)

      hist(v, breaks=30, probability=TRUE, 
           xlab="soil moisture", main=paste(x, y))
      curve(dlnorm(x, meanlog=log(mx), sdlog=log(sdx), log=FALSE), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
      #invisible(readline(prompt="Press [enter] to continue"))
    }
  }
}

# Plot histograms and distributions for mx_monthly
for (x in x_val) {
  for (y in y_val) {
    v = mx_monthly[x,y,]
    
    if (all(is.na(v)) == FALSE) {
      mx = median(v)
      q_upper = as.numeric(quantile(v, 0.975))
      q_lower = as.numeric(quantile(v, 0.025))                
      sdx = (q_upper / q_lower)^(1/4)
      m = mean(v)
      sdx = sqrt(q_upper / m)
      
      print(sdx)
      print(sdx_monthly[x,y])
      
      hist(v, breaks=30, probability=TRUE, 
           xlab="soil moisture", main=paste(x, y))
      curve(dlnorm(x, meanlog=log(mx), sdlog=log(sdx), log=FALSE), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
      #invisible(readline(prompt="Press [enter] to continue"))
    }
  }
}

# Plot histograms and distributions for sdx_yearly
for (x in x_val) {
  for (y in y_val) {
    v = p_yearly[x,y,]
    
    if (all(is.na(v)) == FALSE) {
      mx = median(v)
      q_upper = as.numeric(quantile(v, 0.975))
      q_lower = as.numeric(quantile(v, 0.025))                
      sdx = (q_upper / q_lower)^(1/4)
      
      print(sdx)
      print(sdx_yearly[x,y])
      
      hist(v, breaks=30, probability=TRUE, 
           xlab="soil moisture", main=paste(x, y))
      curve(dlnorm(x, meanlog=log(mx), sdlog=log(sdx), log=FALSE), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
      #invisible(readline(prompt="Press [enter] to continue"))
    }
  }
}
