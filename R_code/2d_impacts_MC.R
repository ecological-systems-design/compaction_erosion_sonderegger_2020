#install.packages('rgdal')
#install.packages('raster')
#install.packages('readxl')
library(rgdal)
library(raster)
library(readxl)


# Set working directory
#setwd('/home/sothomas/R_code/soil')
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

start.time = Sys.time()


##############################


# Load rasters
r_w = raster('./data/prepared/mx_ts_yearly_10km_weighted_median.tif')
r = raster('./data/prepared/mx_ts_yearly_10km_median.tif')
r_w[r_w == 0] = NA
mx_ts = cover(r_w, r)

r_w = raster('./data/prepared/mx_ms_yearly_10km_weighted_median.tif')
r = raster('./data/prepared/mx_ms_yearly_10km_median.tif')
r_w[r_w == 0] = NA
mx_ms = cover(r_w, r)

f = (0.272/400*time_horizon) / (0.326/40)  # bottom / mid
f
mx_bs = mx_ms * f

# Load inventory results

df = read_excel('./data/prepared/topcrops_results.xlsx', sheet='Inventory', col_names=TRUE)
df


###########

# Functions

func_cleanup_raster = function(raster, raster_reference) {
  raster_clean = crop(raster, extent(raster_reference))
  raster_clean[is.na(raster_reference)] = NA
  return(raster_clean)
}


################################################

# Calculation of actual and potential production

################################################

# Actual crop production area

# Crop cycles
cropland_f = raster('./data/EarthStat/CroplandPastureArea2000_Geotiff/Cropland2000_5m.tif')
cropland_f = func_cleanup_raster(cropland_f, mx_ts)

wheat_farea = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_HarvestedAreaFraction.tif')
wheat_farea = func_cleanup_raster(wheat_farea, mx_ts)
wheat_cropcycles = wheat_farea / cropland_f
wheat_cropcycles[wheat_cropcycles < 1] = 1

maize_farea = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/maize/maize_HarvestedAreaFraction.tif')
maize_farea = func_cleanup_raster(maize_farea, mx_ts)
maize_cropcycles = maize_farea / cropland_f
maize_cropcycles[maize_cropcycles < 1] = 1

soybean_farea = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/soybean/soybean_HarvestedAreaFraction.tif')
soybean_farea = func_cleanup_raster(soybean_farea, mx_ts)
soybean_cropcycles = soybean_farea / cropland_f
soybean_cropcycles[soybean_cropcycles < 1] = 1

# Area harvested
wheat_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_HarvestedAreaHectares.tif')
wheat_area = func_cleanup_raster(wheat_area, mx_ts)

maize_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/maize/maize_HarvestedAreaHectares.tif')
maize_area = func_cleanup_raster(maize_area, mx_ts)

soybean_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/soybean/soybean_HarvestedAreaHectares.tif')
soybean_area = func_cleanup_raster(soybean_area, mx_ts)

# Actual crop area

wheat_actarea = wheat_area / wheat_cropcycles
#writeRaster(wheat_actarea, paste0('./data/calculated/wheat_croparea_actual.tif'), format = 'GTiff', overwrite = TRUE)

maize_actarea = maize_area / maize_cropcycles
#writeRaster(maize_actarea, paste0('./data/calculated/maize_croparea_actual.tif'), format = 'GTiff', overwrite = TRUE)

soybean_actarea = soybean_area / soybean_cropcycles
#writeRaster(soybean_actarea, paste0('./data/calculated/soybean_croparea_actual.tif'), format = 'GTiff', overwrite = TRUE)

###############################

# Actual production in one year
# actual yield [t/ha/crop-cycle] * area harvested [ha] * crop cycles per year [1/yr] = t / yr (per cell)

# Actual yield
wheat_yield = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_YieldPerHectare.tif')
wheat_yield = func_cleanup_raster(wheat_yield, mx_ts)

maize_yield = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/maize/maize_YieldPerHectare.tif')
maize_yield = func_cleanup_raster(maize_yield, mx_ts)

soybean_yield = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/soybean/soybean_YieldPerHectare.tif')
soybean_yield = func_cleanup_raster(soybean_yield, mx_ts)

# Actual production
wheat_prod = wheat_yield * wheat_actarea * df[['crop_cycles']][1]
maize_prod = maize_yield * maize_actarea * df[['crop_cycles']][4]
soybean_prod = soybean_yield * soybean_actarea * df[['crop_cycles']][7]

wheat_prod_sum = cellStats(wheat_prod, sum)
maize_prod_sum = cellStats(maize_prod, sum)
soybean_prod_sum = cellStats(soybean_prod, sum)

##################################

# Potential production in one year
# potential yield [t/ha/crop-cycle] * crop area [ha] * crop cycles per year [crop-cycles/yr] = t / yr

# Potential yield
wheat_potyield = raster('./data/EarthStat/YieldGapMajorCrops_Geotiff/wheat_yieldgap_geotiff/wheat_yieldpotential.tif')
wheat_potyield = func_cleanup_raster(wheat_potyield, mx_ts)

maize_potyield = raster('./data/EarthStat/YieldGapMajorCrops_Geotiff/maize_yieldgap_geotiff/maize_yieldpotential.tif')
maize_potyield = func_cleanup_raster(maize_potyield, mx_ts)

soybean_potyield = raster('./data/EarthStat/YieldGapMajorCrops_Geotiff/soybean_yieldgap_geotiff/soybean_yieldpotential.tif')
soybean_potyield = func_cleanup_raster(soybean_potyield, mx_ts)

# Potential production
wheat_potprod = wheat_potyield * wheat_actarea * df[['crop_cycles']][1]
maize_potprod = maize_potyield * maize_actarea * df[['crop_cycles']][4]
soybean_potprod = soybean_potyield * soybean_actarea * df[['crop_cycles']][7]

wheat_potprod_sum = cellStats(wheat_potprod, sum)


################################

# Calculation of erosion impacts

################################

r_w = raster('./data/prepared/krls-factor_10km_weighted_median.tif')
r = raster('./data/prepared/krls-factor_10km_median.tif')
r_w[r_w == 0] = NA
krls = cover(r_w, r)

"""
country = 'Switzerland'
s = readOGR(dsn = './data/natural_earth', layer = 'ne_10m_admin_1_states_provinces')
ch_states = s[s@data$admin == country,]
i = 6
shp = ch_states[i,]

krls = crop(krls, shp)
"""

conversion = conversion_factor * time_horizon

conversion_min = 0.02 * 20
conversion_max = 0.02 * 80

krls_min = krls * conversion_min
krls_max = krls * conversion_max
krls_m = sqrt(krls_min * krls_max)
krls_sd = (krls_max/krls_min)^(1/4)


func_matrix_from_distribution = function(distribution, m, sd) {
  
  x = runif(n=length(getValues(m)), min=0, max=1)  # random generated number for each cell
  x[is.na(getValues(m))] = NA  # set random generated number to NA where there was NA before
  
  if (distribution == 'lognormal') {
    v = qlnorm(x, meanlog=log(getValues(m)), sdlog=log(getValues(sd)))  # get value from distribution
  }
  
  if (distribution == 'normal') {
    v = qnorm(x, mean=getValues(m), sd=getValues(sd))  # get value from distribution
  }
  
  #v[v < minimum] = minimum
  #v[v > maximum] = maximum
  
  v = t(matrix(v, ncol=nrow(m), nrow=ncol(m)))
  
  r = raster(v)
  extent(r) = extent(m)
  crs(r) = crs(m)
  
  return(r)
}

func_get_lognormal_parameters = function(v) {
  
  # median
  m = median(v)
  
  # 97.5%-percentile
  q_upper = as.numeric(quantile(v, 0.975, na.rm=TRUE))
  
  # 2.5%-percentiled
  q_lower = as.numeric(quantile(v, 0.025, na.rm=TRUE))              
  
  # sd*
  sd = (q_upper / q_lower)^(1/4)
  
  
  return(list(m, sd))
}


# Run Monte Carlo

simulations = 1000

wheat_eros_abs = c()
wheat_eros_perc = c()

sim.time = Sys.time()


for (sim in seq(simulations)) {
  
  krls_dist = func_matrix_from_distribution('lognormal', krls_m, krls_sd)
  
  yl_wheat_eros = df[['cp']][1] * krls_dist
  yl_wheat_eros[yl_wheat_eros > 100] = 100
  
  wheat_eros_losses = yl_wheat_eros / 100 * wheat_potprod
  wheat_eros_totloss = cellStats(wheat_eros_losses, sum)
  wheat_eros_percloss = cellStats(wheat_eros_losses, sum) / wheat_potprod_sum * 100
  
  wheat_eros_abs = c(wheat_eros_abs, wheat_eros_totloss)
  wheat_eros_perc = c(wheat_eros_perc, wheat_eros_percloss)
  
  print(paste('Simulation', sim, ', time:', Sys.time()-sim.time))
  
}


vals = wheat_eros_abs/1e6

params = func_get_lognormal_parameters(vals)
m = params[[1]]
sd = params[[2]]

hist(vals, breaks=100, prob=TRUE, xlab='Losses [mio t]')
curve(dlnorm(x, meanlog=log(m), sdlog=log(sd), log=F), add=T, col='red', lw=2)

m
sd

