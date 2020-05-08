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

# CHOOSE TIME HORIZON IN YEARS
time_horizon = 50

# CHOOSE CONVERSION FACTOR FROM EROSION TO YIELD LOSS (GLOBAL PRESET IS 0.02)
conversion_factor = 0.02

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


##############

# Functions

func_cleanup_raster = function(raster, raster_reference) {
  raster_clean = crop(raster, extent(raster_reference))
  raster_clean[is.na(raster_reference)] = NA
  return(raster_clean)
}


#########################

# Compaction calculations


# Actual crop production area

# Crop cycles
cropland_f = raster('./data/EarthStat/CroplandPastureArea2000_Geotiff/Cropland2000_5m.tif')
cropland_f = func_cleanup_raster(cropland_f, mx_ts)

wheat_farea = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_HarvestedAreaFraction.tif')
wheat_farea = func_cleanup_raster(wheat_farea, mx_ts)
wheat_cropcycles = wheat_farea / cropland_f
wheat_cropcycles[wheat_cropcycles < 1] = 1

# Area harvested
wheat_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_HarvestedAreaHectares.tif')
wheat_area = func_cleanup_raster(wheat_area, mx_ts)

# Actual crop area
wheat_actarea = wheat_area / wheat_cropcycles

# Potential yield
wheat_potyield = raster('./data/EarthStat/YieldGapMajorCrops_Geotiff/wheat_yieldgap_geotiff/wheat_yieldpotential.tif')
wheat_potyield = func_cleanup_raster(wheat_potyield, mx_ts)

# Potential production
wheat_potprod = wheat_potyield * wheat_actarea * df[['crop_cycles']][1]

wheat_potprod_sum = cellStats(wheat_potprod, sum)

# Wheat
prod_tot = wheat_area * 3.482  # yield = 3.482 t/ha-yr
plot(prod_tot, main='wheat prod [t/yr]')
prod_tot_sum = cellStats(prod_tot, sum)


# Impact Potential [tonkm/ha / yr] multiplied with CFs [% yield loss / (tonkm/ha)] = % yield loss / yr (per cell)

yl_wheat_s_top = df[['comp_top']][1] * mx_ts
wheat_s_top_losses = yl_wheat_s_top / 100 * wheat_potprod
wheat_s_top_totloss = cellStats(wheat_s_top_losses, sum)
wheat_s_top_percloss = cellStats(wheat_s_top_losses, sum) / wheat_potprod_sum * 100

yl_wheat_s_mid = df[['comp_mid']][1] * mx_ms
wheat_s_mid_losses = yl_wheat_s_mid / 100 * wheat_potprod
wheat_s_mid_totloss = cellStats(wheat_s_mid_losses, sum)
wheat_s_mid_percloss = cellStats(wheat_s_mid_losses, sum) / wheat_potprod_sum * 100

yl_wheat_s_bottom = df[['comp_bottom']][1] * mx_bs
wheat_s_bottom_losses = yl_wheat_s_bottom / 100 * wheat_potprod
wheat_s_bottom_totloss = cellStats(wheat_s_bottom_losses, sum)
wheat_s_bottom_percloss = cellStats(wheat_s_bottom_losses, sum) / wheat_potprod_sum * 100

wheat_s_totloss = c(wheat_s_top_totloss, wheat_s_mid_totloss, wheat_s_bottom_totloss)
wheat_s_percloss = c(wheat_s_top_percloss, wheat_s_mid_percloss, wheat_s_bottom_percloss)


# wheat_s = wheat_m
yl_wheat_s_top = df[['comp_top']][2] * mx_ts
wheat_s_top_losses = yl_wheat_s_top / 100 * wheat_potprod
wheat_s_top_totloss = cellStats(wheat_s_top_losses, sum)
wheat_s_top_percloss = cellStats(wheat_s_top_losses, sum) / wheat_potprod_sum * 100

yl_wheat_s_mid = df[['comp_mid']][2] * mx_ms
wheat_s_mid_losses = yl_wheat_s_mid / 100 * wheat_potprod
wheat_s_mid_totloss = cellStats(wheat_s_mid_losses, sum)
wheat_s_mid_percloss = cellStats(wheat_s_mid_losses, sum) / wheat_potprod_sum * 100

yl_wheat_s_bottom = df[['comp_bottom']][2] * mx_bs
wheat_s_bottom_losses = yl_wheat_s_bottom / 100 * wheat_potprod
wheat_s_bottom_totloss = cellStats(wheat_s_bottom_losses, sum)
wheat_s_bottom_percloss = cellStats(wheat_s_bottom_losses, sum) / wheat_potprod_sum * 100

wheat_m_totloss = c(wheat_s_top_totloss, wheat_s_mid_totloss, wheat_s_bottom_totloss)
wheat_m_percloss = c(wheat_s_top_percloss, wheat_s_mid_percloss, wheat_s_bottom_percloss)


# wheat_s = wheat_l
yl_wheat_s_top = df[['comp_top']][3] * mx_ts
wheat_s_top_losses = yl_wheat_s_top / 100 * wheat_potprod
wheat_s_top_totloss = cellStats(wheat_s_top_losses, sum)
wheat_s_top_percloss = cellStats(wheat_s_top_losses, sum) / wheat_potprod_sum * 100

yl_wheat_s_mid = df[['comp_mid']][3] * mx_ms
wheat_s_mid_losses = yl_wheat_s_mid / 100 * wheat_potprod
wheat_s_mid_totloss = cellStats(wheat_s_mid_losses, sum)
wheat_s_mid_percloss = cellStats(wheat_s_mid_losses, sum) / wheat_potprod_sum * 100

yl_wheat_s_bottom = df[['comp_bottom']][3] * mx_bs
wheat_s_bottom_losses = yl_wheat_s_bottom / 100 * wheat_potprod
wheat_s_bottom_totloss = cellStats(wheat_s_bottom_losses, sum)
wheat_s_bottom_percloss = cellStats(wheat_s_bottom_losses, sum) / wheat_potprod_sum * 100

wheat_l_totloss = c(wheat_s_top_totloss, wheat_s_mid_totloss, wheat_s_bottom_totloss)
wheat_l_percloss = c(wheat_s_top_percloss, wheat_s_mid_percloss, wheat_s_bottom_percloss)


wheat_totloss = c(wheat_s_totloss, wheat_m_totloss, wheat_l_totloss)
wheat_percloss = c(wheat_s_percloss, wheat_m_percloss, wheat_l_percloss)

activity = c('wheat production, global, small, top',
             'wheat production, global, small, mid',
             'wheat production, global, small, bottom',
             'wheat production, global, medium, top',
             'wheat production, global, medium, mid',
             'wheat production, global, medium, bottom',
             'wheat production, global, large, top',
             'wheat production, global, large, mid',
             'wheat production, global, large, bottom')

results = data.frame(row.names=1, activity, wheat_totloss, wheat_percloss)

results

write.table(results, 'impacts_layers.csv', sep=',')

