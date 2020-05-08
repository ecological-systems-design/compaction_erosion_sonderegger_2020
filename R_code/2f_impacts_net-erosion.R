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
maize_potprod_sum = cellStats(maize_potprod, sum)
soybean_potprod_sum = cellStats(soybean_potprod, sum)

# RESULTS (Table S4)

a = c(cellStats(wheat_actarea, sum) / 1e6, cellStats(maize_actarea, sum) / 1e6, cellStats(soybean_actarea, sum) / 1e6)
b = c(cellStats(wheat_prod, sum) / cellStats(wheat_actarea, sum) / df[['crop_cycles']][1],
      cellStats(maize_prod, sum) / cellStats(maize_actarea, sum) / df[['crop_cycles']][4],
      cellStats(soybean_prod, sum) / cellStats(soybean_actarea, sum) / df[['crop_cycles']][7])
c = c(cellStats(wheat_potprod, sum) / cellStats(wheat_actarea, sum) / df[['crop_cycles']][1],
      cellStats(maize_potprod, sum) / cellStats(maize_actarea, sum) / df[['crop_cycles']][4],
      cellStats(soybean_potprod, sum) / cellStats(soybean_actarea, sum) / df[['crop_cycles']][7])
d = c(df[['crop_cycles']][1], df[['crop_cycles']][4], df[['crop_cycles']][7])
e = c(cellStats(wheat_prod, sum) / 1e6, cellStats(maize_prod, sum) / 1e6, cellStats(soybean_prod, sum) / 1e6)
f = c(wheat_potprod_sum / 1e6, maize_potprod_sum / 1e6, soybean_potprod_sum / 1e6)

ts4 = data.frame(a, b, c, d, e, f)
ts4


################################

# Calculation of erosion impacts

################################

r_w = raster('./data/prepared/krls-factor_10km_weighted_median.tif')
r = raster('./data/prepared/krls-factor_10km_median.tif')
r_w[r_w == 0] = NA
krls = cover(r_w, r)

conversion = conversion_factor * time_horizon

############################

# CONSIDERING SOIL FORMATION
# ( CP-factor [-] * KRLS-factor [t/ha-yr] - soil formation [t/ha-yr] )
#                           conversion factor [% yield loss/(t/ha-yr)]
#                                       = % yield loss * ha-yr / ha-yr

yl_wheat_eros = (df[['cp']][1] * krls - 0.75) * conversion
yl_wheat_eros[yl_wheat_eros > 100] = 100
yl_maize_eros = (df[['cp']][4] * krls - 0.75) * conversion
yl_maize_eros[yl_maize_eros > 100] = 100
yl_soybean_eros = (df[['cp']][7] * krls - 0.75) * conversion
yl_soybean_eros[yl_soybean_eros > 100] = 100

# Erosion losses

# Erosion losses per ha-yr * potential yield per crop cycle * area * cropy cycles per year
# = Compaction losses per ha-yr * potential production
# %_yield_loss*ha-yr/ha-yr * t/ha/cc * ha  * cc/yr = t / yr

# Conventional

# Wheat
wheat_eros_losses = yl_wheat_eros / 100 * wheat_potprod
wheat_eros_totloss = cellStats(wheat_eros_losses, sum)
wheat_eros_percloss = cellStats(wheat_eros_losses, sum) / wheat_potprod_sum * 100

# Maize
maize_eros_losses = yl_maize_eros / 100 * maize_potprod
maize_eros_totloss = cellStats(maize_eros_losses, sum)
maize_eros_percloss = cellStats(maize_eros_losses, sum) / maize_potprod_sum * 100

# Soybean
soybean_eros_losses = yl_soybean_eros / 100 * soybean_potprod
soybean_eros_totloss = cellStats(soybean_eros_losses, sum)
soybean_eros_percloss = cellStats(soybean_eros_losses, sum) / soybean_potprod_sum * 100


# Collect erosion results
eros_n1_abs_loss = c(wheat_eros_totloss, maize_eros_totloss, soybean_eros_totloss)
eros_n1_perc_loss = c(wheat_eros_percloss, maize_eros_percloss, soybean_eros_percloss)


########################

# CONSIDERING NATURAL CP
# ( ( CP-factor [-] - CP-factor PNV [-] ) * KRLS-factor [t/ha-yr] )
#                      * conversion factor [% yield loss/(t/ha-yr)]
#                                    = % yield loss * ha-yr / ha-yr

yl_wheat_eros = (df[['cp']][1] - 0.01) * krls * conversion
yl_wheat_eros[yl_wheat_eros > 100] = 100
yl_maize_eros = (df[['cp']][4] - 0.01) * krls * conversion
yl_maize_eros[yl_maize_eros > 100] = 100
yl_soybean_eros = (df[['cp']][7] - 0.01) * krls * conversion
yl_soybean_eros[yl_soybean_eros > 100] = 100

# Erosion losses

# Erosion losses per ha-yr * potential yield per crop cycle * area * cropy cycles per year
# = Compaction losses per ha-yr * potential production
# %_yield_loss*ha-yr/ha-yr * t/ha/cc * ha  * cc/yr = t / yr

# Conventional

# Wheat
wheat_eros_losses = yl_wheat_eros / 100 * wheat_potprod
wheat_eros_totloss = cellStats(wheat_eros_losses, sum)
wheat_eros_percloss = cellStats(wheat_eros_losses, sum) / wheat_potprod_sum * 100

# Maize
maize_eros_losses = yl_maize_eros / 100 * maize_potprod
maize_eros_totloss = cellStats(maize_eros_losses, sum)
maize_eros_percloss = cellStats(maize_eros_losses, sum) / maize_potprod_sum * 100

# Soybean
soybean_eros_losses = yl_soybean_eros / 100 * soybean_potprod
soybean_eros_totloss = cellStats(soybean_eros_losses, sum)
soybean_eros_percloss = cellStats(soybean_eros_losses, sum) / soybean_potprod_sum * 100


# Collect erosion results
eros_n2_abs_loss = c(wheat_eros_totloss, maize_eros_totloss, soybean_eros_totloss)
eros_n2_perc_loss = c(wheat_eros_percloss, maize_eros_percloss, soybean_eros_percloss)


##############

# COMBINED NET

# ( ( CP-factor [-] - CP-factor PNV [-] ) * KRLS-factor [t/ha-yr] - soil formation [t/ha-yr]) )
#                                                  * conversion factor [% yield loss/(t/ha-yr)]
#                                                                = % yield loss * ha-yr / ha-yr

yl_wheat_eros = ((df[['cp']][1] - 0.01) * krls - 0.75) * conversion
yl_wheat_eros[yl_wheat_eros > 100] = 100
yl_maize_eros = ((df[['cp']][4] - 0.01) * krls - 0.75) * conversion
yl_maize_eros[yl_maize_eros > 100] = 100
yl_soybean_eros = ((df[['cp']][7] - 0.01) * krls - 0.75) * conversion
yl_soybean_eros[yl_soybean_eros > 100] = 100

# Erosion losses

# Erosion losses per ha-yr * potential yield per crop cycle * area * cropy cycles per year
# = Compaction losses per ha-yr * potential production
# %_yield_loss*ha-yr/ha-yr * t/ha/cc * ha  * cc/yr = t / yr

# Conventional

# Wheat
wheat_eros_losses = yl_wheat_eros / 100 * wheat_potprod
wheat_eros_totloss = cellStats(wheat_eros_losses, sum)
wheat_eros_percloss = cellStats(wheat_eros_losses, sum) / wheat_potprod_sum * 100

# Maize
maize_eros_losses = yl_maize_eros / 100 * maize_potprod
maize_eros_totloss = cellStats(maize_eros_losses, sum)
maize_eros_percloss = cellStats(maize_eros_losses, sum) / maize_potprod_sum * 100

# Soybean
soybean_eros_losses = yl_soybean_eros / 100 * soybean_potprod
soybean_eros_totloss = cellStats(soybean_eros_losses, sum)
soybean_eros_percloss = cellStats(soybean_eros_losses, sum) / soybean_potprod_sum * 100


# Collect erosion results
eros_n3_abs_loss = c(wheat_eros_totloss, maize_eros_totloss, soybean_eros_totloss)
eros_n3_perc_loss = c(wheat_eros_percloss, maize_eros_percloss, soybean_eros_percloss)


##############################

# Collect results in dataframe

##############################

activity = c('wheat', 'maize', 'soybean')

results = data.frame(row.names=1, activity, eros_n1_abs_loss, eros_n2_abs_loss, eros_n3_abs_loss)

results

write.table(results, 'impacts_net.csv', sep=',')


