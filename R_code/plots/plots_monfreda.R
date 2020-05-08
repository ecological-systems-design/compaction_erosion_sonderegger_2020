#install.packages('rgdal')
#install.packages('raster')
#install.packages('rasterVis')
#install.packages('RColorBrewer')
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)


# Set working directory
#setwd('/home/sothomas/R_code/soil')
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


##############

# Read data

s = readOGR(dsn = './data/natural_earth', layer = 'ne_50m_admin_0_countries')

# Create color ramp

colr = colorRampPalette(brewer.pal(9, 'Greens'))


##################

# Create functions


# Function for colorbar bins

func_bins = function(breaks) {
  bin_length = max(breaks)/(length(breaks)-1)
  bin_center = bin_length/2
  
  bins = seq(0, max(breaks), len=length(breaks))
  
  bin_centers = seq(bin_center, max(breaks)-bin_center, len=length(breaks)-1)
  
  bin_labels = c()
  for (i in 1:(length(breaks)-2)) {
    bin_labels = c(bin_labels, paste0(toString(breaks[i]), '-', toString(breaks[i+1])))
  }
  bin_labels = c(bin_labels, paste0('> ', toString(breaks[length(breaks)-1])))
  
  return(list(bins, bin_centers, bin_labels))
}


# Function for plotting

func_plot = function(r, breaks) {
  
  b = func_bins(breaks)
  bins = b[[1]]
  bin_centers = b[[2]]
  bin_labels = b[[3]]
  
  levelplot(r,
            #maxpixels=1e6,
            
            col.regions=colr,                        # color ramp
            at=breaks,                               # color ramp breaks
            
            colorkey=list(
              space='right',                         # place legend
              at=bins,                               # colorkey breaks
              labels=list(at=bin_centers,            # place legend labels
                          labels=bin_labels,         # legend labels
                          cex=1.6)                   # legend font size
            ),
            
            margin=FALSE,                            # suppress marginal graphics
            scales=list(draw=FALSE),                 # suppress axis labels
            xlab=NULL,
            ylab=NULL,
            par.settings=list(
              axis.line=list(col='transparent')      # suppress axes and legend outline
            )
  ) +
    layer(sp.polygons(s, lwd=0.5))
}


func_cleanup_raster = function(raster, raster_reference) {
  raster_clean = crop(raster, extent(raster_reference))
  raster_clean[is.na(raster_reference)] = NA
  return(raster_clean)
}


#############################

mx_ts = mx_ts = raster('./data/prepared/mx_ts_yearly_10km_median.tif')

# Production

# Area harvested
wheat_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_HarvestedAreaHectares.tif')
wheat_area = func_cleanup_raster(wheat_area, mx_ts)

maize_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/maize/maize_HarvestedAreaHectares.tif')
maize_area = func_cleanup_raster(maize_area, mx_ts)

soybean_area = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/soybean/soybean_HarvestedAreaHectares.tif')
soybean_area = func_cleanup_raster(soybean_area, mx_ts)

# Actual yield
wheat_yield = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/wheat/wheat_YieldPerHectare.tif')
wheat_yield = func_cleanup_raster(wheat_yield, mx_ts)

maize_yield = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/maize/maize_YieldPerHectare.tif')
maize_yield = func_cleanup_raster(maize_yield, mx_ts)

soybean_yield = raster('./data/EarthStat/HarvestedAreaYield175Crops_Geotiff/soybean/soybean_YieldPerHectare.tif')
soybean_yield = func_cleanup_raster(soybean_yield, mx_ts)

wheat = wheat_area * wheat_yield / 1000
maize = maize_area * maize_yield /1000
soybean = soybean_area * soybean_yield / 1000


##############

# Create plots

# wheat
wheat_area

breaks = seq(0,1000, len=9)

png(filename='./output/wheat.png', width=1200, height=480)

func_plot(wheat_area, breaks)

dev.off()


# maize

breaks = seq(0,10, len=11)

png(filename='./output/maize.png', width=1200, height=480)

func_plot(maize, breaks)

dev.off()


# soybean

breaks = seq(0,10, len=11)

png(filename='./output/soybean.png', width=1200, height=480)

func_plot(soybean, breaks)

dev.off()
