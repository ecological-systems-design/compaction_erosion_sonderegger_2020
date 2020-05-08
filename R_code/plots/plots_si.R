#install.packages('rgdal')
#install.packages('raster')
#install.packages('rasterVis')
#install.packages('RColorBrewer')
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)


# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')


##############

# Read data

s = readOGR(dsn = './data/natural_earth', layer = 'ne_50m_admin_0_countries')

# Create color ramp

colr = colorRampPalette(brewer.pal(9, 'YlOrRd'))


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
            maxpixels=1e6,
            
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


##############

# Create plots

# k-factor

r = raster('./erosion/krls-factor/rasters/k-factor_250m.tif')
r

breaks = c(0,0.01,0.18,0.021,0.024,0.028,0.03,0.035,0.04,0.06117)

png(filename='./output/k-factor.png', width=1200, height=480)

func_plot(r, breaks)

dev.off()


# r-factor

r = raster('./erosion/krls-factor/rasters/r_shifted.tif')
r

breaks = c(0,100,200,400,700,1150,1700,3100,5200,7400,26785)

png(filename='./output/r-factor.png', width=1200, height=480)

func_plot(r, breaks)       

dev.off()


# ls-factor

r = raster('./erosion/krls-factor/rasters/ls-factor_mean_250m.tif')
r

breaks = c(0,0.1,0.2,0.5,0.8,1.2,2,3,5,10,326.36)

png(filename='./output/ls-factor.png', width=1200, height=480)

func_plot(r, breaks)

dev.off()


# krls-factor

r = raster('./erosion/krls-factor/rasters/krls-factor_median_1km.tif')
r

breaks = c(0,5,25,50,250,500,2500,5000,15028)

png(filename='./output/krls-factor.png', width=1200, height=480)

func_plot(r, breaks) 

dev.off()


breaks = c(0,1,1.2,1.4,1.6,1.8,2,4,cellStats(r1, max))
func_plot(r1, breaks)

breaks = c(0,1,1.2,1.4,1.6,1.8,2,4,cellStats(r2, max))
func_plot(r2, breaks)
