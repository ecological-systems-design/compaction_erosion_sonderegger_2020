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

start.time = Sys.time()


###########

# Read data

r_sdx_ts = raster('./compaction/characterization_factors/sdx_ts_yearly.tif')
r_k_ts = r_sdx_ts^2

r_sdx_ms = raster('./compaction/characterization_factors/sdx_ms_yearly.tif')
r_k_ms = r_sdx_ms^2

s = readOGR(dsn = './data/natural_earth', layer = 'ne_50m_admin_0_countries')


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
  
  bin_labels[[1]] = '=1'
  bin_labels[[2]] = '1-1.2'
  
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


# Create color ramp

#colr = colorRampPalette(brewer.pal(8, 'Blues'))
myColors = c('#ffffff', '#f1eef6', '#bdc9e1', '#74a9cf', '#2b8cbe', '#045a8d',
             '#ffffb2', '#fecc5c', '#fd8d3c', '#f03b20', '#bd0026')

myColors = c('#ffffff', '#f1eef6', '#bdc9e1', '#74a9cf', '#2b8cbe', '#045a8d',
             '#fff7bc', '#fec44f',
             '#fb6a4a', '#de2d26', '#a50f15')

myColors = c('#ffffff', '#f1eef6', '#bdc9e1', '#74a9cf', '#2b8cbe', '#045a8d',
             '#ffffd4', '#fed98e', '#fe9929', '#d95f0e', '#993404',
             '#fbb4b9', '#f768a1', '#c51b8a')

colr = colorRampPalette(myColors,space='Lab')


# Plots
r = r_sdx_ts
max = r@data@max
breaks = c(1,1.000000000001,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3,4,6,max)

png(filename='./plots/sdx_ts_yearly.png', width=1200, height=480)
func_plot(r, breaks)
dev.off()

r = r_sdx_ms
r = r_k_ms
max = r@data@max
breaks = c(1,1.000000000001,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3,4,6,max)

png(filename='./plots/sdx_ms_yearly.png', width=1200, height=480)
func_plot(r, breaks)
dev.off()

r = r_k_ts
max = r@data@max
breaks = c(1,1.000000000001,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3,4,6,max)

png(filename='./plots/k_ts_yearly.png', width=1200, height=480)
func_plot(r, breaks)
dev.off()

r = r_k_ms
max = r@data@max
breaks = c(1,1.000000000001,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3,4,6,max)

png(filename='./plots/k_ms_yearly.png', width=1200, height=480)
func_plot(r, breaks)
dev.off()

