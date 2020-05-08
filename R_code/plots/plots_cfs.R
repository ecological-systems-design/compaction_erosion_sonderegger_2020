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


############

# Read data

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
                          cex=3.2),                  # legend font size
              width = 3
            ),
            
            margin=FALSE,                            # suppress marginal graphics
            scales=list(draw=FALSE),                 # suppress axis labels
            xlab=NULL,
            ylab=NULL,
            par.settings=list(
              axis.line=list(col='transparent')      # suppress axes and legend outline
            )
  ) +
    layer(sp.polygons(s, lwd=1))
}


##############

# Create plots

# Compaction top soil

r = raster('./compaction/characterization_factors/mx_ts_yearly.tif')
r

# Create color ramp
colr = colorRampPalette(brewer.pal(9, 'Reds'))


png(filename='./plots/compaction_ts.png', width=2400, height=960)

levelplot(r,
          maxpixels=1e6,
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=seq(0,0.12,0.06),       # legend ticks and labels
                        cex=3.2),                  # font size
            width = 3
          ),
          
          margin=FALSE,                            # suppress marginal graphics
          scales=list(draw=FALSE),                 # suppress axis labels
          xlab=NULL,
          ylab=NULL,
          par.settings=list(
            axis.line=list(col='transparent')      # suppress axes and legend outline
          ),
          
          col.regions=colr,                        # color ramp
          at=seq(0,0.12, len=101)                  # color ramp breaks
) +
  layer(sp.polygons(s, lwd=1))       

dev.off()


# Compaction mid soil

r = raster('./compaction/characterization_factors/mx_ms_yearly.tif')
r

# Create color ramp
colr = colorRampPalette(brewer.pal(9, 'Purples'))

breaks = c(0,0.1,0.5,1,5,10,50,100,301)

png(filename='./plots/compaction_ms.png', width=2400, height=960)

levelplot(r,
          maxpixels=1e6,
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=seq(0,0.024,0.012),     # legend ticks and labels
                        cex=3.2),                  # font size
            width = 3
          ),
          
          margin=FALSE,                            # suppress marginal graphics
          scales=list(draw=FALSE),                 # suppress axis labels
          xlab=NULL,
          ylab=NULL,
          par.settings=list(
            axis.line=list(col='transparent')      # suppress axes and legend outline
          ),
          
          col.regions=colr,                        # color ramp
          at=seq(0,0.024, len=101)                 # color ramp breaks
) +
  layer(sp.polygons(s, lwd=1))  

dev.off()


# Erosion cf

r = raster('./erosion/krls-factor/rasters/krls-factor_median_1km.tif')
r

# Create color ramp
colr = colorRampPalette(brewer.pal(9, 'RdPu'))

breaks = c(0,5,10,50,100,500,1000,15000)

png(filename='./plots/krls-factor.png', width=2400, height=960)

func_plot(r, breaks)

dev.off()

