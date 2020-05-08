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


############

# Read data

s = readOGR(dsn = './data/natural_earth', layer = 'ne_50m_admin_0_countries')

# Create color ramp

colr = colorRampPalette(rev(brewer.pal(9, 'RdYlBu')))


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


##############

# Create plots

# Erosion cf

r = raster('./output/yl_compaction-to-total.tif')
r

levelplot(r, col.regions=colr, at=c(0,0.00025,0.0005,0.000751,0.001,0.01), zlim=c(0,0.001))

miat = seq(0,0.001, len=101)

levelplot(r,
          #maxpixels=1e6,
          
          col.regions=colr,                        # color ramp
          at=miat,                                 # color ramp breaks

          colorkey=list(
            space='right',                         # place legend
            labels=list(at=miat,                   # place legend labels
                        #labels=bin_labels,        # legend labels
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


breaks = c(0,0.00001,0.0001,0.001,0.01)

png(filename='C:/Users/Sonderegger/Documents/Sync/PhD/Next paper/Figures/erosion.png', width=1200, height=480)

func_plot(r, breaks)

dev.off()

