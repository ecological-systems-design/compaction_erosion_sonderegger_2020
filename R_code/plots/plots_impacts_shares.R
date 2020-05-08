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

start.time = Sys.time()



################################

# RUN SCRIPT 2b_Impacts.R FIRST!

################################


# Production shares for maize (medium, conventional tillage)

m_prod_share = maize_potprod / maize_potprod_sum *100

cellStats(m_prod_share, sum)

# Create map
r = m_prod_share
s = readOGR(dsn = './data/natural_earth', layer = 'ne_50m_admin_0_countries')

# Create color ramp
#colr = colorRampPalette(brewer.pal(9, 'YlGnBu'))
myColors = c('#ffff88', '#a1dab4', '#41b6c4', '#2c7fb8', '#253494')
colr = colorRampPalette(myColors,space='Lab')

# Plots
png(filename='./plots/maize_m_c_prod.png', width=1200, height=480)
levelplot(r,
          xlim=c(-126,180), ylim=c(-48,64),
          maxpixels=1e6,
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=seq(0,0.005,0.0025),       # legend ticks and labels
                        cex=1.6)                   # font size
          ),
          
          margin=FALSE,                            # suppress marginal graphics
          scales=list(draw=FALSE),                 # suppress axis labels
          xlab=NULL,
          ylab=NULL,
          par.settings=list(
            axis.line=list(col='transparent')      # suppress axes and legend outline
          ),
          
          col.regions=colr,                        # color ramp
          at=seq(0,0.005, len=101)                 # color ramp breaks
) +
  layer(sp.polygons(s, lwd=0.5)) 
dev.off()


########################################################

# Impact shares for maize (medium, conventional tillage)

m_ylc_perc = yl_maize_m
m_ylc_perc[maize_area == 0] = 0
m_ylc_abs = m_ylc_perc * maize_potprod
m_ylc_abs_sum = cellStats(m_ylc_abs, sum)
m_ylc_share = m_ylc_abs / m_ylc_abs_sum *100

cellStats(m_ylc_share, sum)

# Create map
r = m_ylc_share
r[r>0.0012] = 0.0012

# Create color ramp
colr = colorRampPalette(rev(brewer.pal(5, 'RdYlBu')))

# Plots
png(filename='./plots/maize_m_c_comp_losses.png', width=1200, height=480)
my.at1 = c(0,0.0001,0.0002,0.0006,0.0012)
my.at2 = c(seq(0,0.0001,len=101),seq(0.00011,0.0002,len=101),seq(0.00021,0.0006,len=101),seq(0.00061,0.0012,len=101))
levelplot(r,
          xlim=c(-126,180), ylim=c(-48,64),
          maxpixels=1e6,
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=my.at1,                 # legend ticks and labels
                        cex=1.6)                   # font size
          ),
          
          margin=FALSE,                            # suppress marginal graphics
          scales=list(draw=FALSE),                 # suppress axis labels
          xlab=NULL,
          ylab=NULL,
          par.settings=list(
            axis.line=list(col='transparent')      # suppress axes and legend outline
          ),
          
          col.regions=colr,                        # color ramp
          at=my.at2                                # color ramp breaks
) +
  layer(sp.polygons(s, lwd=0.5)) 
dev.off()


m_yle_perc = yl_maize_eros
m_yle_perc[is.na(maize_area)] = NA
m_yle_perc[maize_area == 0] = NA
m_yle_perc[m_yle_perc < 0] = 0
m_yle_abs = m_yle_perc * maize_potprod
m_yle_abs_sum = cellStats(m_yle_abs, sum)
m_yle_share = m_yle_abs / m_yle_abs_sum *100

# Create map
r = m_yle_share
r[r>0.0012] = 0.0012

# Plots
png(filename='./plots/maize_m_c_eros_losses.png', width=1200, height=480)
levelplot(r,
          xlim=c(-126,180), ylim=c(-48,64),
          maxpixels=1e6,
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=my.at1,                 # legend ticks and labels
                        cex=1.6)                   # font size
          ),
          
          margin=FALSE,                            # suppress marginal graphics
          scales=list(draw=FALSE),                 # suppress axis labels
          xlab=NULL,
          ylab=NULL,
          par.settings=list(
            axis.line=list(col='transparent')      # suppress axes and legend outline
          ),
          
          col.regions=colr,                        # color ramp
          at=my.at2                                # color ramp breaks
) +
  layer(sp.polygons(s, lwd=0.5)) 
dev.off()



m_ylt_perc = m_ylc_perc + m_yle_perc
m_ylt_abs = m_ylt_perc * maize_potprod
m_ylt_abs_sum = cellStats(m_ylt_abs, sum)
m_ylct_share = m_ylc_abs / m_ylt_abs_sum *100

m_ylet_share = m_yle_abs / m_ylt_abs_sum *100

