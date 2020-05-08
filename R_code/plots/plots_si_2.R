#install.packages('rgdal')
#install.packages('raster')
#install.packages('spatstat')
#install.packages('rasterVis')
#install.packages('RColorBrewer')
library(rgdal)
library(raster)
library(spatstat)
library(rasterVis)
library(RColorBrewer)


# The code calculates KRLS-factors of the RUSLE erosion model for a country and its states.
# Factors at 1km resolution are aggregated weighting them by cropland area.
# Lognormal distribution is assumed and the median (m) and a spread-factor (sd) covering 95% of the confidence intervall are computed:
# m / sd = 2.5%-percentile
# m * sd = 97.5%-percentile
# Results for the country are again calculated via Monte Carlo simulation and via weighted aggregation using m and sd of states.
# Results are compared.


# Set working directory
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil/TEST')


country = 'Switzerland'


###########

# Functions

func_lognorm_vector = function(m, sd) {
  
  x = runif(n=length(m), min=0, max=1)  # random generated number for each element
  
  v = qlnorm(x, meanlog=log(m), sdlog=log(sd))  # get value from distribution
  
  return(v)
}

func_get_lognormal_parameters = function(v) {
  
  mx = median(v)
  q_upper = as.numeric(quantile(v, 0.975))
  q_lower = as.numeric(quantile(v, 0.025))                
  sdx = (q_upper / q_lower)^(1/4)
  
  # dispersion-factor where 2.5%-percentile is 0
  if (q_lower == 0) {
    sdx = sqrt(q_upper / mx)
  }
  
  # dispersion-factor where median is 0
  m = mean(v)
  if (mx == 0) {
    sdx = sqrt(q_upper / m)
  }
  
  # dispersion-factor where mean is 0
  if (m == 0) {
    sdx = 1
  }
  
  return(c(mx, sdx))
}

func_get_lognormal_weighted_parameters = function(v, w) {
  
  mx = weighted.median(v, w)
  q_upper = as.numeric(weighted.quantile(v, w, probs=0.975))
  q_lower = as.numeric(weighted.quantile(v, w, probs=0.025))                
  sdx = (q_upper / q_lower)^(1/4)
  
  # dispersion-factor where 2.5%-percentile is 0
  if (q_lower == 0) {
    sdx = sqrt(q_upper / mx)
  }
  
  # dispersion-factor where median is 0
  x = v * w
  m = sum(x, na.rm=TRUE) / sum(w, na.rm=TRUE)
  if (mx == 0) {
    sdx = sqrt(q_upper / m)
  }
  
  # dispersion-factor where mean is 0
  if (m == 0) {
    sdx = 1
  }
  
  return(c(mx, sdx))
}


##############

# Computations

# Rasterfiles
r = raster('./krls-factor_median_1km.tif')
rw = raster('./landuse_area_weighting.tif')


# Zürich

s = readOGR(dsn = './natural_earth', layer = 'ne_10m_admin_1_states_provinces')
ch_states = s[s@data$admin == country,]

i = 6
  
shp = ch_states[i,]

id = as.character(shp@data$ne_id)
name = as.character(shp@data$name)

rm = crop(r, shp)
rm = mask(rm, shp)

rwm = crop(rw, shp)
rwm = mask(rwm, shp)

v = getValues(rm)
w = getValues(rwm)

w = w[!is.na(v)]
v = v[!is.na(v)]
v = v[w != 0]
w = w[w != 0]

params = func_get_lognormal_weighted_parameters(v, w)

m = params[1]
sd = params[2]


# Figures

lake = readOGR(dsn = '.', layer = 'lake_zurich')

png(filename='C:/Users/Sonderegger/polybox/DISS/presentation/zurich_krls.png')
colr = colorRampPalette(brewer.pal(6, 'YlOrRd'))
levelplot(rm,
          maxpixels=1e6,
          
          main=list('KRLS-factor', cex=1.6),
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=seq(0,600, len=7),      # legend ticks and labels
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
          at=seq(0,600, len=101)                  # color ramp breaks
) + layer(sp.polygons(shp, lwd=0.5), sp.polygons(lake, fill='steelblue1', border='blue', lwd=0.5))
dev.off()


png(filename='C:/Users/Sonderegger/polybox/DISS/presentation/zurich_agrarea.png')
colr = colorRampPalette(brewer.pal(6, 'YlGn'))
levelplot(rwm,
          maxpixels=1e6,
          
          main=list(expression(bold('Cropland '~(km^{2}))), cex=1.6),
          
          colorkey=list(
            space='right',                         # place legend
            labels=list(at=seq(0,0.35, len=8),   # legend ticks and labels
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
          at=seq(0,0.35, len=101)                # color ramp breaks
) + layer(sp.polygons(shp, lwd=0.5), sp.polygons(lake, fill='steelblue1', border='blue', lwd=0.5))
dev.off()


png(filename='C:/Users/Sonderegger/polybox/DISS/presentation/zurich_hist_pure.png', width=680, height=529)
hist(v, breaks=100, prob=T, ylim=c(0,0.02), main='', xlab='cropland weighted krls-factor', ylab='probability density')
curve(dlnorm(x, meanlog=log(m), sdlog=log(sd), log=F), add=T, col='red', lw=2)
abline(v=m, col='red', lw=2)
dev.off()  

