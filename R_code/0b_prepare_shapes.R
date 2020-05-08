#install.packages('rgdal')
#install.packages('raster')
library(rgdal)
library(raster)


# Set working directory
#setwd('/home/sothomas/R_code/soil')
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

# Set working directory
s = readOGR(dsn = './data/ne_10m_cultural', layer = 'ne_10m_admin_1_states_provinces')

states = s[s@data$admin == 'Australia' | s@data$admin == 'Brazil' | s@data$admin == 'Canada'
           | s@data$admin == 'China' | s@data$admin == 'India' | s@data$admin == 'Russia' 
           | s@data$admin == 'United States of America',]

writeOGR(states, './data/prepared', 'states',
         driver='ESRI Shapefile', overwrite_layer = TRUE, encoding='ISO-8859-1')
