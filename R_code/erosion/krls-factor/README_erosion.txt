Data sources

- CGIAR-CSI digital elevation model (DEM): https://cgiarcsi.community/data/srtm-90m-digital-elevation-database-v4-1/
- GMTED2010 digital elevation model: https://topotools.cr.usgs.gov/GMTED_viewer/gmted2010_global_grids.php (be75_grd)
- soil texture data: http://data.isric.org/geonetwork/srv/eng/catalog.search#/home
- R-factor: https://esdac.jrc.ec.europa.eu/content/global-rainfall-erosivity
________________

Code description

# LS-factor
1 compute LS-factors for each tile from the CGIAR-CSI DEM (90 m) using the SAGA GIS via the RSAGA package, aggregate to 250 m resolution
2 compute LS-factors for each UTM zone for the GMTED2010 DEM (250 m) using the SAGA GIS via the RSAGA package
3 aggregate tiles for the GMTED LS-factors
4 aggregate tiles for the CGIAR LS-factors
5 correct the origin of the GMTED raster

# K-factor
6 calculate k-factor
7 aggregate to larger tiles and correct the origin
8 merge large k-factor tiles

# Finalize LS-raster 
9 "clear" GMTED raster with NAs from k-factor file
10 merge CGIAR and GMTED raster datasets
11 crop LS-raster to match other raster datasets

# KLS-factor
12 compute KLS-factor, cut raster into tiles for parallel aggregation to 1 km resolution
13 mosaic KLS-tiles

# KRLS-factor
14 correct origin of R-factor raster
15 compute KRLS-factor

