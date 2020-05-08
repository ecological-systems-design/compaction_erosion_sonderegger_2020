Data sources

- Stoessel 2018:
	- soil water content prepared: re-calculate from Stoessel or available on request
	- clay content: https://www.research-collection.ethz.ch/handle/20.500.11850/253177
- soil moisture time series: http://www.waterandclimatechange.eu/about/watch-20th-century-model-output-datasets
- WaterGAP_availability: available on request
________________

Code description

# Data preparation
0a: correct origin of raster datasets from Stoessel et al. 2018
0b: compute yearly mean of soil water content
0c: compute weighting map based on size of cell and share of cropland within a cell

# Dispersion factors
1a: compute dispersion factors of soil moisture
1b: disaggregate dispersion factor from 0.5 arc degrees to 30 arc seconds (1 km) resolution

# Yearly characterization factors (CFs)
2a-1: compute yearly CFs for top soil including Monte Carlo simulation for uncertainty distribution
2a-2: compute yearly CFs for bottom soil including Monte Carlo simulation for uncertainty distribution
2b-1: merge tiles computed in 2a (run 4 times: top soil & mid soil, median & dispersion factor)
2b-2: clear folders with tiles
2b-3: (optional) adjust NA values for mid soil (same as for top soil)
2b-4: compute CFs for bottom soil from CFs for mid soil

# Monthly characterization factors (CFs)
same as for yearly CFs
