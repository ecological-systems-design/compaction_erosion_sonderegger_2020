Code description

# Prepare missing shapes
0:  extract shapes of states/provinces for large countries missing in Ecoinvent

# Aggregation of characterization factors (CFs) for geo-units (countries, states)
1a-1: compaction top soil countries
1a-2: compaction top soil states
1b-1: compaction mid soil countries
1b-2: compaction mid soil states
1c-1: erosion countries
1c-2: erosion states

# Case study
2a: aggregate CF raster datasets to 10 km resolution (matching crop data)
2b: compute impacts
2c: compute contributions of different soil layers for the wheat production scenarios

# Global CFs
3: compute global CFs (median)

# Country and states CFs
4: aggregate for geounits