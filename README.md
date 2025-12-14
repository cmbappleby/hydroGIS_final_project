# Hydrologic Applications of GIS & Remote Sensing Final Project
### Project objectives
* Estimate post-fire soil erosion risk using Revised Universal Soil Loss Equation (RUSLE)
* Identify potential post-fire landslide areas using a simple GIS-based approach

To calculate average annual precipitation and mean rainy days per year, I used R. I also used R to retrieve the SSURGO soils tabular and spatial data for my study area, and joined Kfact to the spatial data before exporting as a geopackage to use in ArcGIS Pro.

I used Sentinel-2 L2A imagery to calculate dNBR, and I used Python and the scikit-fuzzy package to classify the dNBR raster into burn severity classes.
