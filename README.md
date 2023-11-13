# RhodoNiche-Projection
Scripts and examples used for the paper "Niche projection and species distribution modelling to predict current and future potential invasion of *Rhododendron ponticum* in France".

## Scripts
This repo includes two R scripts, i) a main script Script_RhNicheProj.R to do the niche projection analysis with environmental variables, and ii) the script for a complementary analysis Script_CompAnalysis.R, with bioclimatic and soil variables independently analyzed. 

Both analyzes the potential distribution of *R.ponticum subsp baeticum* (native to Iberic Peninsula) across introduction and invaded area, here France or British Isles, on periods between 1950 and 2100, based on three scenarios (SSP1-2.6, SSP3-7.0, SSP5-8.5). The aim is to projected the environmental variables (bioclimatic and soil) in a principal component analysis (PCA) to represent the environmental niche of *R. ponticum baeticum* and environmental conditions in introduction area together in a two-dimensional environmental space. The points in the introduction area that are close (overlapping) to those in the *R.poncticum* native range are selected, as it is considered that their environmental conditions are similar, and these conditions within the introduction area should then be suitable for *R.ponticum subsp baeticum*. These selected points are then reprojected onto a map (in this case, France or British Isles), representing the niche projection or species potential distribution. 

## Data
Data used: **required to download** [CHELSA](https://chelsa-climate.org/downloads/) bioclimatic variables (optimally periods 1981-2010, 2011-2040, 2041-2070, 2071-2100 and SSP1-2.6, SSP3-7.0, SSP5-8.5 for future periods). 
No necessary for [SoilGrids](https://www.isric.org/explore/soilgrids) variables, which are downloaded with function geodata::soil_world, directly with crop.env() function in script.

Data available: Shapefile for *R.ponticum subsp baeticum* species available in Native_range folder.

The latest version of R (>4.2.3) should be installed from [https://cran.r-project.org](https://cran.r-project.org/).

Contact: Tristan Ubaldi (tristan.ubaldi@u-picardie.fr)
