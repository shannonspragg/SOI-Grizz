## ONA-GRIZZ-shannon
Below are descriptions of each analysis code script and how to reproduce the full analysis. --------------------------------------------------------

The data for all of the following scripts can be found at this googledrive link: 
https://drive.google.com/drive/u/0/folders/1mpUVwvzqOsUF-Kpu_uF9UdZchJLcakSi

The below scripts include the process we followed for collecting and cleaning our data, building a "master data frame", and running our data through both Bayesian logistic regressions. The order of the scripts that reproduce our analysis is as follows:


############# Original Data Prep.R ###################
This script prepares the raw data for our predictors from a collective of different sources. It produces the boundary for our SOI region, preps our agricultural predictor and census subdivision data, and the human population density predictor.


################# Pseudo-Abs Point Model.R #######################
This script produces the general conflict data frame by developing pseudo-absence points for our warp conflict data. The result of this is a larger data frame with presences representing all species conflict reports, and absences drawn from random background points.


################ WARP All Species Data Prep.R #######################
This script is used to prepare the first part of the analysis data frame by adding our predictors into the two data sets. Here, we attribute distance to protected and metro area variables, dominant farm type and total farm counts, and ccs region ID's to the points in each of our data sets. The result of this is a master data frame that is semi-complete with our predictors (see next script for full completion).



############ Extracting Rasters to Master Dataframe.R ####################
In this script we be bring in the produced "master data frame" resulting from our All Species Master Prep script as a sf data frame and our rasters for the grizz increase , biophysical CS, and grizzly density (bear habitat suitability or BHS). We buffer the the WARP points by 500m and convert them to Spatvectors, overlay each individual raster with the points, and then extract the attributes from each raster to each WARP point by location. The result is three additional columns (one for each raster) in the master WARP data frame, representing these values. Lastly, we check for any NA's present within our variable columns. This produces our final "master data frame" which can now be used in our regression script.


############## Prepping Predictor Rasters.R #####################
In this script, we bring in the rasters for each of our predictors and ensure that they are cropped down to the SOI boundary and all have the same extents and projections. We produce the rasters to represent our distance to protected and metro areas. These rasters will be used in the latter half of our analysis to produce our probability of conflict maps.



############## Bayes Pseudo-Abs Regression.R #####################
This script is where we bring in our pres-abs master data frame and run a series of Bayesian regressions with individual and combined covariates from the variable columns that we produced above. Here we run individual and full models, run model comparisons, validation processes, and produce our probability of general conflict raster.


############## Conflict Bears Bayes Regression.R #####################
This script is where we bring in our warp-only master data frame and run a series of Bayesian regressions with individual and combined covariates from the variable columns that we produced above. Here we run individual and full models, run model comparisons, validation processes, and produce our probability of bear conflict raster.
