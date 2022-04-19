# Pseudo-absence Binary Model: --------------------------------------------
  #### Here we will generate pseudo-absences to represent our 0's for the binary model with bear conflict reports (1's)


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)

# Bring in Data: ----------------------------------------------------------
soi.10k.buf <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km_buf.shp")
warp.all <-st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp_crop_10km_buf.shp") 

soi.rast <- raster("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km.tif")

# Make sure our presence points are all species reports: ------------------
  # Convert all of our species to 1's (not just bears):

warp.all['bears'] <- 1  # so this gives 1's for all species reports now
unique(warp.all$bears) # checked - all good

# Generate Random Points for Pseudo-absences: -----------------------------
set.seed(2345)
p.abs.pts <- randomPoints(soi.rast, 11000)

plot(soi.rast)
plot(p.abs.pts, add=TRUE) # This gives us our absence points!

  # Make this a data frame:
abs.pts.df <- data.frame(p.abs.pts)

  # Make these spatial points:
abs.pts.sf <- st_as_sf(abs.pts.df, coords= c("x","y"), crs= st_crs(warp.all))

  # Add the missing columns:
  # Let's try this manually:
abs.pts.sf['encontr_d'] <- NA
abs.pts.sf['encntr_dt'] <- NA
abs.pts.sf['spcs_nm'] <- NA
abs.pts.sf['encntr_lc'] <- NA
abs.pts.sf['encntr_dl'] <- 1
abs.pts.sf['encntr_y'] <- 0
abs.pts.sf['encntr_lt'] <- NA
abs.pts.sf['encntr_ln'] <- NA
abs.pts.sf['attrct_'] <- NA
abs.pts.sf['enctyp_'] <- NA
abs.pts.sf['otcm_nm'] <- NA
abs.pts.sf['grop_cd'] <- NA
abs.pts.sf['ttl_ncn'] <- NA
abs.pts.sf['bears'] <- 0  # so this shows as absences

  # Reorder the columns to match:
abs.pts.sf <- abs.pts.sf[ , c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,1)]



# Restructure Data frame: --------------------------------------------------
  ## Here we add our presence points to our absences
  # Join our all species presence points with the absence points:

all.conflict.pts.w.abs <- rbind(warp.all, abs.pts.sf)


# Plot these to check:
plot(st_geometry(all.conflict.pts.w.abs))

# Save as New Df: ---------------------------------------------------------
st_write(all.conflict.pts.w.abs, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp_pres.abs.shp")





