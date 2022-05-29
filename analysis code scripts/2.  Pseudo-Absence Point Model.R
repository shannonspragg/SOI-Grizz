# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)

# Bring in Data: ----------------------------------------------------------
soi.10k.buf <- st_read("Data/processed/SOI_10km_buf.shp")
warp.all <-st_read("Data/processed/warp_crop_10km_buf.shp") 


# Create a template raster ------------------------------------------------

grizz.dens <- rast("Data/original/grizz_dens.tif")
soi.vect <- project(vect(soi.10k.buf), crs(grizz.dens))
grizz.crop <- crop(grizz.dens, soi.vect, mask=TRUE)
# Make sure our presence points are all species reports: ------------------
  # Convert all of our species to 1's (not just bears):
#Let's not do this because it's confusing later

warp.all$anyConflict <- 1  # so this gives 1's for all species reports now
#unique(warp.all$bears) # checked - all good

# Generate Random Points for Pseudo-absences: -----------------------------
set.seed(2345)
p.abs.pts <- randomPoints(raster(grizz.crop), 11000)

#plot(grizz.dens)
#plot(p.abs.pts, add=TRUE) # This gives us our absence points!

  # Make this a data frame:
abs.pts.df <- data.frame(p.abs.pts)

  # Make these spatial points:
abs.pts.sf <- st_as_sf(abs.pts.df, coords= c("x","y"), crs= st_crs(grizz.crop))

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
abs.pts.sf['anyConflict'] <- 0

  # Reorder the columns to match:
abs.pts.sf <- abs.pts.sf[ , c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1)]



# Restructure Data frame: --------------------------------------------------
  ## Here we add our presence points to our absences
  # Join our all species presence points with the absence points:

all.conflict.pts.w.abs <- rbind(warp.all, abs.pts.sf)


# Plot these to check:
plot(st_geometry(all.conflict.pts.w.abs), add=TRUE)

# Save as New Df: ---------------------------------------------------------
st_write(all.conflict.pts.w.abs, "Data/processed/warp_pres.abs.shp", append=FALSE)





