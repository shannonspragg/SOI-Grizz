# Overlaying WARP Points with CS Outputs ----------------------------------
## Here we bring in the predictor rasters and overlay each individual one with the WARP points, buffering the points by 500m, and
# then extract the attributes from each raster to each WARP point by location. The result should be the creation of three additional 
# columns (one for each raster) in the master WARP df, representing these values.

# Should have: extracted values from BHS (the grizz_density estimate), Biophysical CS ( biophys raster) , 
# and  griz_inc survey values (not cs, extracted from normal raster).

# Load Packages -----------------------------------------------------------
library(sf)
#library(raster)
library(tidyverse)
library(sp)
library(terra)
library(rgdal)

# Bring in Data: ----------------------------------
  # Our WARP points master df:
warp.all.sp <- st_read("Data/processed/warp.master.shp")
  # Our pres-abs ponts master df:
pres.abs.master <- st_read("Data/processed/pres.abs.master.shp")

  # Predictor Rasters:
biophys.cum.curmap <- rast("Data/original/cum_currmap.tif") # use this one
  # Cumulative current flow shows the total current for each landscape pixel
  # Normalized shows the degree to which a pixel has more or less current than expected under resistance-free conditions (cumulative current flow divided by flow potential)

  # Just survey response layer (not CS):
grizz.inc.rast <- rast("Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
  # responded “I would like to see grizzlies increase or increase substantially” in response to “how would you like to see grizzly 
  # populations respond in the next several years?” 

  # BHS layer:
grizz.dens <- rast("Data/original/grizz_dens.tif") # Estimated grizzly density for the region
plot(grizz.dens)

  # SOI Boundary and Raster for template:
soi.10k.boundary <- st_read("Data/processed/SOI_10km_buf.shp")
#soi.rast <- terra::rast("Data/processed/SOI_10km.tif") # SOI Region 10km buffer raster

 # Human Density for SOI:
hm.dens <- terra::rast("Data/processed/human_dens_crop.tif") # SOI Region 10km

# Check / Set CRS for Raster and Points -----------------------------------
  # Match the projection and CRS of the current map to the resistance maps:
#only hm.dens and griz increas rast are in the wrong projection
grizz.dens.crop <- terra::crop(grizz.dens, vect(soi.10k.boundary))
grizz.dens.crop <- mask(grizz.dens, vect(soi.10k.boundary))
biophys.curmap.crop <- terra::crop(biophys.cum.curmap, vect(soi.10k.boundary))
biophys.curmap.crop <- terra::mask(biophys.curmap.crop, vect(soi.10k.boundary))
hm.dens.proj <- terra::project(hm.dens, grizz.dens.crop)
griz.inc.crop <- terra::crop(grizz.inc.rast, terra::project(vect(soi.10k.boundary), grizz.inc.rast))
griz.inc.crop <- terra::mask(griz.inc.crop, vect(soi.10k.boundary))
griz.inc.proj <- terra::project(griz.inc.crop, grizz.dens.crop)

# Buffer the WARP Points (Before Overlay) --------------------------------------------------
# Here we buffer the WARP and ppres-abs points by 5km before extracting the attributes from the current maps
warp.all.buf <- warp.all.sp %>% 
  st_buffer(., 5000)
plot(st_geometry(warp.all.buf)) # Check the buffers

pres.abs.buf <- pres.abs.master %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

# Let's Turn the Buffered Points into a SpatVector:
warp.sv.buf <- vect(warp.all.buf)
pres.abs.sv.buf <- vect(pres.abs.buf)

# Plot them together to see if projection truly is same:
plot(griz.inc.proj)
plot(warp.sv.buf, add = TRUE) 

plot(grizz.dens.crop)
plot(warp.sv.buf, add = TRUE) 

plot(biophys.curmap.crop)
plot(warp.sv.buf, add = TRUE) 

plot(hm.dens.proj)
plot(pres.abs.sv.buf, add = TRUE) 


# Overlay WARP Points with CS Raster  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
warp.biophys.b.ext <- terra::extract(biophys.curmap.crop, warp.sv.buf, mean, na.rm = TRUE)  # This gives us the mean value of each buffered area --> what we want!
warp.grizz.inc.b.ext <- terra::extract(griz.inc.proj, warp.sv.buf, mean, na.rm = TRUE) 
warp.bhs.b.extract <- terra::extract(grizz.dens.crop, warp.sv.buf, mean, na.rm = TRUE) 
warp.dens.b.ext <- terra::extract(hm.dens.proj, warp.sv.buf, mean, na.rm = TRUE) 

pres.abs.biophys.b.ext <- terra::extract(biophys.curmap.crop, pres.abs.sv.buf, mean, na.rm = TRUE)  # This gives us the mean value of each buffered area --> what we want!
pres.abs.grizz.inc.b.ext <- terra::extract(griz.inc.proj, pres.abs.sv.buf, mean, na.rm = TRUE) 
pres.abs.bhs.b.extract <- terra::extract(grizz.dens.crop, pres.abs.sv.buf, mean, na.rm = TRUE) 
pres.abs.dens.b.ext <- terra::extract(hm.dens.proj, pres.abs.sv.buf, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
warp.all.sp$Biophys <- warp.biophys.b.ext[,2]  
warp.all.sp$GrizzInc <- warp.grizz.inc.b.ext[,2]
warp.all.sp$BHS <- warp.bhs.b.extract[,2]
warp.all.sp$Human_Dens <- warp.dens.b.ext[,2] # Number of persons per square kilometer

pres.abs.master$Biophys <- pres.abs.biophys.b.ext[,2]
pres.abs.master$GrizzInc <- pres.abs.grizz.inc.b.ext[,2]
pres.abs.master$BHS <- pres.abs.bhs.b.extract[,2]
pres.abs.master$Human_Dens <- pres.abs.dens.b.ext[,2] 

# Check for NA's:
which(is.na(warp.all.sp$Biophys)) #none
which(is.na(warp.all.sp$BHS)) #none
which(is.na(warp.all.sp$GrizzInc)) # none
which(is.na(warp.all.sp$Human_Dens)) 

which(is.na(pres.abs.master$Biophys)) #none
which(is.na(pres.abs.master$BHS)) #none
which(is.na(pres.abs.master$GrizzInc)) # none
which(is.na(pres.abs.master$Human_Dens)) 
which(is.na(pres.abs.reproj$Human_Dens)) # none

# Save this as new file ---------------------------------------------------

st_write(warp.all.sp, "Data/processed/warp_final.shp", append = FALSE)
st_write(pres.abs.master, "Data/processed/pres_abs_final.shp", append=FALSE)

# Save projected cropped rasters ------------------------------------------

terra::writeRaster(griz.inc.proj, "Data/processed/grizz_inc_SOI_10km.tif", overwrite=TRUE)
terra::writeRaster(biophys.curmap.crop, "Data/processed/biophys_SOI_10km.tif", overwrite=TRUE)
terra::writeRaster(grizz.dens.crop, "Data/processed/bhs_SOI_10km.tif", overwrite=TRUE)
terra::writeRaster(hm.dens.proj, "Data/processed/human_dens_SOI_10km.tif" , overwrite=TRUE)
