# Prep Predictor Rasters: --------------------------------------------
  # Here we bring and produce in our predictor rasters and make sure they are all cropped to SOI and equally projected:

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)

# Load Data: --------------------------------------------------------------

  # Grizzinc:  UPDATE THIS WITH NEW DATA
grizzinc.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/original/Grizz Increase/grizz.increase.map.fixed.tif") 

  # Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/original/Grizz Density/grizz_dens.tif")

  # Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/original/Biophysical CS/cum_currmap.tif") 

  # SOI Region for plotting:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km_buf.shp")
soi.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km.tif")
  
  # PA and Metro Data: (need to be cropped)
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/original/CAN Protected Areas/Parks_Combined2.shp") # Clayton's data
bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/Data/original/BC Metro Areas/CNCNSSMTRR_polygon.shp")
  
  # Extent Grizzly Populations:
extent.grizz <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/Extent Grizzly Pop Units.shp")
 
  # Human Density for SOI:
hm.dens <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/human_dens.tif") # SOI Region 10km

  
################################# First, we need to produce our Distance to PA, Metro, and Grizzly Pop Rasters:

# Check Projections: ------------------------------------------------------

bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=crs(soi.rast))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=crs(soi.rast))
soi.bound.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(soi.rast))
grizz.pop.reproj <- st_make_valid(extent.grizz) %>% 
  st_transform(crs=crs(soi.rast))

  # Check to see if they match:
st_crs(soi.bound.reproj) == st_crs(bc.PAs.reproj) # [TRUE] 
st_crs(metro.reproj) == st_crs(soi.bound.reproj) # [TRUE]
st_crs(grizz.pop.reproj) == st_crs(soi.bound.reproj) # [TRUE]

# Crop PA's & Metro to SOI Region: ----------------------------------------
  # Crop these points to the SOI:
PAs.soi.crop <- st_intersection(bc.PAs.reproj, soi.bound.reproj)
plot(st_geometry(PAs.soi.crop))

metro.soi.crop <- st_intersection(metro.reproj, soi.bound.reproj)
plot(st_geometry(metro.soi.crop))
plot(st_geometry(soi.bound.reproj), add=TRUE) # This works

  # Save these:
st_write(PAs.soi.crop, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/soi.PAs.10km.buf.shp")
st_write(metro.soi.crop, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/soi.metro.10km.buf.shp")

  # Buffer our region by 5km, so we have a space of 15km total (to account for PA's just outside of our SOI 10km boundary):
soi.15km.buf <- soi.bound.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(soi.15km.buf)) # Check the buffers

soi.pas.15km.buf <- st_intersection(bc.PAs.reproj, soi.15km.buf)
plot(st_geometry(soi.pas.15km.buf)) # Check the buffers


# Rasterize our Points & Polygons: ----------------------------------------

  # Make our data spatvectors:
PAs.soi <- vect(PAs.soi.crop) 
metro.soi.sv <- vect(metro.soi.crop)
grizz.pop.sv <- vect(extent.grizz)

# Create a Continuous Raster for Cell Distance to PA's: -------------------

  # Do this for our variables:
dist.pa.raster <- terra::distance(soi.rast, PAs.soi) 

dist.met.raster <- terra::distance(soi.rast, metro.soi.sv) 

dist.grizz.pop.raster <- terra::distance(soi.rast, grizz.pop.sv) 

  # Check this to see if it looks right:
plot(dist.pa.raster) # Plot our PAs
plot(dist.met.raster)  ## Plot our metro areas
plot(dist.grizz.pop.raster)  # Plot our grizz pops

  # Make sure our rasters are in km:
dist.pa.raster <- conv_unit(dist.pa.raster,"m","km") # There we go
dist.met.raster <- conv_unit(dist.met.raster,"m","km")
dist.grizz.pop.raster <- conv_unit(dist.grizz.pop.raster,"m","km")

names(dist.pa.raster)[names(dist.pa.raster) == "OBJECTID"] <- "Distance to Nearest PA (km)"
names(dist.met.raster)[names(dist.met.raster) == "OBJECTID"] <- "Distance to Nearest Metro (km)"
names(dist.grizz.pop.raster)[names(dist.grizz.pop.raster) == "OBJECTID"] <- "Distance to Nearest Extent Grizzly Pop (km)"

  # Invert distances: we want the highest raster value to be the closest (lowest distance) to our predictors:
dist.pa.rast.invert <- 1 - dist.pa.raster
dist.met.rast.invert <- 1 - dist.met.raster
dist.grizz.rast.invert <- 1 - dist.grizz.pop.raster


  # Plot to check:
plot(dist.pa.rast.invert) # This just makes the 0km distance the "highest" value (i.e. closest in proximity)
plot(dist.met.rast.invert)
plot(dist.grizz.rast.invert)


######################################## Now we Match All of our Rasters:

# Check Projections: ------------------------------------------------------
  # GrizzInc Map:
grizz.inc.reproj <- terra::project(grizz.inc.rast, crs(soi.rast))  
  # Bear Density (BHS) Estimate:
bhs.reproj <- terra::project(bhs.rast, crs(soi.rast))
  # Biophys Map:
biophys.reproj <- terra::project(biophys.rast, crs(soi.rast))
  # Human Density:
hm.dens.reproj <- terra::project(hm.dens, crs(soi.rast))


crs(soi.rast) == crs(grizz.inc.reproj) #TRUE
crs(grizz.inc.reproj) == crs(grizz.dens.reproj) #TRUE
crs(biophys.rast) == crs(hm.dens.reproj) #TRUE
crs(hm.dens.reproj) == crs(dist.pa.rast.invert)
crs(dist.pa.rast.invert) == crs(dist.met.rast.invert)
crs(dist.met.rast.invert) == crs(dist.grizz.rast.invert)


  # Crop these Rasters:
grizzinc.crop <- terra::crop(grizz.inc.reproj, soi.rast)  
biophys.crop <- terra::crop(biophys.reproj, soi.rast)
bhs.crop <- terra::crop(bhs.reproj, soi.rast)
d2pa.crop <- terra::crop(dist.pa.raster, soi.rast)
d2met.crop <- terra::crop(dist.met.raster, soi.rast)
d2grizzpop.crop <- terra::crop(dist.grizz.pop.raster, soi.rast)

d2pa.inv.crop <- terra::crop(dist.pa.rast.invert, soi.rast)
d2met.inv.crop <- terra::crop(dist.met.rast.invert, soi.rast)
d2grizzpop.inv.crop <- terra::crop(dist.grizz.rast.invert, soi.rast)


  # Resample to match extents and res:
grizzinc.rsmple <- resample(grizzinc.crop, soi.rast, method='bilinear')
biophys.rsmple <- resample(biophys.crop, soi.rast, method='bilinear')
bhs.rsmple <- resample(bhs.crop, soi.rast, method='bilinear')
d2pa.rsmpl <- resample(d2pa.crop, soi.rast, method='bilinear')
d2met.rsmpl <- resample(d2met.crop, soi.rast, method='bilinear')
d2grizz.pop.rsmpl <- resample(d2grizzpop.crop, soi.rast, method='bilinear')
hm.dens.rsmple <- resample(hm.dens.reproj, soi.rsmple, method='bilinear')

d2pa.inv.rsmpl <- resample(d2pa.inv.crop, soi.rast, method='bilinear')
d2met.inv.rsmpl <- resample(d2met.inv.crop, soi.rast, method='bilinear')
d2grizz.pop.inv.rsmpl <- resample(d2grizzpop.inv.crop, soi.rast, method='bilinear')

  # Plot Check:
soi.bound.vect <- vect(soi.bound.reproj)

plot(grizzinc.rsmple)
plot(soi.bound.vect, add=TRUE)

plot(biophys.rsmple)
plot(soi.bound.vect, add=TRUE)

plot(bhs.rsmple)
plot(soi.bound.vect, add=TRUE)

plot(d2pa.rsmpl)
plot(soi.bound.vect, add=TRUE)

plot(d2met.rsmpl)
plot(soi.bound.vect, add=TRUE)

plot(d2grizz.pop.rsmpl)
plot(soi.bound.vect, add=TRUE)

plot(hm.dens.rsmple)
plot(soi.bound.vect, add=TRUE) 


# Cut these down to the SOI Boundary: -------------------------------------

grizzinc.soi <- terra::mask(grizzinc.rsmple, soi.bound.vect) 
biophys.soi <- terra::mask(biophys.rsmple, soi.bound.vect) 
bhs.soi <- terra::mask(bhs.rsmple, soi.bound.vect) 
d2pa.soi <- terra::mask(d2pa.rsmpl, soi.bound.vect) 
d2met.soi <- terra::mask(d2met.rsmpl, soi.bound.vect) 
d2grizzpop.soi <- terra::mask(d2grizz.pop.rsmpl, soi.bound.vect) 
hm.dens.soi <- terra::mask(hm.dens.rsmple, soi.bound.vect) # BEA-UTIFUL!

d2pa.inv.soi <- terra::mask(d2pa.inv.rsmpl, soi.bound.vect) 
d2met.inv.soi <- terra::mask(d2met.inv.rsmpl, soi.bound.vect) 
d2grizzpop.inv.soi <- terra::mask(d2grizz.pop.inv.rsmpl, soi.bound.vect) 

plot(biophys.soi)
plot(bhs.soi)
plot(d2pa.soi)
plot(d2met.soi)
plot(d2grizzpop.soi)
plot(hm.dens.soi)

  # Fix the column names:
names(grizzinc.soi)[names(grizzinc.soi) == "grizz.increase.map.fixed"] <- "Support for Grizzly Increase"
names(biophys.soi)[names(biophys.soi) == "cum_currmap"] <- "Biophysical Connectivity Current Map"
names(bhs.soi)[names(bhs.soi) == "Height"] <- "Bear Habitat Suitability (BHS)"
names(hm.dens.soi)[names(hm.dens.soi) == "gpw_v4_population_density_adju~ountry_totals_rev11_2020_1_deg"] <- "Human Population Density by Nearest Km"



# Save our Cropped Rasters: -----------------------------------------------
terra::writeRaster(grizzinc.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/grizz_inc_SOI_10km.tif")
terra::writeRaster(biophys.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/biophys_SOI_10km.tif")
terra::writeRaster(bhs.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/bhs_SOI_10km.tif")
terra::writeRaster(d2pa.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2pa_SOI_10km.tif")
terra::writeRaster(d2met.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2metro_raster.tif" )
terra::writeRaster(d2grizzpop.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2grizz_pop_raster.tif" )
terra::writeRaster(hm.dens.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/human_dens_SOI_10km.tif" )

terra::writeRaster(d2pa.inv.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2pa_Sinvert_raster.tif")
terra::writeRaster(d2met.inv.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2metro_invert_raster.tif" )
terra::writeRaster(d2grizzpop.inv.soi, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2grizz_pop_invert_raster.tif" )

