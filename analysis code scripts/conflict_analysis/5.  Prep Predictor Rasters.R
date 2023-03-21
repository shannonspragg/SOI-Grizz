# Prep Predictor Rasters: --------------------------------------------
  # Here we bring and produce in our predictor rasters and make sure they are all cropped to SOI and equally projected:

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)

# Load Data: --------------------------------------------------------------

  # Bear Density - Bear Habitat Suitability (BHS)
bhs.rast <- rast("Data/processed/bhs_SOI_10km.tif")

  # SOI Region for plotting:
soi.10k.boundary <- st_read("Data/processed/SOI_10km_buf.shp")

bc.PAs <- st_read("Data/processed/bc_PAs.shp")
bc.metro<-st_read("Data/original/CNCNSSMTRR_polygon.shp")
  
  # Extant Grizzly Populations:
extant.grizz <- st_read("Data/processed/Extant Grizzly Pop Units.shp")
 
  # Farm Rasters:
animal.prod.rast <- terra::rast("Data/processed/animal_production_density_raster.tif")
ground.crop.rast <- terra::rast("Data/processed/ground_crop_density_raster.tif" )

  
################################# First, we need to produce our Distance to PA, Metro, and Grizzly Pop Rasters:

# Check Projections: ------------------------------------------------------

bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=st_crs(soi.10k.boundary))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=st_crs(soi.10k.boundary))
grizz.pop.reproj <- st_make_valid(extant.grizz) %>% 
  st_transform(crs=st_crs(soi.10k.boundary))

  # Check to see if they match:
st_crs(soi.10k.boundary) == st_crs(bc.PAs.reproj) # [TRUE] 
st_crs(metro.reproj) == st_crs(soi.10k.boundary) # [TRUE]
st_crs(grizz.pop.reproj) == st_crs(soi.10k.boundary) # [TRUE]

# Rasterize our Points & Polygons: ----------------------------------------

  # Make our data spatvectors:
PAs.sv <- vect(bc.PAs.reproj) 
metro.sv <- vect(metro.reproj)
grizz.pop.sv <- vect(grizz.pop.reproj)

# Crop PAs & Metro --------------------------------------------------------
soi.15k.buf <- soi.10k.boundary %>% 
  st_buffer(., 5000)

soi.15km.sv <- vect(soi.15k.buf)

PAs.soi.sv <- terra::crop(PAs.sv, soi.15km.sv)
metro.soi.sv <- terra::crop(metro.sv, soi.15km.sv)

# Create a Continuous Raster for Cell Distance to PA's: -------------------

  # Do this for our variables:
dist.pa.raster <- terra::distance(bhs.rast, PAs.soi.sv) 

dist.met.raster <- terra::distance(bhs.rast, metro.soi.sv) 

dist.grizz.pop.raster <- terra::distance(bhs.rast, grizz.pop.sv) 

  
  # Make sure our rasters are in km:
dist.pa.raster <- measurements::conv_unit(dist.pa.raster,"m","km") # There we go
dist.met.raster <- measurements::conv_unit(dist.met.raster,"m","km")
dist.grizz.pop.raster <- measurements::conv_unit(dist.grizz.pop.raster,"m","km")

# Crop remaining rasters and match resolution -----------------------------
dist.pa.crop <- terra::crop(dist.pa.raster, vect(soi.10k.boundary))
dist.pa.crop <- mask(dist.pa.crop, vect(soi.10k.boundary))
dist.met.crop <- terra::crop(dist.met.raster, vect(soi.10k.boundary))
dist.met.crop <- mask(dist.met.crop, vect(soi.10k.boundary))
dist.griz.pop.crop <- terra::crop(dist.grizz.pop.raster, vect(soi.10k.boundary))
dist.griz.pop.crop <- mask(dist.griz.pop.crop, vect(soi.10k.boundary))
animal.prod.crop <- terra::crop(animal.prod.rast, vect(soi.10k.boundary))
animal.prod.crop <- mask(animal.prod.crop, vect(soi.10k.boundary))
ground.crop.crop <- terra::crop(ground.crop.rast, vect(soi.10k.boundary))
ground.crop.crop <- mask(ground.crop.crop, vect(soi.10k.boundary))
combined.crop <- animal.prod.crop + ground.crop.crop

terra::writeRaster(dist.pa.crop, "Data/processed/dist2pa_SOI_10km.tif", overwrite=TRUE)
terra::writeRaster(dist.met.crop, "Data/processed/dist2metro_SOI_10km.tif", overwrite=TRUE )
terra::writeRaster(dist.griz.pop.crop, "Data/processed/dist2grizz_pop_raster.tif", overwrite=TRUE)
terra::writeRaster(animal.prod.crop, "Data/processed/animal_production_density_cropped.tif", overwrite=TRUE)
terra::writeRaster(ground.crop.crop, "Data/processed/ground_crop_density_cropped.tif",  overwrite=TRUE )
terra::writeRaster(combined.crop, "Data/processed/combined_farm_density_cropped.tif",  overwrite=TRUE )
