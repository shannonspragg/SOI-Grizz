# Prepping Biophys Resistance Rasters: ------------------------------------
## This is where we prep the biophysical raster input for our omniscape model

# Load Packages: ----------------------------------------------------------
library(here)
library(terra)
library(sf)
library(rgdal)
library(geodata)
#library(raster)
#library("sp")
library(ggmap)
library(maptools)
library(viridis)



# Bring in Data: ----------------------------------------------------------
#Creating the raster file objects pulled down from the data folder
soi_bdry <- st_read(here("Data/processed/SOI_10km_buf.shp")) 
bc_bdry <- st_read("Data/processed/BC CCS.shp")
griz_dens <- rast(here("Data/original/grizz_dens.tif"))
hmi <- rast("Data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif")




# Reproject the SOI shapefile boundary
griz_proj <- terra::project(griz_dens, hmi)
soi_proj.sp <- soi_bdry %>% st_transform(., crs(griz_proj)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
soi_proj.vec <- vect(soi_proj.sp)

# Prep Other Rasters: -----------------------------------------------------

# Expand grizz_dens extent:
griz.ext <- terra::extend(griz_proj, soi_proj.vec, filename=here("data/processed/griz_ext.tif"), overwrite=TRUE)
griz.ext[is.nan(griz.ext)] <- 0

# Project & Crop HMI:
hmi.crop <- crop(hmi, soi_proj.vec)

grizz.crop <- crop(griz.ext, soi_proj.vec)
hmi.proj <- project(hmi.crop, grizz.crop)

# Rescale HMI:
hmi.rescale <- hmi.proj / 65536
# Obtain the elevation values for CAN and US, merge them together
lat.long.comb <- expand.grid(data.frame(lon = seq(-125, -110, by=5),
                            lat = seq(40, 55, by=5)))
  
elev <- lapply(1:nrow(lat.long.comb), function(x) elevation_3s(lon=lat.long.comb[x,1], lat.long.comb[x,2], path=tempdir()))
elev.mos <- do.call(mosaic, elev)
crs(elev.mos) <- "+proj=longlat +datum=WGS84"


# Project & Crop Elev:
soi.proj.elev <- terra::project(soi_proj.vec, elev.mos)
elev.crop <- crop(elev.mos, soi.proj.elev)
elev.proj <- terra::project(elev.crop, grizz.crop)

rough <- terrain(elev.proj, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.rescale[rough.rescale==0] <- 0.000000001
rough.rescale[is.nan(rough.rescale)] <- 1


# Fuzzysum Our Rasters: ---------------------------------------------------

# Fuzzy sum approach to combine them from Theobald 2013:
fuzzysum2 <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
# Add together our biophys attributes: gHM and roughness
biophys_fuzsum <- fuzzysum2(hmi.rescale, rough.rescale)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "BHS+gHM Resistance Layer")

# Make into resistance surface
biophys_resistance <- (1+biophys_fuzsum)^10
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "Biophysical Resistance Layer")

# Project these back to BC Albers:
grizz.reproj <- terra::project(grizz.crop, griz_dens)
biophys.resist.reproj <- terra::project(biophys_resistance, griz_dens)
soi.vect.reproj <- terra::project(soi_proj.vec, griz_dens)

# Crop to our extent:
biophys.resist.crop <- crop(biophys.resist.reproj, soi.vect.reproj)
grizz.crop <- crop(grizz.reproj, soi.vect.reproj)


# Save Biophys for Circuitscape Run: -----------------------------------------
writeRaster(hmi.rescale, filename=here("data/processed/hmi_rescale.tif"), overwrite=TRUE)
writeRaster(rough.rescale, filename=here("data/processed/roughness_rescale.tif"), overwrite=TRUE)

writeRaster(biophys_fuzsum, filename=here("data/processed/biophys_comnined.tif"), overwrite=TRUE)

  # Omniscape Inputs:
writeRaster(grizz.crop, filename=here("data/processed/grizz_dens_crop.tif"), overwrite=TRUE) # source input for omniscape
writeRaster(biophys.resist.crop, filename=here("data/processed/biophys_resist_soi.tif"), overwrite=TRUE) # resist input for omniscape


