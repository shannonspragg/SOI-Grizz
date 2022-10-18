# Creating a WARP All Species Master Data frames ----------------------------
  ## Here we add in the following predictor variables: distance to protected area, distance to metro area, dominant farm type,
  #  total farm count, and ccs region name/ID. The result of this script should be two semi-complete "master" data frames: one for
  #  the general conflict regression (with pres-abs points), and the second for the bear conflict regression (warp-only points)


# Load Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
#library(raster)
library(rgdal)
#library(fasterize)
library(terra)
#library(stars)
library(measurements)

# Bring in the Points Data -------------------------------
  #  WARP SOI 10km Buffer Data:
warp.all<-st_read("Data/processed/warp_crop_10km_buf.shp") 
#head(warp.all)
  # Bring in our pres abs data frame to get variables for our absences:
warp.pres.abs <- st_read("Data/processed/warp_pres.abs.shp")


# Bring in the Variable Data -----------------
  # Our SOI 10km Buffered Boundary:
soi.10k.boundary <- st_read("Data/processed/SOI_10km_buf.shp")
  # Filtered BC Protected Areas:
fgdb <- "Data/original/CPCAD-BDCAPC_Dec2020.gdb"
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf")
bc.PAs <- fc.sf %>% 
  filter(., LOC_E == "British Columbia") 

# Filter by IUCN status (Muise et al., 2022 https://esajournals.onlinelibrary.wiley.com/doi/10.1002/eap.2603)
bc.PAs.iucn.filtered <- bc.PAs %>% 
  filter(., IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II" | IUCN_CAT == "IV") %>% 
  st_make_valid()
bc.PAs.iucn.filtered$areaha <- st_area(bc.PAs.iucn.filtered) 
units(bc.PAs.iucn.filtered$areaha) <- units::make_units(ha)
bc.PAs.iucn.filtered$areaha <- as.numeric(bc.PAs.iucn.filtered$areaha) 
# Filter by PA's larger than 100 ha:
bc.PAs <- filter(bc.PAs.iucn.filtered, areaha > 100) 

st_write(bc.PAs, "Data/processed/bc_PAs.shp")

  # BC Metropolitan Areas:
bc.metro<-st_read("Data/original/CNCNSSMTRR_polygon.shp")
str(bc.metro) # check this

  # Animal Product & Meat Farming:
animal.prod.sf <- st_read("Data/processed/Animal Product Farming.shp")
  # Ground Crop & Produce Production:
ground.crop.sf <- st_read("Data/processed/Ground Crop Production.shp")

  # BC CCS Regions:
bc.ccs<-st_read("Data/processed/BC CCS.shp")
str(bc.ccs)

  # Extant Grizzly Populations:
extant.grizz <- st_read("Data/processed/Extant Grizzly Pop Units.shp")

  # SOI Raster for rasterizing later:
bear.hab <- rast("Data/original/grizz_dens.tif")
sa.vect.proj <- terra::project(vect(soi.10k.boundary), bear.hab)
bear.sa <- crop(bear.hab, sa.vect.proj)
bear.sa <- mask(bear.sa, sa.vect.proj)

# Reproject All Data ------------------------------------------------------
  # Now we project data to match the template raster:

bears.reproj <- st_make_valid(warp.all) %>%  # our warp data
  st_transform(crs=crs(bear.sa))
pres.abs.reproj <- st_make_valid(warp.pres.abs) %>%  # our pres-abs data
  st_transform(crs=crs(bear.sa))
bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=crs(bear.sa))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=crs(bear.sa))
grizz.pop.reproj <- st_make_valid(extant.grizz) %>% 
  st_transform(crs=crs(bear.sa))

animal.farms.reproj <- st_make_valid(animal.prod.sf) %>% 
  st_transform(crs=crs(bear.sa))
ground.crop.reproj <- st_make_valid(ground.crop.sf) %>% 
  st_transform(crs=crs(bear.sa))
soi.bound.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(bear.sa))

# Check to see if they match:
st_crs(bears.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(pres.abs.reproj) == st_crs(bears.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(animal.farms.reproj) # [TRUE]

############################ Adding the Distance Variables to the Data:

# Prep Variable 1: Dist to PA's -------------------------------------------

#Calculation of the distance between the PA's and our points

  # Do this for our WARP only data:
#dist.pts2pas.warp <- st_distance(bears.reproj, bc.PAs.reproj)
dist.pts2pas.warp <- terra::distance(vect(bears.reproj), vect(bc.PAs.reproj))
head(dist.pts2pas.warp)

  # And for our pres-abs data:
dist.pts2pas.presabs <- terra::distance(vect(pres.abs.reproj), vect(bc.PAs.reproj))
head(dist.pts2pas.presabs)

  # Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.warp <- apply(dist.pts2pas.warp, 1, min)
str(min.dist.warp)

min.dist.presabs <- apply(dist.pts2pas.presabs, 1, min)
str(min.dist.presabs)

  # Add Distance Variable into Datatable:
# Convert units from meters to km:
bears.reproj$dist_to_PA<-(min.dist.warp/1000)   
head(bears.reproj)

pres.abs.reproj$dist_to_PA<-(min.dist.presabs/1000)
head(pres.abs.reproj)

# Now each df has our Dist to PA column added in, and in km units

# Prep Variable 2: Dist to Metro Areas ------------------------------------
  #Calculation of the distance between the metro areas and our points

  # Do this for our WARP only data:
dist.pts2met.warp <- terra::distance(vect(bears.reproj), vect(metro.reproj))
head(dist.pts2met.warp)

  # And the pres-abs data:
dist.pts2met.presabs <- terra::distance(vect(pres.abs.reproj), vect(metro.reproj))
head(dist.pts2met.presabs)

  # Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.met.warp <- apply(dist.pts2met.warp, 1, min)
min.dist.met.presabs <- apply(dist.pts2met.presabs, 1, min)

  # Add Distance Variable into Data table
# Convert units from meters to km:
bears.reproj$dist_to_Metro<-(min.dist.met.warp/1000)
head(bears.reproj)

pres.abs.reproj$dist_to_Metro<-(min.dist.met.presabs/1000)
head(pres.abs.reproj)
# This added the dist to metro areas column to our data

  # Check for NA's quick:
which(is.na(bears.reproj$dist_to_PA))
which(is.na(bears.reproj$dist_to_Metro))

which(is.na(pres.abs.reproj$dist_to_PA))
which(is.na(pres.abs.reproj$dist_to_Metro))

############################# Prepare Distance to Extent Bear Populations Variable: -------------------

# Prep Variable 3: Dist to Extent Grizzly Populations ------------------------------------
#Calculation of the distance between the grizz pop units and our points

  # Do this for our WARP only data:
dist.pts2grizz.warp <- terra::distance(vect(bears.reproj), vect(grizz.pop.reproj))
head(dist.pts2grizz.warp)

  # And the pres-abs data:
dist.pts2grizz.presabs <- terra::distance(vect(pres.abs.reproj), vect(grizz.pop.reproj))
head(dist.pts2grizz.presabs)

  # Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.grizz.warp <- apply(dist.pts2grizz.warp, 1, min)
min.dist.grizz.presabs <- apply(dist.pts2grizz.presabs, 1, min)

  # Add Distance Variable into Data table
# Convert units from meters to km:
bears.reproj$dist_to_GrizzPop<-(min.dist.grizz.warp/1000)
head(bears.reproj)

pres.abs.reproj$dist_to_GrizzPop<-(min.dist.grizz.presabs/1000)
head(pres.abs.reproj)
# This added the dist to grizzly populations column to our data

  # Check for NA:
which(is.na(bears.reproj$dist_to_GrizzPop)) #none
which(is.na(pres.abs.reproj$dist_to_GrizzPop)) #none

############################## Adding the Agriculture Predictors to Our Data:

# Rasterize Farm Data & WARP Points ---------------------------------------
  ## Here we make rasters for the farm type categories within our SOI region:

  # Make these spat vectors:
animal.prod.sv <- vect(animal.farms.reproj)
ground.crop.sv <- vect(ground.crop.reproj)

animal.prod.crop <- crop(animal.prod.sv, bear.sa)
ground.crop.crop <- crop(ground.crop.sv, bear.sa)
  # Rasterize our subset rasters:
animal.prod.rast <- terra::rasterize(animal.prod.crop, bear.sa, field = "Frms___")
ground.crop.rast <- terra::rasterize(ground.crop.crop, bear.sa, field = "Frms___")

  # Save these Farm Rasters:
terra::writeRaster(animal.prod.rast, "Data/processed/animal_production_density_raster.tif", overwrite=TRUE)
terra::writeRaster(ground.crop.rast, "Data/processed/ground_crop_density_raster.tif" , overwrite=TRUE)

farm.density.combined <- animal.prod.rast + ground.crop.rast

terra::writeRaster(farm.density.combined, "Data/processed/combined_farm_density.tif" , overwrite=TRUE)

# Buffer WARP Points Before Attributing Farm Values -----------------------
  # Here we buffer the WARP and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
bears.buf <- bears.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(bears.buf)) # Check the buffers

pres.abs.buf <- pres.abs.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

  # Make the buffered points spat vectors:
bears.sv.buf <- vect(bears.buf)
pres.abs.sv.buf <- vect(pres.abs.buf)

crs(bears.sv.buf) == crs(animal.prod.rast) #TRUE
crs(pres.abs.sv.buf) == crs(animal.prod.rast) #TRUE


# Prep Variable 4: the Dominant Ag Type & Total Farm Count by CCS ----------------------------
  # Here I will extract the mean values from each raster to the buffered points

  # First, for our WARP data:
bears.animal.prods.ext <- terra::extract(animal.prod.rast, bears.sv.buf, mean, na.rm=TRUE) 
# This gives us the mean value of each buffered area --> what we want!
bears.ground.crop.ext <- terra::extract(ground.crop.rast, bears.sv.buf, mean, na.rm = TRUE) 

  # Next, for our pres abs data:
pres.abs.animal.prods.ext <- terra::extract(animal.prod.rast, pres.abs.sv.buf, mean, na.rm = TRUE) 
# This gives us the mean value of each buffered area --> what we want!
pres.abs.ground.crop.ext <- terra::extract(ground.crop.rast, pres.abs.sv.buf, mean, na.rm = TRUE) 

  # Create New Column(s) for Extracted Values:

bears.reproj$Animal_Farming <- bears.animal.prods.ext[,2]
bears.reproj$Ground_Crops <- bears.ground.crop.ext[,2]

pres.abs.reproj$Animal_Farming <- pres.abs.animal.prods.ext[,2]
pres.abs.reproj$Ground_Crops <- pres.abs.ground.crop.ext[,2]

  # Check for NA's quick:
which(is.na(bears.reproj$Animal_Farming))
which(is.na(bears.reproj$Ground_Crops))

which(is.na(pres.abs.reproj$Animal_Farming)) # We have about 150 NA's
which(is.na(pres.abs.reproj$Ground_Crops)) # Same NA's as above

#   # Plot some of our NA's to see if they're outside the boundary:
plot(animal.prod.rast)
plot(st_geometry(pres.abs.reproj[12615,]), col = "red", add=TRUE)
plot(st_geometry(pres.abs.reproj[11107,]), col = "red", add=TRUE)
plot(st_geometry(pres.abs.reproj[8453,]), col = "red", add=TRUE) # All of these are outside our southern border

  # Drop these NA records:
pres.abs.dropped <- pres.abs.reproj %>% drop_na(Animal_Farming) %>% drop_na(Ground_Crops)

which(is.na(pres.abs.dropped$Animal_Farming)) # Now they're gone
which(is.na(pres.abs.dropped$Ground_Crops)) # Same

  # Update data frame:
pres.abs.reproj <- pres.abs.dropped

############################ Next, Add in the CCS Region Names to the Data:
# Project the CCS Regions to match our data: ------------------------------
bc.ccs.reproj <- st_transform(bc.ccs, st_crs(soi.bound.reproj))

  # Check to see if the projections match:
st_crs(bc.ccs.reproj) == st_crs(soi.bound.reproj) # [TRUE] 

st_crs(bears.reproj) == st_crs(bc.ccs.reproj) # [TRUE] 
st_crs(pres.abs.reproj) == st_crs(bc.ccs.reproj) # [TRUE] 

  # Plot these together to make sure:
plot(st_geometry(bc.ccs.reproj))
plot(st_geometry(soi.bound.reproj), add=TRUE)

# Crop CCS Down to SOI 10km Extent: --------------------------------------------
soi.ccs.crop <- st_intersection(bc.ccs.reproj, soi.bound.reproj)

plot(st_geometry(soi.ccs.crop))
plot(st_geometry(bears.reproj), add=TRUE)

  # Write this as a .shp for later:
st_write(soi.ccs.crop, "Data/processed/SOI_CCS_10km.shp", append=FALSE)

# Assign the WARP Points to a CCS Region: ---------------------------------
  ## Here we want to overlay the points with the regions, adding a column in the warp data that is CCS region ID, 
  #  make sure this is a factor, to fit this as a varying intercept

  # Assign our points to a CCS category:
warp.ccs.join <- st_join(bears.reproj, left = TRUE, bc.ccs.reproj) # join points

pres.abs.ccs.join <- st_join(pres.abs.reproj, left = TRUE, bc.ccs.reproj) # join points

head(warp.ccs.join) # Assigned points to a CCS category
head(pres.abs.ccs.join) # nice

warp.ccs.join <- warp.ccs.join %>% 
  dplyr::select(., -c(22:26))
  # Delete the columns we don't want:
pres.abs.ccs.join <- pres.abs.ccs.join %>% 
  dplyr::select(., -c(23:27))


head(warp.ccs.join)
head(pres.abs.ccs.join)

# Check for NA's: -------------------------------------------------------

which(is.na(warp.ccs.join$CCSNAME)) # no NAs
which(is.na(warp.ccs.join$CCSUID)) # same

which(is.na(pres.abs.ccs.join$CCSNAME)) # some
which(is.na(pres.abs.ccs.join$CCSUID)) # same as above

  # Plot to see if these are outside the soi boundary too:
plot(st_geometry(soi.ccs.crop))
plot(st_geometry(pres.abs.ccs.join[5659,]), col = "red", add=TRUE)
plot(st_geometry(pres.abs.reproj[10851,]), col = "red", add=TRUE)
plot(st_geometry(pres.abs.reproj[13771,]), col = "red", add=TRUE) # All of these are just outside our southern border

  # Drop these NA records:
pres.abs.dropped <- pres.abs.ccs.join %>% drop_na(CCSNAME) %>% drop_na(CCSUID)

which(is.na(pres.abs.dropped$CCSNAME)) # Now they're gone
which(is.na(pres.abs.dropped$CCSUID)) # Same

# Update data frame:
pres.abs.ccs.join <- pres.abs.dropped

# WARP All Species Master Data Frame --------------------------------------
  # Save the resulting data frames here:
st_write(warp.ccs.join, "Data/processed/warp.master.shp", append = FALSE)
st_write(pres.abs.ccs.join, "Data/processed/pres.abs.master.shp", append=FALSE)