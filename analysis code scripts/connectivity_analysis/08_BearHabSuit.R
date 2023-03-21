# Prep Grizz Habitat data -------------------------------------------------

# Load packages:
library(terra)
library(sf)

# Bring in data:
bhs <- rast("Data/original/grizz_dens.tif")
dist2met <- rast("Data/processed/dist2met_km_ONA.tif")

bhs.ONA <- project(bhs, dist2met)
bhs.trim <- trim(bhs.ONA)

# load US recovery unit ---------------------------------------
#The NC RZ has few (if any) bears currently, but is known to have lots of available habitat. 
#In order to develop a potential habitat layer that covers the entirety of the landscape, we assign the upper 80th percentile 
# to the NC area and the mean value to the rest of the region that occurs beyond the existing model

nc.rz <- st_read("Data/original/ncegrizrecovg1.shp") %>% 
  st_transform(., crs(bhs.ONA))

nc.rz.crp <-crop(vect(nc.rz), bhs.trim) #get ncrz that is modeled so we can remove that
nc.rz.crp.sf <- st_as_sf(nc.rz.crp)
nc.rz.nonver <-st_difference(nc.rz, nc.rz.crp.sf) #get ncrz that isn't modeled to assign new values
nc.non.mod <- crop(vect(nc.rz.nonver), bhs.ONA) 
nc.rstr <- rasterize(nc.non.mod, bhs.ONA, field=global(bhs.ONA, fun=quantile, probs= 0.8,na.rm=TRUE)[[1]]) #set ncrz to 80%
nc.rstr[is.na(nc.rstr)] <- global(bhs.ONA, fun=quantile, probs= 0.5,na.rm=TRUE)[[1]] #set rest of us to mean
nc.rstr.crop <- mask(nc.rstr, bhs.ONA, inverse=TRUE) #get rid of modeled region so we don't overwrite it
bhs.mos <- mosaic(bhs.ONA, nc.rstr.crop, fun="median") #combine the modeled and US version

writeRaster(bhs.mos, "Data/processed/bhs_ONA.tif", overwrite=TRUE)
