# Prep the dist to metro dataset ------------------------------------------

# Load Packages:
library(terra)
library(sf)
library(tidycensus)
library(dplyr)
library(tidyverse)
#load Canadian metro data:
bc.metro <- st_read("Data/original/CNCNSSMTRR_polygon.shp") %>% 
  dplyr::select(., c(CNSS_MT_ID, CNSS_MTR_M, geometry))
colnames(bc.metro)[1:2] <- c("GEOID", "NAME")
ab.metro <- st_read("Data/original/Alberta_CMACA2016.shp") %>% 
  dplyr::select(., c(CMAPUID, CMANAME, geometry)) %>% 
  st_transform(., st_crs(bc.metro))
colnames(ab.metro)[1:2] <- c("GEOID", "NAME")
can.metro <- rbind(bc.metro, ab.metro)
#Download WA metro area using tidycensus and some hacks
wa_metromicro <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", product = "components", geometry = TRUE) %>% 
  filter(str_detect(NAME,"WA|ID"))

metro <- wa_metromicro %>% 
  filter(str_detect(NAME, "Metro")) %>% 
  dplyr::select(., c(GEOID, NAME, geometry)) %>% 
  distinct(.) %>% 
  st_transform(., crs=st_crs(can.metro))

combined.metro <- rbind(can.metro, metro)


# create template raster --------------------------------------------------

ona.bound <- st_read("Data/original/ONA_TerritoryBound.shp") %>% 
  st_buffer(., 25000) %>% 
  as(., "SpatVector")
temp.rast <- rast(res=c(1000,1000), ext=ext(ona.bound), crs = crs(ona.bound))
values(temp.rast) <- rep(1, ncell(temp.rast))

combined.metro.proj <- combined.metro %>% 
  st_transform(., crs=crs(temp.rast)) %>%
  as(., "SpatVector")

dist2met <- terra::distance(temp.rast, combined.metro.proj)
dist2metkm <- measurements::conv_unit(dist2met, "m", "km")
writeRaster(dist2metkm, "Data/processed/dist2met_km_ONA.tif", overwrite=TRUE)
