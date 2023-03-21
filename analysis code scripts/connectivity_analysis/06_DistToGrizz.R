# Distance to Extant grizzly populations: ---------------------------------

# Load packages:
library(sf)
library(tidyverse)
library(terra)

#Prepare distance from recovery units
bc.grizz.units <- st_read("Data/original/GBPU_BC_polygon.shp") %>% 
  filter(., POP_NAME == "South Chilcotin Ranges" | 
           POP_NAME == "Squamish-Lillooet" |
           POP_NAME == "Columbia-Shuswap" |
           POP_NAME == "Central Monashee" |
           POP_NAME == "Valhalla" |
           POP_NAME == "Kettle-Granby" | 
           POP_NAME == "Central Selkirk" |
           POP_NAME == "Wells Gray" | 
           POP_NAME == "South Selkirk")
#BC pops based on Mowat 2020 GBPU Data for BC, filtering to current populations that border/overlap the SOI

#Bring in the US Recovery zones and filter to those in ID,WA, MT with bears based on https://www.fws.gov/species/brown-bear-ursus-arctos-horribilis
fgdb <- "Data/original/GrizzlyDistribRecoveryZones.gdb/"
ncde <- read_sf(dsn = fgdb, layer = "GrizzlyRecoveryZoneNCDE")
cye <- read_sf(dsn=fgdb, layer = "GrizzlyRecoveryZoneCabinetYaak")
se <- read_sf(dsn=fgdb, layer = "GrizzlyRecoveryZoneSelkirk")
us.zones <- rbind(ncde, cye, se) %>% select(., c(GRIZUNIT, SHAPE)) %>% 
  rename(NAME = GRIZUNIT, geometry = SHAPE) %>% 
  st_transform(., st_crs(bc.grizz.units))

bc.zones <- bc.grizz.units %>% 
  select(., c(POP_NAME, geometry)) %>% 
  rename(NAME=POP_NAME)

all.zones <- rbind(bc.zones, us.zones)


# Load template raster ----------------------------------------------------

dist2met <- rast("Data/processed/dist2met_km_ONA.tif")

all.zone.vect <- project(vect(all.zones), dist2met)
dist2grizz <- terra::distance(dist2met, all.zone.vect)
dist2grizzkm <- measurements::conv_unit(dist2grizz, "m", "km")
writeRaster(dist2grizzkm, "Data/processed/dist2grizz_km_ONA.tif", overwrite=TRUE)
