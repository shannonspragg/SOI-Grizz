# #Estimate distance to protected areas -----------------------------------

# Load packages:
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)

# Load BC PAs -------------------------------------------------------------
fgdb <- "Data/original/CPCAD-BDCAPC_Dec2020.gdb"
fc <- st_read(dsn=fgdb,layer="CPCAD_Dec2020")

can.PAs <- fc %>% 
  filter(., LOC_E == "British Columbia" | LOC_E == "Alberta") 

# Filter by IUCN status (Muise et al., 2022 https://esajournals.onlinelibrary.wiley.com/doi/10.1002/eap.2603)
can.PAs.iucn.filtered <- can.PAs %>% 
  filter(., IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II" | IUCN_CAT == "IV") %>% 
  st_make_valid()
can.PAs.iucn.filtered$areaha <- st_area(can.PAs.iucn.filtered) 
units(can.PAs.iucn.filtered$areaha) <- units::make_units(ha)
can.PAs.iucn.filtered$areaha <- as.numeric(can.PAs.iucn.filtered$areaha) 
# Filter by PA's larger than 100 ha:
can.PAs.fin <- filter(can.PAs.iucn.filtered, areaha > 100) 


# Load US PAs -------------------------------------------------------------
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}
#fgdb <- "Data/original/PAD_US2_1.gdb/"
fgdb <- "Data/original/PAD_US3_0.gdb/"
wa.id.pas <- read_sf(dsn = fgdb, layer = "PADUS3_0Combined_Proclamation_Marine_Fee_Designation_Easement") %>% 
  filter(., State_Nm %in% c("WA", "ID")) %>% 
  filter(., IUCN_Cat == "Ia" | IUCN_Cat == "Ib" | IUCN_Cat == "II" | IUCN_Cat == "IV" )

# wa.id.pas <- read_sf(dsn = fgdb, layer = "PADUS2_1Combined_Proclamation_Marine_Fee_Designation_Easement") %>% 
#   filter(., State_Nm %in% c("WA", "ID")) %>% 
#   filter(., IUCN_Cat == "Ia" | IUCN_Cat == "Ib" | IUCN_Cat == "II" | IUCN_Cat == "IV" )
public.land.corrected <-  ensure_multipolygons(wa.id.pas)

wa.id.pas.valid <- st_make_valid(public.land.corrected)

wa.id.pas.valid$areaha <- st_area(wa.id.pas.valid)
units(wa.id.pas.valid$areaha) <- units::make_units(ha)
wa.id.pas.valid$areaha <- as.numeric(wa.id.pas.valid$areaha) 
wa.id.pas.fin <- filter(wa.id.pas.valid, areaha > 100) 


wa.id.pas.join <- wa.id.pas.fin %>% 
  dplyr::select(., c(Unit_Nm, State_Nm, IUCN_Cat, geom)) %>% 
  st_transform(., st_crs(can.PAs.fin)) %>% 
  rename(geometry = geom)

can.pas.join <- can.PAs.fin %>% 
  dplyr::select(., c(NAME_E, LOC_E, IUCN_CAT, Shape)) %>% 
  rename(Unit_Nm = NAME_E, State_Nm = LOC_E, IUCN_Cat = IUCN_CAT, geometry = Shape)

pas.all <- rbind(wa.id.pas.join, can.pas.join)

# load template raster and estimate distance ------------------------------

dist2metkm <- rast("Data/processed/dist2met_km_ONA.tif") 
pas.vect <- terra::project(vect(pas.all), dist2metkm)
dist2pa <- terra::distance(dist2metkm, pas.vect)
dist2pakm <- measurements::conv_unit(dist2pa, "m", "km")
writeRaster(dist2pakm, "Data/processed/dist2pa_km_ONA.tif", overwrite=TRUE)

