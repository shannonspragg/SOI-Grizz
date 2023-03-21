# generate livesotck ops and row-crop ops rasters -------------------------

# Load packages:
library(tidyverse)
library(terra)
library(sf)
library(tigris)

# Ag Data:
farm.type <- read.csv("Data/original/farm type_32100403.csv") #from Ag census
animal.farm.wa <- read.csv("Data/original/Animal Farming WA.csv") # From US NASS
crop.farm.wa <- read.csv("Data/original/Crop Farming WA.csv") # From US NASS
animal.farm.id <- read.csv("Data/original/idaho_animal_ag_census.csv") %>% 
  filter(., Data.Item == "ANIMAL TOTALS, INCL PRODUCTS - OPERATIONS WITH SALES")
crop.farm.id <- read.csv("Data/original/idaho_crop_ag_census.csv") %>% 
  filter(., Data.Item == "CROP TOTALS - OPERATIONS WITH SALES")

animal.farm.us <- rbind(animal.farm.id, animal.farm.wa)
crop.farm.us <- rbind(crop.farm.id, crop.farm.wa )
# Process BC Ag data ------------------------------------------------------

farm.type.can <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO) | grepl("Alberta", farm.type$GEO) ) 

# Filtering to just the BC regions with a CCS number (so we can join to the CCS spatial data):
can.farm.filter.ccs<-farm.type.can %>%
  filter(., grepl("*CCS59*", farm.type.can$GEO) | grepl("*CCS48*", farm.type.can$GEO)) %>%
  filter(., REF_DATE == "2016") 
can.farm.filter.ccs$geoid <- str_sub(can.farm.filter.ccs$DGUID, -7, -1)

# Subset the data - separate total farms out of NAIC:
farm.subset <- subset(can.farm.filter.ccs, North.American.Industry.Classification.System..NAICS. != "Total number of farms")
names(farm.subset)[names(farm.subset) == "North.American.Industry.Classification.System..NAICS."] <- "N_A_I_C"

# Condense Farm Types to Animal & Ground Crop Production:
animal.product.farming <- dplyr::filter(farm.subset,  N_A_I_C == "Cattle ranching and farming [1121]" | N_A_I_C == "Hog and pig farming [1122]" | N_A_I_C == "Poultry and egg production [1123]"| N_A_I_C == "Sheep and goat farming [1124]" | N_A_I_C =="Other animal production [1129]") 


ground.crop.production <- dplyr::filter(farm.subset, N_A_I_C == "Fruit and tree nut farming [1113]" | N_A_I_C == "Greenhouse, nursery and floriculture production [1114]" | N_A_I_C == "Vegetable and melon farming [1112]"
                                        | N_A_I_C == "Oilseed and grain farming [1111]" | N_A_I_C == "Other crop farming [1119]")

#Tally by CCSUID (geoid)
animal.prod.tally <- animal.product.farming %>% 
  group_by(geoid) %>% 
  summarise(., TotalFarm = sum(VALUE))
ground.crop.tally <- ground.crop.production %>% 
  group_by(geoid) %>% 
  summarise(., TotalFarm = sum(VALUE))


# join to spatial data and calc density -----------------------------------
us.counties <- counties(state= c("WA", "ID"))

#get wa geoids right
animal.farm.us$County.ANSI <- str_pad(animal.farm.us$County.ANSI, width=3, side="left", pad = "0")
animal.farm.us$geoid <- paste0(animal.farm.us$State.ANSI, animal.farm.us$County.ANSI)
crop.farm.us$County.ANSI <- str_pad(crop.farm.us$County.ANSI, width=3, side="left", pad = "0")
crop.farm.us$geoid <- paste0(crop.farm.us$State.ANSI, crop.farm.us$County.ANSI)

#join to county sf, calculate area, and farm density
animal.farm.us.sf <-us.counties[,c("GEOID", "NAME", "geometry")] %>% 
  left_join(.,animal.farm.us , by = c("GEOID" = "geoid"))%>% 
  mutate(., ValCorrect = str_replace_all(Value, "[[:punct:]]", ""), 
         AREA_SQ_KM = units::set_units(st_area(.), km^2),
         FARMS_SQ_KM = as.numeric(ValCorrect)/AREA_SQ_KM)

crop.farm.us.sf <- us.counties[,c("GEOID", "NAME", "geometry")] %>% 
  left_join(.,crop.farm.us , by = c("GEOID" = "geoid"))%>% 
  mutate(., ValCorrect = str_replace_all(Value, "[[:punct:]]", ""), 
         AREA_SQ_KM = units::set_units(st_area(.), km^2),
         FARMS_SQ_KM = as.numeric(ValCorrect)/AREA_SQ_KM)


# Join BC Data ------------------------------------------------------------

can.ccs <- st_make_valid(st_read("Data/original/lccs000b16a_e.shp")) %>% 
  filter(., PRNAME == "British Columbia / Colombie-Britannique" | PRNAME == "Alberta")

animal.farm.can.sf <- can.ccs[, c("CCSUID", "CCSNAME", "geometry")] %>% 
  left_join(., animal.prod.tally, by=c("CCSUID" = "geoid")) %>% 
  mutate(., AREA_SQ_KM = units::set_units(st_area(.), km^2),
         FARMS_SQ_KM = TotalFarm/AREA_SQ_KM)

crop.farm.can.sf <- can.ccs[, c("CCSUID", "CCSNAME", "geometry")] %>% 
  left_join(., ground.crop.tally, by=c("CCSUID" = "geoid")) %>% 
  mutate(., AREA_SQ_KM = units::set_units(st_area(.), km^2),
         FARMS_SQ_KM = TotalFarm/AREA_SQ_KM)


# Combine into single sf for each type and rasterize ----------------------

crop.farm.us.join <- crop.farm.us.sf %>% 
  select(.,c(GEOID, NAME, Value, AREA_SQ_KM, FARMS_SQ_KM, geometry)) %>% 
  rename(CCSUID = GEOID, CCSNAME=NAME, TotalFarm = Value)
crop.farm.us.join$FARMS_SQ_KM <- as.numeric(crop.farm.us.join$FARMS_SQ_KM)

animal.farm.us.join <- animal.farm.us.sf %>% 
  select(.,c(GEOID, NAME, Value, AREA_SQ_KM, FARMS_SQ_KM, geometry)) %>% 
  rename(CCSUID = GEOID, CCSNAME=NAME, TotalFarm = Value)
animal.farm.us.join$FARMS_SQ_KM <- as.numeric(animal.farm.us.join$FARMS_SQ_KM)

animal.farm.can.sf$FARMS_SQ_KM <- as.numeric(animal.farm.can.sf$FARMS_SQ_KM)
crop.farm.can.sf$FARMS_SQ_KM <- as.numeric(crop.farm.can.sf$FARMS_SQ_KM)

animal.farm.can.join <- animal.farm.can.sf[,c("CCSUID", "CCSNAME", "TotalFarm", "AREA_SQ_KM", "FARMS_SQ_KM","geometry")] %>% st_transform(., st_crs(animal.farm.us.join))
crop.farm.can.join <- crop.farm.can.sf[,c("CCSUID", "CCSNAME", "TotalFarm", "AREA_SQ_KM", "FARMS_SQ_KM","geometry")] %>% st_transform(., st_crs(crop.farm.us.join))

animal.farm.all <- rbind(animal.farm.can.join, animal.farm.us.join)
crop.farm.all <- rbind(crop.farm.can.join, crop.farm.us.join)

#load template raster
dist2metkm <- rast("Data/processed/dist2met_km_ONA.tif")

#crop sfs
animal.farm.vect <- terra::project(vect(animal.farm.all), crs(dist2metkm))
crop.farm.vect <- terra::project(vect(crop.farm.all), crs(dist2metkm))

animal.crp <- crop(animal.farm.vect, dist2metkm)
crop.crp <- crop(crop.farm.vect, dist2metkm)

animal.crp.rst <- rasterize(animal.crp, dist2metkm, field="FARMS_SQ_KM", fun=mean, na.rm=TRUE)
writeRaster(animal.crp.rst, "Data/processed/animal_crop_dens.tif", overwrite=TRUE)

crop.crp.rst <- rasterize(crop.crp, dist2metkm, field="FARMS_SQ_KM", fun=mean, na.rm=TRUE)
writeRaster(crop.crp.rst, "Data/processed/crop_crop_dens.tif", overwrite=TRUE)
