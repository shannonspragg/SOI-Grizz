
# Comparing PA Datasets: --------------------------------------------------

# Publication Data: Load in Canada Spatial Data ---------------------------------------------
library(rgdal)
library(sf)
library(tidyverse)
library(measurements)
fgdb <- "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN Protected Areas/CPCAD-BDCAPC_Dec2020.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

  # Read the feature class for PA shapes
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf") 

  #  Filter to just BC:
bc.PAs <- fc.sf %>% 
  filter(., LOC_E == "British Columbia") %>% 
  st_make_valid()

  # Filter by IUCN status (Muise et al., 2022 https://esajournals.onlinelibrary.wiley.com/doi/10.1002/eap.2603)
bc.PAs.iucn.filtered <- bc.PAs %>% 
  filter(., IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II" | IUCN_CAT == "IV")

  # Filter by PA's larger than 100 ha:
bc.PAs.100.ha <- filter(bc.PAs.iucn.filtered, O_AREA > 100) 


# Clayton's PA Data: ------------------------------------------------------

bc.PAs.clay <- st_read("/Users/shannonspragg/SOI-Grizz/Data/original/Parks_Combined2.shp") # Clayton's data

bc.PAs.clay$AREA <- st_area(bc.PAs.clay)

bc.PAs.clay$AREA <- units::set_units(x = bc.PAs.clay$AREA, value = ha)


