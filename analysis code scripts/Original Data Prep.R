# Predictor Data Prep -----------------------------------------------------
  ## Here we load in the original data for our WARP conflict reports, BC Ecoprovinces, CAN census regions, CAN census Data, and protected areas.
  # Author note -- (this is combining Cropping WARP to Ecoprov, WARP Dom Farm Type, WARP Total Farm Count, and WARP Dist top PAs (the very beginning))

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(fasterize)
library(terra)
library(stars)
library(units)
library(googledrive)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Download Original Data --------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1EOzq2fjN9FI0Pj8YR9eiVE_TnH0LaRCF" # all original data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))



  # WARP All Species 1 Year:
warp.all.sp <-read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/WARP 3.24.20 to 3.31.21 full .csv")
  # BC Ecoprovinces:
bc.ecoprovs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Ecoprovinces/ERC_ECOPRO_polygon.shp")
  # CAN Agriculture Data
farm.type <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/farm type_32100403/farm type_32100403.csv")
  # CAN Consolidated Census Subdivisions (CCS):
can.ccs.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN census cons subdivisions (CCS)/lccs000b16a_e.shp")
  # Global Human Density:
world.hum.dens <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/Human Pop Density/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_1_deg.tif")
  # SOI Boundary and Raster for template:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")
  # BC Provincial & National Parks:
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/original/CAN Protected Areas/Parks_Combined2.shp")
  # Grizzly Population Units:
grizz.units <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/original/Grizz Pop Units/GBPU_BC_polygon.shp")
  # Grizz Inc:
grizz.inc.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/original/Grizz Increase/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 

################# We begin by filtering to our SOI ecoprovince, buffering, and cropping our conflict data to the buffered region:


# Prepping the WARP Data: -------------------------------
  # Merge the two encounter columns into one total encounter column:
warp.all.sp$total_encounter<-warp.all.sp$encounter_adults + warp.all.sp$encounter_young
head(warp.all.sp)

  # Convert selected species to 1's and all others to 0's:
warp.all.sp<- warp.all.sp %>% 
  mutate(warp.all.sp, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
head(warp.all.sp) # Check this to make sure it looks good

  # Making Conflict Data a Spatial Dataframe 

bc.sp<-structure(warp.all.sp,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.sp)
xy<-bc.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

  # Ensure this is a sf data frame:
warp.all.sp <- as(bears.spdf, "sf")
str(warp.all.sp)

# Filter down to Southern Interior Ecoprovince: ---------------------------
  # Here we select for just the southern interior province
south.interior.ep <- bc.ecoprovs %>% filter(bc.ecoprovs$CPRVNCNM == "SOUTHERN INTERIOR")

  # Write this for later use:
#st_write(south.interior.ep, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Ecoprovinces/south.interior.shp")


# Check Projections: ------------------------------------------------------
st_crs(warp.all.sp) == st_crs(south.interior.ep) #FALSE

warp.all.sp <- st_transform(warp.all.sp, st_crs(south.interior.ep))

st_crs(warp.all.sp) == st_crs(south.interior.ep) #TRUE

  # Plot these together to see overlap:
plot(st_geometry(south.interior.ep))
plot(st_geometry(warp.all.sp), add= TRUE)

# Crop WARP Points to SOI Boundary -------------------------------------------- 
  # Now we crop the WARP points to those within our southern interior ecoprovince buffer

  # Buffering EcoProvince by 10km:
  # Now let's butter our ecoprovince and see how many more reports it captures with the buffers:
south.int.10k.buf <- st_buffer(south.interior.ep, 10000)

plot(st_geometry(south.int.10k.buf))
plot(st_geometry(south.interior.ep), add= TRUE) # Here we see it with a 10k buffer

  # Save this buffered SOI Boundary:
st_write(south.int.10k.buf, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km_buf.shp")

  # Make our SOI template raster:
soi.vect <- vect(south.int.10k.buf)
grizz.inc.templ <- terra::project(grizz.inc.rast, crs(soi.vect))
grizzinc.crop.t <- terra::crop(grizz.inc.templ, soi.vect)  

soi.rast.templ <- rast(soi.vect, nrows= 218, ncols=298, nlyrs=1, xmin=1149612, xmax=1585533, ymin=453864.2, ymax=772759.3)
soi.rast <- terra::rasterize(soi.vect, soi.rast.templ, field = "OBJECTID")
soi.rast <- resample( soi.rast, grizzinc.crop.t, method='bilinear')
soi.rast[soi.rast == 327] <- 0


# Export as tiff:
terra::writeRaster(soi.rast, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km.tif")

  # Reports Within a 10k Buffer: 
  # Let's check how many total and just bear reports we include with a 10k buffer:
warp.crop.10k <- st_intersection(warp.all.sp, south.int.10k.buf) # This gives 5,606 total reports

  # Let's see how many bears this has:
warp.crop.10k %>% filter(warp.crop.10k$species_name == "BLACK BEAR" | warp.crop.10k$species_name == "GRIZZLY BEAR") # 2,062 bears out of 5,606 total reports
  # This buffer includes a better sample size for bears and total reports 

# Remove Extra Columns: ---------------------------------------------------
  # Let's remove the unwanted columns from our data frame:
warp.crop.10k$CPRVNCCD <- NULL
warp.crop.10k$FTRCD <- NULL
warp.crop.10k$PRNTCDVSNC <- NULL
warp.crop.10k$FFCTVDT <- NULL
warp.crop.10k$CPRVNCNM <- NULL
warp.crop.10k$XPRDT <- NULL
warp.crop.10k$OBJECTID <- NULL
warp.crop.10k$AREA_SQM <- NULL
warp.crop.10k$FEAT_LEN <- NULL

# Save our Cropped WARP DF ------------------------------------------------
st_write(warp.crop.10k, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp_crop_10km_buf.shp")


####################### Now, we will filter the CCS regions and Agriculture Data to BC:


# Filter CCS and Ag Files to BC Only ---------------------------------------------------
  # Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(can.ccs.shp, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"
  
# Filter down to just BC:
bc.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()
  
# Save this for later:
st_write(bc.ccs, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/BC CCS.shp")


# Filter the Ag Files down to just BC districts: --------------------------
  # See here: https://www.statology.org/filter-rows-that-contain-string-dplyr/  searched: 'Return rows with partial string, filter dplyr'
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 

  # Filtering to just the BC regions with a CCS number (so we can join to the CCS spatial data):
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))

  # Check to see what specific farm types exist in BC:
unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

  # Filter for just the 2016 census results (the data had 2011 and 2016):
bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., grepl("2016", bc.farm.filter.ccs$REF_DATE)) # Now there are 344 observations


# Editing CCS Code into new column for join -------------------------------
  # Here we separate out the CCS code into new column for join with CCS .shp:
bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # Now we have a matching 6 digits
unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
str(bc.farm.2016.ccs) # Check the structure before joining

bc.farm.2016.ccs$CCSUID.crop<- str_sub(bc.farm.2016.ccs$GEO,-6,-2) # Now we have a matching 6 digits
unique(bc.farm.2016.ccs$CCSUID.crop) #This is a 5 digit code
str(bc.farm.2016.ccs) # Check the structure before joining

# Joining the CCS with the Farm Type: -------------------------------------
  # Join the BC CCS with Ag Files:
farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 

  # Double check that this is the correct structure:
farm.ccs.sf <- st_as_sf(farm.ccs.join)
head(farm.ccs.sf) # Here we have a farm type data frame with Multi-polygon geometry - check!

######################## Here, we calculate the denisty of farms in the region:


# Here we subset the farm data to SOI, and pull out the total farm counts: ---------------------------------

  # Start by cropping the data down to SOI buffer:
farm.ccs.sf <- st_transform(farm.ccs.sf, st_crs(south.int.10k.buf))
farm.ccs.soi <- st_intersection(farm.ccs.sf, south.int.10k.buf) 

  # Subset the data - separate total farms out of NAIC:
farm.soi.subset <- subset(farm.ccs.soi, North.American.Industry.Classification.System..NAICS. != "Total number of farms")
names(farm.soi.subset)[names(farm.soi.subset) == "North.American.Industry.Classification.System..NAICS."] <- "N_A_I_C"

  # Condense Farm Types to Animal & Ground Crop Production:
animal.product.farming <- dplyr::filter(farm.soi.subset, N_A_I_C == "Beef cattle ranching and farming, including feedlots [112110]" | N_A_I_C == "Cattle ranching and farming [1121]" 
                                        | N_A_I_C == "Dairy cattle and milk production [112120]" | N_A_I_C == "Hog and pig farming [1122]" | N_A_I_C == "Poultry and egg production [1123]"
                                        | N_A_I_C == "Chicken egg production [112310]" | N_A_I_C == "Broiler and other meat-type chicken production [112320]" | N_A_I_C == "Turkey production [112330]"
                                        | N_A_I_C == "Poultry hatcheries [112340]" | N_A_I_C == "Combination poultry and egg production [112391]" | N_A_I_C == "All other poultry production [112399]"
                                        | N_A_I_C == "Sheep and goat farming [1124]" | N_A_I_C == "Sheep farming [112410]" | N_A_I_C == "Goat farming [112420]" | N_A_I_C =="Other animal production [1129]"
                                        | N_A_I_C == "Apiculture [112910]" | N_A_I_C == "Horse and other equine production [112920]" | N_A_I_C == "Fur-bearing animal and rabbit production [112930]"
                                        | N_A_I_C == "Animal combination farming [112991]" | N_A_I_C == "All other miscellaneous animal production [112999]") 


ground.crop.production <- dplyr::filter(farm.soi.subset, N_A_I_C == "Fruit and tree nut farming [1113]" | N_A_I_C == "Greenhouse, nursery and floriculture production [1114]" | N_A_I_C == "Vegetable and melon farming [1112]"
                                        | N_A_I_C == "Oilseed and grain farming [1111]" | N_A_I_C == "Soybean farming [111110]" | N_A_I_C == "Oilseed (except soybean) farming [111120]"
                                        | N_A_I_C == "Dry pea and bean farming [111130]" | N_A_I_C == "Wheat farming [111140]" | N_A_I_C == "Corn farming [111150]" | N_A_I_C == "Other grain farming [111190]"
                                        | N_A_I_C == "Potato farming [111211]" | N_A_I_C == "Other vegetable (except potato) and melon farming [111219]" | N_A_I_C == "Mushroom production [111411]" 
                                        | N_A_I_C == "Other food crops grown under cover [111419]" | N_A_I_C == "Nursery and tree production [111421]" | N_A_I_C == "Floriculture production [111422]" 
                                        | N_A_I_C == "Other crop farming [1119]" | N_A_I_C == "Tobacco farming [111910]" | N_A_I_C == "Hay farming [111940]" | N_A_I_C == "Fruit and vegetable combination farming [111993]"
                                        | N_A_I_C == "Maple syrup and products production [111994]" | N_A_I_C == "All other miscellaneous crop farming [111999]" )

  # Total the counts of these farm categories by CCS region:
animal.prod.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= animal.product.farming, FUN=sum)
ground.crop.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= ground.crop.production, FUN=sum)

names(animal.prod.counts)[names(animal.prod.counts) == "VALUE"] <- "Total Farms in CCS"
names(ground.crop.counts)[names(ground.crop.counts) == "VALUE"] <- "Total Farms in CCS"

  # Join this back to our data as a total column:
animal.prod.join <- merge(animal.prod.counts, animal.product.farming, by.x = "CCSUID", by.y = "CCSUID") 
ground.crop.join <- merge(ground.crop.counts, ground.crop.production, by.x = "CCSUID", by.y = "CCSUID") 

animal.prod.sf <- st_as_sf(animal.prod.join)
ground.crop.sf <- st_as_sf(ground.crop.join)


# Calculate the Density of Farm Types: ------------------------------------

  # We do so by dividing the count of farms by the overall area of the farm type categories (for our 10km buffered area, but save this to the 50km dataset 
  # so that we have values on the edge of our 10km zone):

  # Calculate our areas for the two objects: 
animal.prod.sf$AREA_SQM <- st_area(animal.prod.sf)
ground.crop.sf$AREA_SQM <- st_area(ground.crop.sf)

  # Make our area units kilometers:
animal.prod.sf$AREA_SQ_KM <- set_units(animal.prod.sf$AREA_SQM, km^2)
ground.crop.sf$AREA_SQ_KM <- set_units(ground.crop.sf$AREA_SQM, km^2)

  # Now we make a new col with our farms per sq km:
animal.prod.sf$Farms_per_sq_km <- animal.prod.sf$`Total Farms in CCS` / animal.prod.sf$AREA_SQ_KM
head(animal.prod.sf)

ground.crop.sf$Farms_per_sq_km <- ground.crop.sf$`Total Farms in CCS` / ground.crop.sf$AREA_SQ_KM
head(ground.crop.sf)

  # Make this col numeric:
animal.prod.sf$Farms_per_sq_km <- as.numeric(as.character(animal.prod.sf$Farms_per_sq_km))
ground.crop.sf$Farms_per_sq_km <- as.numeric(as.character(ground.crop.sf$Farms_per_sq_km))


  # Save these as .shp's for later:
st_write(animal.prod.sf,"/Users/shannonspragg/ONA_GRIZZ/Data/processed/Animal Product Farming.shp")

st_write(ground.crop.sf, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/Ground Crop Production.shp") 


################################# Prep Grizzly Population Units:

# Check Projections: ------------------------------------------------------
st_crs(grizz.units) == st_crs(south.int.10k.buf) #TRUE

  # Plot these together to see overlap:
plot(st_geometry(grizz.units))


# Filter these to just the extant populations: ----------------------------

extent.grizz <- filter(grizz.units, POP_NAME == "South Chilcotin Ranges" | POP_NAME == "Squamish-Lillooet" | POP_NAME == "Columbia-Shuswap"
                       | POP_NAME == "Central Monashee" | POP_NAME == "Valhalla" | POP_NAME == "Kettle-Granby" | POP_NAME == "Central Selkirk"
                       | POP_NAME == "Wells Gray" | POP_NAME == "South Selkirk")

  # Plot with our boundary to see overlap/position
plot(st_geometry(extent.grizz))
plot(st_geometry(south.int.10k.buf), add=TRUE)

  # Save this for later:
st_write(extent.grizz, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/Extent Grizzly Pop Units.shp") 



################################# Prep Human Density Predictor:

# Reproject the Data: --------------------------------------------------

world.dens.reproj <- terra::project(world.hum.dens, crs(soi.rast))
plot(world.dens.reproj)

crs(world.dens.reproj) == crs(soi.rast) #TRUE

soi.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(soi.rast))


# Crop and match the Human Density Data to SOI: -------------------------------------
  # Crop to SOI region:
hum.dens.crop <- terra::crop(world.dens.reproj, soi.rast)
  # Resample to match template raster:
hm.dens.rsmple <- resample(hum.dens.crop, soi.rast, method='bilinear')


# Save Raster as .tif for later: ----------------------------------------------------
terra::writeRaster(hm.dens.rsmple, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/human_dens.tif")

