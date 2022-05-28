# Predictor Data Prep -----------------------------------------------------
  ## Here we load in the original data for our WARP conflict reports, BC Ecoprovinces, CAN census regions, CAN census Data, and protected areas.
  # Author note -- (this is combining Cropping WARP to Ecoprov, WARP Dom Farm Type, WARP Total Farm Count, and WARP Dist top PAs (the very beginning))

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
#library(raster)
library(rgdal)
#library(fasterize)
library(terra)
#library(stars)
library(units)
library(googledrive)


# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

folder_url <- "https://drive.google.com/drive/u/0/folders/1EOzq2fjN9FI0Pj8YR9eiVE_TnH0LaRCF" # all original data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

folder_url <- "https://drive.google.com/drive/folders/1pvMatdKYeUdGAxME5y3zO-jfit1T8aC3" # Canada PAs gdb
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/CPCAD-BDCAPC_Dec2020.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Bring in our Original Data --------------------------------------------

  # WARP All Species 1 Year:
warp.all.sp <-read.csv("Data/original/WARP 3.24.20 to 3.31.21 full .csv")
  # BC Ecoprovinces:
bc.ecoprovs <- st_read("Data/original/ERC_ECOPRO_polygon.shp")
  # CAN Agriculture Data
farm.type <- read.csv("Data/original/farm type_32100403.csv")
  # CAN Consolidated Census Subdivisions (CCS):
can.ccs.shp<- st_make_valid(st_read("Data/original/lccs000b16a_e.shp"))
  # Global Human Density:
world.hum.dens <- terra::rast("Data/original/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
  # Grizzly Population Units:
grizz.units <- st_read("Data/original/GBPU_BC_polygon.shp")
  # Grizz Inc:
grizz.inc.rast <- rast("Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 

  # Check Validity:
any(!st_is_valid(bc.ecoprovs)) #FALSE
any(!st_is_valid(can.ccs.shp)) # FALSE
any(!st_is_valid(grizz.units)) # FALSE


################# We begin by filtering to our SOI ecoprovince, buffering, and cropping our conflict data to the buffered region:

# Prepping the WARP Data: -------------------------------
  # Merge the two encounter columns into one total encounter column:
warp.all.sp$total_encounter<-warp.all.sp$encounter_adults + warp.all.sp$encounter_young
head(warp.all.sp)

  # Convert selected species to 1's and all others to 0's:
warp.all.sp<- warp.all.sp %>% 
  mutate(warp.all.sp, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
head(warp.all.sp) # Check this to make sure it looks good
##MW: Got rid of STRUCTURE Piece and adjusted
  # Making Conflict Data a Spatial Dataframe 
xy<-warp.all.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = warp.all.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

  # Ensure this is a sf data frame:
warp.all.sp <- as(bears.spdf, "sf")
str(warp.all.sp)

# Filter down to Southern Interior Ecoprovince: ---------------------------
  # Here we select for just the southern interior province
south.interior.ep <- bc.ecoprovs %>% filter(bc.ecoprovs$CPRVNCNM == "SOUTHERN INTERIOR", drop=TRUE)


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
st_write(south.int.10k.buf, "Data/processed/SOI_10km_buf.shp", append = FALSE)

  # Make our SOI template raster:
#soi.vect <- vect(south.int.10k.buf)
#grizz.inc.templ <- terra::project(grizz.inc.rast, crs(soi.vect))
#grizzinc.crop.t <- terra::crop(grizz.inc.templ, soi.vect)  

#soi.rast.templ <- rast(soi.vect, nrows= 218, ncols=298, nlyrs=1, xmin=1149612, xmax=1585533, ymin=453864.2, ymax=772759.3)
#soi.rast <- terra::rasterize(soi.vect, soi.rast.templ, field = "OBJECTID")
#soi.rast <- resample( soi.rast, grizzinc.crop.t, method='bilinear')
#soi.rast[soi.rast == 327] <- 0


# Export as tiff:
#terra::writeRaster(soi.rast, "Data/processed/SOI_10km.tif")

  # Reports Within a 10k Buffer: 
  # Let's check how many total and just bear reports we include with a 10k buffer:
warp.crop.10k <- st_intersection(warp.all.sp, south.int.10k.buf) # This gives 5,606 total reports

  # Let's see how many bears this has:
warp.crop.10k %>% filter(warp.crop.10k$species_name == "BLACK BEAR" | warp.crop.10k$species_name == "GRIZZLY BEAR") # 2,062 bears out of 5,606 total reports
  # This buffer includes a better sample size for bears and total reports 

# Remove Extra Columns: ---------------------------------------------------
  # Let's remove the unwanted columns from our data frame:
#MW: USING SELECT TO DROP UNWANTED COLUMNS
warp.crop.10k <- warp.crop.10k %>% 
  dplyr::select(., -c(15:23))

#warp.crop.10k$CPRVNCCD <- NULL
#warp.crop.10k$FTRCD <- NULL
#warp.crop.10k$PRNTCDVSNC <- NULL
#warp.crop.10k$FFCTVDT <- NULL
#warp.crop.10k$CPRVNCNM <- NULL
#warp.crop.10k$XPRDT <- NULL
#warp.crop.10k$OBJECTID <- NULL
#warp.crop.10k$AREA_SQM <- NULL
#warp.crop.10k$FEAT_LEN <- NULL

# Save our Cropped WARP DF ------------------------------------------------
st_write(warp.crop.10k, "Data/processed/warp_crop_10km_buf.shp", append=FALSE)


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
st_write(bc.ccs, "Data/processed/BC CCS.shp", append = FALSE)



# Filter the Ag Files down to just BC districts: --------------------------
  # See here: https://www.statology.org/filter-rows-that-contain-string-dplyr/  searched: 'Return rows with partial string, filter dplyr'
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 

  # Filtering to just the BC regions with a CCS number (so we can join to the CCS spatial data):
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))

  # Check to see what specific farm types exist in BC:
unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

  # Filter for just the 2016 census results (the data had 2011 and 2016):

#MW removing the grepl so that you get exact match for 2016
bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., REF_DATE == "2016") 

# Editing CCS Code into new column for join -------------------------------
  # Here we separate out the CCS code into new column for join with CCS .shp:
#bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # Now we have a matching 6 digits
#unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
#str(bc.farm.2016.ccs) # Check the structure before joining

#bc.farm.2016.ccs$CCSUID.crop<- str_sub(bc.farm.2016.ccs$GEO,-6,-2) # Now we have a matching 6 digits
#unique(bc.farm.2016.ccs$CCSUID.crop) #This is a 5 digit code
#str(bc.farm.2016.ccs) # Check the structure before joining

# Joining the CCS with the Farm Type: -------------------------------------
  # Join the BC CCS with Ag Files:
#farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 
##MW: THE CCSUID AND DGUID FIELDS BOTH HAVE THE GEOGRAPHY DATA SO TRYING TO MINIMIZE THE NUMBER OF PLACES WHERE ERROR COULD BE INTRODUCED

bc.farm.2016.ccs$geoid <- str_sub(bc.farm.2016.ccs$DGUID, -7, -1)

farm.ccs.join <- bc.ccs %>% 
  left_join(., bc.farm.2016.ccs, by = c("CCSUID" = "geoid"))




#MW: don't do this st_as_sf because the above produces an sf object and st_as_sf has some potential consequences when you have both coordinate columns and geometry columns; also the code below doesn't return and sf object
  # Double check that this is the correct structure:
#farm.ccs.sfb <- st_as_sf(farm.ccs.join)
#head(farm.ccs.sf) # Here we have a farm type data frame with Multi-polygon geometry - check!


######################## Here, we calculate the denisty of farms in the region:

# Here we subset the farm data to SOI, and pull out the total farm counts: ---------------------------------

  # Start by cropping the data down to SOI buffer:
farm.ccs.sf <- st_transform(farm.ccs.join, st_crs(south.int.10k.buf))
#MW: This actually clips the CCS so the areas get reduced need to keep the area to get the densities correct.

#farm.ccs.soi <- st_intersection(farm.ccs.sf, south.int.10k.buf) 
#MW: This ensures we keep the entire geometry for calculating the density
farm.ccs.soi <- farm.ccs.sf[st_intersects(south.int.10k.buf, farm.ccs.sf, sparse =  FALSE),]

  # Subset the data - separate total farms out of NAIC:
farm.soi.subset <- subset(farm.ccs.soi, North.American.Industry.Classification.System..NAICS. != "Total number of farms")
names(farm.soi.subset)[names(farm.soi.subset) == "North.American.Industry.Classification.System..NAICS."] <- "N_A_I_C"

  # Condense Farm Types to Animal & Ground Crop Production:
animal.product.farming <- dplyr::filter(farm.soi.subset,  N_A_I_C == "Cattle ranching and farming [1121]" | N_A_I_C == "Hog and pig farming [1122]" | N_A_I_C == "Poultry and egg production [1123]"| N_A_I_C == "Sheep and goat farming [1124]" | N_A_I_C =="Other animal production [1129]") 


ground.crop.production <- dplyr::filter(farm.soi.subset, N_A_I_C == "Fruit and tree nut farming [1113]" | N_A_I_C == "Greenhouse, nursery and floriculture production [1114]" | N_A_I_C == "Vegetable and melon farming [1112]"
                                        | N_A_I_C == "Oilseed and grain farming [1111]" | N_A_I_C == "Other crop farming [1119]")

  # Total the counts of these farm categories by CCS region:
#animal.prod.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= animal.product.farming, FUN=sum)
#ground.crop.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= ground.crop.production, FUN=sum)
##MW: Using the group_by; summarize avoids having to rejoin data
animal.prod.sf <- animal.product.farming %>% 
  group_by(CCSUID) %>% 
  summarise(., "Total Farms in CCS" = sum(VALUE))
ground.crop.sf <- ground.crop.production %>% 
  group_by(CCSUID) %>% 
  summarise(., "Total Farms in CCS" = sum(VALUE))
#names(animal.prod.counts)[names(animal.prod.counts) == "VALUE"] <- "Total Farms in CCS"
#names(ground.crop.counts)[names(ground.crop.counts) == "VALUE"] <- "Total Farms in CCS"

  # Join this back to our data as a total column:
#animal.prod.join <- merge(animal.prod.counts, animal.product.farming, by.x = "CCSUID", by.y = "CCSUID") 
#ground.crop.join <- merge(ground.crop.counts, ground.crop.production, by.x = "CCSUID", by.y = "CCSUID") 

#animal.prod.sf <- st_as_sf(animal.prod.join)
#ground.crop.sf <- st_as_sf(ground.crop.join)


# Calculate the Density of Farm Types: ------------------------------------

  # We do so by dividing the count of farms by the overall area of the farm type categories (for our 10km buffered area, but save this to the 50km dataset 
  # so that we have values on the edge of our 10km zone):

  # Calculate our areas for the two objects: 
  # Make our area units kilometers:
animal.prod.sf$AREA_SQ_KM <- units::set_units(st_area(animal.prod.sf), km^2)
ground.crop.sf$AREA_SQ_KM <- units::set_units(st_area(ground.crop.sf), km^2)

  # Now we make a new col with our farms per sq km:
animal.prod.sf$Farms_per_sq_km <- animal.prod.sf$`Total Farms in CCS` / animal.prod.sf$AREA_SQ_KM
head(animal.prod.sf)

ground.crop.sf$Farms_per_sq_km <- ground.crop.sf$`Total Farms in CCS` / ground.crop.sf$AREA_SQ_KM
head(ground.crop.sf)

  # Make this col numeric:
animal.prod.sf$Farms_per_sq_km <- as.numeric(animal.prod.sf$Farms_per_sq_km)
ground.crop.sf$Farms_per_sq_km <- as.numeric(ground.crop.sf$Farms_per_sq_km)


  # Save these as .shp's for later:
st_write(animal.prod.sf,"Data/processed/Animal Product Farming.shp", append = FALSE)

st_write(ground.crop.sf, "Data/processed/Ground Crop Production.shp", append = FALSE) 

################################# Prep Grizzly Population Units:

# Check Projections: ------------------------------------------------------
st_crs(grizz.units) == st_crs(south.int.10k.buf) #TRUE

  # Plot these together to see overlap:
plot(st_geometry(grizz.units))


# Filter these to just the extant populations: ----------------------------

extant.grizz <- filter(grizz.units, POP_NAME == "South Chilcotin Ranges" | 
                         POP_NAME == "Squamish-Lillooet" | 
                         POP_NAME == "Columbia-Shuswap" |
                         POP_NAME == "Central Monashee" | 
                         POP_NAME == "Valhalla" | 
                         POP_NAME == "Kettle-Granby" | 
                         POP_NAME == "Central Selkirk" |
                         POP_NAME == "Wells Gray" | 
                         POP_NAME == "South Selkirk")

  # Plot with our boundary to see overlap/position
plot(st_geometry(extent.grizz))
plot(st_geometry(south.int.10k.buf), add=TRUE)

  # Save this for later:
st_write(extant.grizz, "Data/processed/Extant Grizzly Pop Units.shp") 



################################# Prep Human Density Predictor:

# Reproject the Data: --------------------------------------------------
##MW: I am not reprojecting the data here as that doesn't seem necessary yet and it's not clear what the new resolution should be
soi.buf.vect <- vect(south.int.10k.buf)
soi.buf.reproj <- project(soi.buf.vect, world.hum.dens)
world.dens.crop <- crop(world.hum.dens, soi.buf.reproj)

#world.dens.reproj <- terra::project(world.hum.dens, crs(soi.rast))
#plot(world.dens.reproj)

#crs(world.dens.reproj) == crs(soi.rast) #TRUE

#soi.reproj <- st_make_valid(south.int.10k.buf) %>%  # reproject the vector data first to match hum density, then crop using soi.vect, then reproject smaller hum dens raster to match soi rast, then resample
#  st_transform(crs=crs(soi.rast))


# Crop and match the Human Density Data to SOI: -------------------------------------
  # Crop to SOI region:
#hum.dens.crop <- terra::crop(world.dens.reproj, soi.rast)
  # Resample to match template raster:
#hm.dens.rsmple <- resample(hum.dens.crop, soi.rast, method='bilinear')


# Save Raster as .tif for later: ----------------------------------------------------
terra::writeRaster(world.dens.crop, "Data/processed/human_dens_crop.tif")

