# prep bear conflict surface ----------------------------------------------

# Load packages:
library(tidyverse)
library(rstanarm)
library(terra)
library(sf)
library(tigris)

# Bring in original data and model fit ------------------------------------
#this file was produced for the SOI-Grizz analysis and is brought in here to ensure that the predictor rasters are on the appropriate scale
bear.conflict <- st_read("Data/original/warp_final.shp") %>% 
  st_buffer(., 5000)

prob.conflict <- rast("Data/processed/prob_conflict_all.tif")
bear.prob.conflict <- terra::extract(prob.conflict, vect(bear.conflict), mean, na.rm=TRUE)
bear.conflict$conflictprob <- bear.prob.conflict[,2]

bear.conflict.df <- bear.conflict %>% 
  st_drop_geometry() %>% 
  dplyr::select(., bears, CCSNAME, dst__PA, dst__GP, Anml_Fr, Grnd_Cr, Biophys, GrizzInc, BHS, Human_Dens,conflictprob)

colnames(bear.conflict.df) <- c("conflict", "CCSNAME.ps", "dist2pa", "dist2grizz", "livestockOps", "rowcropOps", "connectivity", "grizzinc", "habsuit", "humandens", "conflictprob")

bear.conflict.df$logpopdens <- log(bear.conflict.df$humandens +1, 10)

# Scale Data:
bear.conflict.df.scl <- bear.conflict.df %>% 
  mutate_at(c("dist2pa", "dist2grizz", "livestockOps", "rowcropOps", "connectivity", "grizzinc", "habsuit", "humandens", "logpopdens", "conflictprob"), scale) 


# Load Model object -------------------------------------------------------
bear.conflict.mod <- readRDS('Data/processed/bear_quad_reg4.rds')
fixed.effects <- fixef(bear.conflict.mod)
var.int <- ranef(bear.conflict.mod)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

# Load Rasters ------------------------------------------------------------
dist.2.pa <- rast("Data/processed/dist2pa_km_ONA.tif") 
pop.dens <- rast("Data/processed/hudens_ONA.tif")
animal.dens <- rast("Data/processed/animal_crop_dens.tif")
rowcrop.dens <- rast("Data/processed/crop_crop_dens.tif")
dist.2.grizz <- rast("Data/processed/dist2grizz_km_ONA.tif")
bhs <- rast("Data/processed/bhs_ONA.tif")
grizinc <- rast("Data/processed/griz_inc_ONA.tif")
biophys <- rast("Data/processed/biophysONA/cum_currmap.tif")
conflict <- rast("Data/processed/prob_conflict_all_ONA.tif")

# Create global intercept raster ------------------------------------------
global.int <- dist.2.pa
global.int[!is.na(global.int)] <- fixed.effects[[1]] 

# Create Varying Intercept Raster -----------------------------------------
#Only have varying intercepts for CCS encountered in the SOI region so setting any unmatched CCS or county to 0 (i.e., reverts to the global mean/intercept)

can.ccs <- st_make_valid(st_read("Data/original/lccs000b16a_e.shp")) %>% 
  filter(., PRNAME == "British Columbia / Colombie-Britannique" | PRNAME == "Alberta") %>% 
  dplyr::select(., c(CCSNAME, geometry))

us.counties <- counties(state= c("WA", "ID")) %>% 
  dplyr::select(., c(NAME, geometry)) %>% 
  rename(CCSNAME = NAME) %>% 
  st_transform(., st_crs(can.ccs))

all.cs <- rbind(can.ccs, us.counties)

all.cs.join <- all.cs %>% 
  left_join(., var.int) %>% st_transform(., crs = crs(dist.2.pa))

all.cs.join$`(Intercept)`[is.na(all.cs.join$`(Intercept)`)] <- 0

ccs.int <- rasterize(vect(all.cs.join), dist.2.pa, field='(Intercept)') 
all.cs.vect <- crop(ccs.int, dist.2.pa)


#scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(bear.conflict.df.scl$dist2pa)[[2]])/attributes(bear.conflict.df.scl$dist2pa)[[3]]

pop.dens.scl <- (pop.dens - attributes(bear.conflict.df.scl$logpopdens)[[2]])/attributes(bear.conflict.df.scl$logpopdens)[[3]]

animal.dens.scl <- (animal.dens - attributes(bear.conflict.df.scl$livestockOps)[[2]])/attributes(bear.conflict.df.scl$livestockOps)[[3]]

row.crop.dens.scl <- (rowcrop.dens - attributes(bear.conflict.df.scl$rowcropOps)[[2]])/attributes(bear.conflict.df.scl$rowcropOps)[[3]]

grizz.dist.scl <- (dist.2.grizz - attributes(bear.conflict.df.scl$dist2grizz)[[2]])/attributes(bear.conflict.df.scl$dist2grizz)[[3]]

bhs.scl <- (bhs - attributes(bear.conflict.df.scl$habsuit)[[2]])/attributes(bear.conflict.df.scl$habsuit)[[3]]

grizzinc.scl <- (grizinc - attributes(bear.conflict.df.scl$grizzinc)[[2]])/attributes(bear.conflict.df.scl$grizzinc)[[3]]

biophys.scl <- (biophys - attributes(bear.conflict.df.scl$connectivity)[[2]])/attributes(bear.conflict.df.scl$connectivity)[[3]]

conflict.scl <- (conflict - attributes(bear.conflict.df.scl$conflictprob)[[2]])/attributes(bear.conflict.df.scl$conflictprob)[[3]]

# Generate lin pred
dist2pa.pred <- dist.2.pa.scl * fixed.effects[['dist2pa']]
grizz.dist.pred <- grizz.dist.scl * fixed.effects[['dist2grizz']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['livestockOps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['rowcropOps']]
rowcrop.dens.quad.pred <- (row.crop.dens.scl)^2 * fixed.effects[['I(rowcropOps^2)']]
biophys.pred <- biophys.scl * fixed.effects[['connectivity']]
grizzinc.pred <- grizzinc.scl * fixed.effects[['grizzinc']]
grizzinc.quad.pred <- (grizzinc.scl)^2 * fixed.effects[['I(grizzinc^2)']]
bhs.pred <- bhs.scl * fixed.effects[['habsuit']]
bhs.quad.pred <- (bhs.scl)^2 * fixed.effects[['I(habsuit^2)']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['logpopdens']]
pop.dens.quad.pred <- (pop.dens.scl)^2 * fixed.effects[['I(logpopdens^2)']]
conflict.pred <- conflict.scl * fixed.effects[['conflictprob']]
conflict.quad.prd <- (conflict.scl)^2 * fixed.effects[['I(conflictprob^2)']]

pred.stack <- c(global.int, ccs.int, dist2pa.pred, grizz.dist.pred, animal.dens.pred, rowcrop.dens.pred, rowcrop.dens.quad.pred, biophys.pred, grizzinc.pred, grizzinc.quad.pred, bhs.pred, bhs.quad.pred, pop.dens.pred, pop.dens.quad.pred,      conflict.pred, conflict.quad.prd)

linpred.rst <- sum(pred.stack)
prob.rast <- (exp(linpred.rst))/(1 + exp(linpred.rst))
writeRaster(prob.rast, "Data/processed/prob_conflict_bears_ONA.tif", overwrite=TRUE)
