# prep general prob of conflict data --------------------------------------

# Load packages:
library(tidyverse)
library(rstanarm)
library(terra)
library(sf)
library(tigris)

# Bring in data used for model fit ------------------------------------
pres.abs.filter <- readRDS("Data/processed/fullconf_presabs_filter.rds") %>% 
  select(.,-pop.dens)

#scale to match original analysis
pres.abs.scl <- pres.abs.filter %>% 
  mutate_at(c("dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps",  "logpopdens"), scale)


# Load Model object -------------------------------------------------------
full.conflict.mod <- readRDS('Data/processed/post_pa_full_quad.rds')
fixed.effects <- fixef(full.conflict.mod)
var.int <- ranef(full.conflict.mod)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")


# Load Rasters ------------------------------------------------------------
dist.2.pa <- rast("Data/processed/dist2pa_km_ONA.tif") 
dist.2.met <- rast("Data/processed/dist2met_km_ONA.tif")
pop.dens <- log(rast("Data/processed/hudens_ONA.tif")+1,10)
animal.dens <- rast("Data/processed/animal_crop_dens.tif")
rowcrop.dens <- rast("Data/processed/crop_crop_dens.tif")


# Create global intercept raster ------------------------------------------
global.int <- dist.2.met
global.int[!is.na(global.int)] <- fixed.effects[[1]] 


# Create Varying Intercept Raster -----------------------------------------
#Only have varying intercepts for CCS encountered in the SOI region so setting any unmatched CCS or county to 0 (i.e., reverts to the global mean/intercept)

can.ccs <- st_make_valid(st_read("Data/original/lccs000b16a_e.shp")) %>% 
  filter(., PRNAME == "British Columbia / Colombie-Britannique" | PRNAME == "Alberta") %>% 
  select(., c(CCSNAME, geometry))

us.counties <- counties(state= c("WA", "ID")) %>% 
  select(., c(NAME, geometry)) %>% 
  rename(CCSNAME = NAME) %>% 
  st_transform(., st_crs(can.ccs))

all.cs <- rbind(can.ccs, us.counties)

all.cs.join <- all.cs %>% 
  left_join(., var.int) %>% st_transform(., crs = crs(dist.2.met))

all.cs.join$`(Intercept)`[is.na(all.cs.join$`(Intercept)`)] <- 0

all.cs.vect <- crop(vect(all.cs.join), dist.2.met)
ccs.int <- rasterize(all.cs.vect, dist.2.met, field='(Intercept)') 


# Scale preds based on original data --------------------------------------

dist.2.pa.scl <- (dist.2.pa - attributes(pres.abs.scl$dist.2.pa.ps)[[2]])/attributes(pres.abs.scl$dist.2.pa.ps)[[3]]
dist.2.met.scl <- (dist.2.met - attributes(pres.abs.scl$dist.2.met.ps)[[2]])/attributes(pres.abs.scl$dist.2.met.ps)[[3]]
pop.dens.scl <- (pop.dens - attributes(pres.abs.scl$logpopdens)[[2]])/attributes(pres.abs.scl$logpopdens)[[3]]
animal.dens.scl <- (animal.dens - attributes(pres.abs.scl$animal.farm.dens.ps)[[2]])/attributes(pres.abs.scl$animal.farm.dens.ps)[[3]]
row.crop.dens.scl <- (rowcrop.dens - attributes(pres.abs.scl$ground.crop.dens.ps)[[2]])/attributes(pres.abs.scl$ground.crop.dens.ps)[[3]]

# use regression coefficients ---------------------------------------------

dist.2.pa.pred <- dist.2.pa.scl * fixed.effects[['dist.2.pa.ps']]
dist.2.met.pred <- dist.2.met.scl * fixed.effects[['dist.2.met.ps']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['logpopdens']]
pop.dens.pred2 <- (pop.dens.scl)^2 * fixed.effects[['I(logpopdens^2)']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['animal.farm.dens.ps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['ground.crop.dens.ps']]

pred.stack <- c(global.int, ccs.int, dist.2.pa.pred, dist.2.met.pred, pop.dens.pred, pop.dens.pred2, animal.dens.pred, rowcrop.dens.pred)
linpred.rast <- sum(pred.stack, na.rm=TRUE)
prob.rast <- (exp(linpred.rast))/(1 + exp(linpred.rast))
writeRaster(prob.rast, "Data/processed/prob_conflict_all_ONA.tif", overwrite=TRUE)
