library(rstanarm)
library(tidyverse)
library(sf)
options(mc.cores = parallel::detectCores())
library(loo)
library(bayesplot)
library(tidybayes)
library(ggeffects)
library(viridis)
library(patchwork)
theme_set(bayesplot::theme_default(base_family = "sans"))
warp.pres.abs <- st_read(here::here("./Data/processed/pres_abs_final.shp")) %>% st_drop_geometry()

#filter some of the absences
pres.abs.filter <- warp.pres.abs %>%
  filter(anyCnfl == 0) %>%
  slice_sample(n = 9000) %>%
  bind_rows(warp.pres.abs %>% filter(anyCnfl != 0)) %>% 
  select(., c(anyCnfl, dst__PA, dst_t_M, Anml_Fr, Grnd_Cr, CCSNAME, Human_Dens)) 

colnames(pres.abs.filter) <- c("conflict_presence_ps", "dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps", "CCSNAME.ps", "pop.dens")

#scale for analysis
pres.abs.scl <- pres.abs.filter %>% 
  mutate_at(c("dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps",  "pop.dens"), scale)

t_prior <- student_t(df = 7, location = 0, scale = 1.5)
SEED<-14124869
post.pa.full <- stan_glmer(conflict_presence_ps ~ dist.2.pa.ps + dist.2.met.ps + animal.farm.dens.ps + ground.crop.dens.ps + pop.dens + (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED) # we add seed for reproducability
post.pa.full.quad <- update(post.pa.full, formula = conflict_presence_ps ~ dist.2.pa.ps + dist.2.met.ps + animal.farm.dens.ps + ground.crop.dens.ps + pop.dens + I(pop.dens^2) + (1 | CCSNAME.ps), QR = TRUE)

post.int.only <-  update(post.pa.full, formula = conflict_presence_ps ~ 1+ (1 | CCSNAME.ps), QR = FALSE)

loo1 <- loo(post.pa.full, save_psis = TRUE)
loo2 <- loo(post.pa.full.quad, save_psis = TRUE)
loo0 <- loo(post.int.only, save_psis = TRUE)

preds <- posterior_epred(post.pa.full)
preds2 <- posterior_epred(post.pa.full.quad)
preds0 <- posterior_epred(post.int.only)
pred <- colMeans(preds)
pred2 <- colMeans(preds2)
pred0 <- colMeans(preds0)
pr <- as.integer(pred >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)
round(mean(xor(pr,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.89
round(mean(xor(pr2,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.88
round(mean(xor(pr0,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.82

ploo=E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post.pa.full))$value
ploo2 <- E_loo(preds2, loo2$psis_object, type="mean", log_ratios = -log_lik(post.pa.full.quad))$value

ploo0 <- E_loo(preds0, loo0$psis_object, type="mean", log_ratios = -log_lik(post.int.only))$value
# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.89
round(mean(xor(ploo2>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.89
round(mean(xor(ploo0>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.82

# Building plots of results -----------------------------------------------


##AUC
opar <- par()
par(pty = "s")
pROC::roc(pres.abs.scl$conflict_presence_ps, post.pa.full$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(pres.abs.scl$conflict_presence_ps, post.int.only$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=60)

legend("bottomright", legend=c("Full Model", "Varying Intercept Model"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)
par(opar)

#using full model without quadratic term as their predictive accuracy is similar and the predictor estimates seem more stable

posterior <- as.matrix(post.pa.full)
p <- mcmc_intervals(posterior,
           pars = c("dist.2.pa.ps",
                    "dist.2.met.ps",
                    "animal.farm.dens.ps",
                    "ground.crop.dens.ps",
                    "pop.dens"),
           prob = 0.8) +
  scale_y_discrete(labels = c("dist.2.pa.ps" = "Dist. to PA",
                              "dist.2.met.ps" = "Dist. to metro",
                              "animal.farm.dens.ps" = "Dens. of livestock ops.",
                              "ground.crop.dens.ps" = "Dens. of row-crop ops.",
                              "pop.dens" = "Population dens.")) 



simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
            dist.2.met.ps = mean(dist.2.met.ps),
            animal.farm.dens.ps = seq_range(animal.farm.dens.ps, n=300),
            ground.crop.dens.ps = mean(ground.crop.dens.ps),
            pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_fitted_draws(post.pa.full, 
                                 newdata=simdata,
                                 ndraws=1000,
                                 re_formula=NA)

postdraws$animal.farm.dens <- (postdraws$animal.farm.dens.ps * attributes(pres.abs.scl$animal.farm.dens.ps)[[3]])+attributes(pres.abs.scl$animal.farm.dens.ps)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(animal.farm.dens, pop.dens) %>% 
  summarise(., mean = mean(.value),
            lo = quantile(.value, 0.2),
            hi = quantile(.value, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
animal.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = animal.farm.dens, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=animal.farm.dens, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="F","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="F", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
 # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = seq_range(ground.crop.dens.ps,n=300),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_fitted_draws(post.pa.full, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$ground.crop.dens <- (postdraws$ground.crop.dens.ps * attributes(pres.abs.scl$ground.crop.dens.ps)[[3]])+attributes(pres.abs.scl$ground.crop.dens.ps)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(ground.crop.dens, pop.dens) %>% 
  summarise(., mean = mean(.value),
            lo = quantile(.value, 0.2),
            hi = quantile(.value, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
ground.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = ground.crop.dens, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ground.crop.dens, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="F","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="F", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Row-Crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = seq_range(dist.2.met.ps, n=300),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_fitted_draws(post.pa.full, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$dist.2.met <- (postdraws$dist.2.met.ps * attributes(pres.abs.scl$dist.2.met.ps)[[3]])+attributes(pres.abs.scl$dist.2.met.ps)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(dist.2.met, pop.dens) %>% 
  summarise(., mean = mean(.value),
            lo = quantile(.value, 0.2),
            hi = quantile(.value, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
dist.2met.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.met, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.met, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="F","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="F", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Distance to Metro Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = seq_range(dist.2.pa.ps, n=300),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_fitted_draws(post.pa.full, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$dist.2.pa <- (postdraws$dist.2.pa.ps * attributes(pres.abs.scl$dist.2.pa.ps)[[3]])+attributes(pres.abs.scl$dist.2.pa.ps)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(dist.2.pa, pop.dens) %>% 
  summarise(., mean = mean(.value),
            lo = quantile(.value, 0.2),
            hi = quantile(.value, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
dist.2pa.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.pa, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.pa, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="F","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="F", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Distance to Protected Area (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

p.all <- animal.dens.plot + ground.dens.plot + dist.2met.plot + dist.2pa.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')



# Generating raster predictions -------------------------------------------
library(terra)
fixed.effects <- fixef(post.pa.full)
var.int <- ranef(post.pa.full)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/SOI_CCS_10km.shp")
ccs.sf.join <- ccs.sf %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Powell River A",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

#load predictor rasters
dist.2.pa <- rast("Data/processed/dist2pa_SOI_10km.tif") 
dist.2.met <- rast("Data/processed/dist2metro_SOI_10km.tif")
pop.dens <- rast("Data/processed/human_dens_SOI_10km.tif")
animal.dens <- rast("Data/processed/animal_production_density_cropped.tif")
rowcrop.dens <- rast("Data/processed/ground_crop_density_cropped.tif")

#Create global intercept raster
global.int <- dist.2.met
global.int[!is.na(global.int)] <- fixed.effects[[1]] 

#create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.met, field='(Intercept)')

#scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(pres.abs.scl$dist.2.pa.ps)[[2]])/attributes(pres.abs.scl$dist.2.pa.ps)[[3]]
dist.2.met.scl <- (dist.2.met - attributes(pres.abs.scl$dist.2.met.ps)[[2]])/attributes(pres.abs.scl$dist.2.met.ps)[[3]]
pop.dens.scl <- (pop.dens - attributes(pres.abs.scl$pop.dens)[[2]])/attributes(pres.abs.scl$pop.dens)[[3]]
animal.dens.scl <- (animal.dens - attributes(pres.abs.scl$animal.farm.dens.ps)[[2]])/attributes(pres.abs.scl$animal.farm.dens.ps)[[3]]
row.crop.dens.scl <- (rowcrop.dens - attributes(pres.abs.scl$ground.crop.dens.ps)[[2]])/attributes(pres.abs.scl$ground.crop.dens.ps)[[3]]

dist.2.pa.pred <- dist.2.pa.scl * fixed.effects[['dist.2.pa.ps']]
dist.2.met.pred <- dist.2.met.scl * fixed.effects[['dist.2.met.ps']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['pop.dens']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['animal.farm.dens.ps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['ground.crop.dens.ps']]

pred.stack <- c(global.int, ccs.int, dist.2.pa.pred, dist.2.met.pred, pop.dens.pred, animal.dens.pred, rowcrop.dens.pred)
linpred.rast <- sum(pred.stack)
prob.rast <- (exp(linpred.rast))/(1 + exp(linpred.rast))

writeRaster(prob.rast, "Data/processed/prob_conflict_all_mw.tif")
saveRDS(post.int.only, "Data/processed/int_only_reg.rds")
saveRDS(post.pa.full, "Data/processed/full_mod_reg.rds")
saveRDS(post.pa.full.quad, "Data/processed/full_mod_quad.rds")
