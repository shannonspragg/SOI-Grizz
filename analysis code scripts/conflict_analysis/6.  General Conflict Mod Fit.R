
# General Conflict Model --------------------------------------------------
  ## Here we run our general conflict full, quad, and null models. We test the data simulation and plot results, then produce the prob general conflict
  ## raster for the bear analysis.


# Load Packages: ----------------------------------------------------------
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
library(modelr)
bayesplot_theme_set(bayesplot::theme_default(base_family = "sans"))

set.seed(14124869)
# Bring in Data: ----------------------------------------------------------
warp.pres.abs <- st_read(here::here("./Data/processed/pres_abs_final.shp")) %>% st_drop_geometry()

#filter some of the absences
pres.abs.filter <- warp.pres.abs %>%
  filter(anyCnfl == 0) %>%
  slice_sample(n = 9000) %>%
  bind_rows(warp.pres.abs %>% filter(anyCnfl != 0)) %>% 
  dplyr::select(., c(anyCnfl, dst__PA, dst_t_M, Anml_Fr, Grnd_Cr, CCSNAME, Human_Dens)) 

colnames(pres.abs.filter) <- c("conflict_presence_ps", "dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps", "CCSNAME.ps", "pop.dens")

pres.abs.filter$logpopdens <- log((pres.abs.filter$pop.dens+1),10) 
# Scale Data for Analysis: ------------------------------------------------
  ## Here we scale the predictors for analysis

pres.abs.scl <- pres.abs.filter %>% 
  mutate_at(c("dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps",  "pop.dens", "logpopdens"), scale)

# Run General Conflict Models: --------------------------------------------
t_prior <- student_t(df = 4, location = 0, scale = 0.35)

  # Full Model:
post.pa.full <- stan_glmer(conflict_presence_ps ~ dist.2.pa.ps + dist.2.met.ps + animal.farm.dens.ps + ground.crop.dens.ps + logpopdens + (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = normal(0,2), QR=TRUE,
                           iter = 3000, chains=5,
                           seed = 14124869) # we add seed for reproducibility


  # Full Model + Quadratic for Pop Dens:
post.pa.full.quad <- update(post.pa.full, formula = conflict_presence_ps ~ dist.2.pa.ps + dist.2.met.ps + animal.farm.dens.ps + ground.crop.dens.ps + logpopdens + I(logpopdens^2) + (1 | CCSNAME.ps), QR = TRUE)

  # Intercept-only model:
post.int.only <-  update(post.pa.full, formula = conflict_presence_ps ~ 1+ (1 | CCSNAME.ps), QR = FALSE, iter=5000)

saveRDS(pres.abs.filter, "Data/processed/fullconf_presabs_filter.rds")
saveRDS(post.pa.full, "Data/processed/post_pa_full.rds")
saveRDS(post.pa.full.quad, "Data/processed/post_pa_full_quad.rds")
saveRDS(post.int.only, "Data/processed/post_int_only.rds")


# Run LOOIC and Posterior Comparisons: ------------------------------------
loo1 <- loo(post.pa.full, save_psis = TRUE)
loo2 <- loo(post.pa.full.quad, save_psis = TRUE)
loo0 <- loo(post.int.only, save_psis = TRUE)

post_pa_loo_comp <- loo_compare(loo1, loo2, loo0) #quad outperforms (0, -34, 3284.4)

preds <- posterior_epred(post.pa.full)
preds2 <- posterior_epred(post.pa.full.quad)
preds0 <- posterior_epred(post.int.only)
pred <- colMeans(preds)
pred2 <- colMeans(preds2)
pred0 <- colMeans(preds0)
pr <- as.integer(pred >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)
round(mean(xor(pr,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.93
round(mean(xor(pr2,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.93
round(mean(xor(pr0,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) # 0.83

ploo=E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post.pa.full))$value
ploo2 <- E_loo(preds2, loo2$psis_object, type="mean", log_ratios = -log_lik(post.pa.full.quad))$value

ploo0 <- E_loo(preds0, loo0$psis_object, type="mean", log_ratios = -log_lik(post.int.only))$value

saveRDS(loo1, "Data/processed/post_pa_full_loo.rds")
saveRDS(loo2, "Data/processed/post_pa_full_quad_loo.rds")
saveRDS(loo0, "Data/processed/post_int_only_loo.rds")
saveRDS(post_pa_loo_comp, "Data/processed/post_pa_loo_comp.rds")

  # LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.93 
round(mean(xor(ploo2>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.93,
round(mean(xor(ploo0>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.82

# Building plots of results -----------------------------------------------

  # Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(pres.abs.scl$conflict_presence_ps, post.pa.full.quad$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(pres.abs.scl$conflict_presence_ps, post.int.only$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=60)

legend("bottomright", legend=c("Full Model with Quadratic Term",  "Varying Intercept Model"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)
par(opar)


  # Plot Effects of Posterior Coefficients:
library(bayestestR)
library(see)
library(insight)
library(ggplot2)

post.pa.result <- p_direction(post.pa.full.quad)
post.pa.full.preds.plot <- plot(post.pa.result) +
  scale_y_discrete(labels = c("dist.2.pa.ps" = "Dist. to PA",
                              "dist.2.met.ps" = "Dist. to metro",
                              "animal.farm.dens.ps" = "Dens. of livestock ops.",
                              "ground.crop.dens.ps" = "Dens. of row-crop ops.",
                              "logpopdens" = "log(Population dens.)",
                              "I(logpopdens^2)" = "log(Population dens.)^2")) +
  ggtitle("Predictor Effects for General Wildlife Conflict")

  # this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect


ggsave("plots/allconf_pd_plot.png", post.pa.full.preds.plot)


# Simulate Data & Posterior Predictive Draws: -----------------------------

posterior <- as.matrix(post.pa.full.quad)
p <- mcmc_intervals(posterior,
           pars = c("dist.2.pa.ps",
                    "dist.2.met.ps",
                    "animal.farm.dens.ps",
                    "ground.crop.dens.ps",
                    "logpopdens",
                    "I(logpopdens^2)"),
           prob = 0.89) +
  scale_y_discrete(labels = c("dist.2.pa.ps" = "Dist. to PA",
                              "dist.2.met.ps" = "Dist. to metro",
                              "animal.farm.dens.ps" = "Dens. of livestock ops.",
                              "ground.crop.dens.ps" = "Dens. of row-crop ops.",
                              "logpopdens" = "log(Population dens.)",
                              "I(logpopdens^2)" = "log(Population dens.)^2")) 

ggsave("plots/allconf_CI_plot.png", p)

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
            dist.2.met.ps = mean(dist.2.met.ps),
            animal.farm.dens.ps = seq_range(animal.farm.dens.ps, n=300),
            ground.crop.dens.ps = mean(ground.crop.dens.ps),
            logpopdens = quantile(pres.abs.scl$logpopdens, probs = c(0.1, 0.5, 0.9)))

# postdraws <- tidybayes::add_fitted_draws(post.pa.full, 
#                                  newdata=simdata,
#                                  ndraws=1000,
#                                  re_formula=NA)

postdraws <- tidybayes::add_epred_draws(post.pa.full.quad, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$animal.farm.dens <- (postdraws$animal.farm.dens.ps * attributes(pres.abs.scl$animal.farm.dens.ps)[[3]])+attributes(pres.abs.scl$animal.farm.dens.ps)[[2]]

  # Plotting Livestock Density: # NOTE: changing .value to .epred to match update from add_epred_draws
plot.df <- postdraws %>% 
  mutate_at(., vars(logpopdens), as.factor) %>% 
  group_by(animal.farm.dens, logpopdens) %>% 
  dplyr::summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$logpopdens) <-  c("Lower 10%", "Mean", "Upper 10%")
animal.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = animal.farm.dens, y = mean, colour =logpopdens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=animal.farm.dens, fill = logpopdens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Log(Population Density)")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Log(Population Density)") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
 # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("plots/allconf_animaldens_me.png", animal.dens.plot)
###### Testing other color palette:
# library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)
# 
# animal.dens.plot <- ggplot(data=plot.df) +
#   geom_line(aes(x = animal.farm.dens, y = mean, colour =pop.dens), lwd=1.5) +
#   geom_ribbon(aes(ymin=lo, ymax=hi, x=animal.farm.dens, fill = pop.dens), alpha = 0.2) +
#   discrete_scale(scale_name, palette="Dark2", "Population Density")+
#   discrete_scale(scale_name, palette="Dark2", "Population Density") +
#   ylab("Probability of Conflict") + 
#   xlab(expression("Density of Livestock Operations per"~km^{2}))+
#   # guides(fill=guide_legend(title="Population Density"))+
#   theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))


simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = seq_range(ground.crop.dens.ps,n=300),
                    logpopdens = quantile(pres.abs.scl$logpopdens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(post.pa.full.quad, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$ground.crop.dens <- (postdraws$ground.crop.dens.ps * attributes(pres.abs.scl$ground.crop.dens.ps)[[3]])+attributes(pres.abs.scl$ground.crop.dens.ps)[[2]]

  # Plotting Row Crop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(logpopdens), as.factor) %>% 
  group_by(ground.crop.dens, logpopdens) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$logpopdens) <-  c("Lower 10%", "Mean", "Upper 10%")
ground.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = ground.crop.dens, y = mean, colour =logpopdens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ground.crop.dens, fill = logpopdens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Log(Population Density)")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Log(Population Density)") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Row-Crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("plots/allconf_rowcrop_me.png", ground.dens.plot)

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = seq_range(dist.2.met.ps, n=300),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    logpopdens = quantile(pres.abs.scl$logpopdens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(post.pa.full.quad, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$dist.2.met <- (postdraws$dist.2.met.ps * attributes(pres.abs.scl$dist.2.met.ps)[[3]])+attributes(pres.abs.scl$dist.2.met.ps)[[2]]

  # Plot Dist 2 Metro Areas:
plot.df <- postdraws %>% 
  mutate_at(., vars(logpopdens), as.factor) %>% 
  group_by(dist.2.met, logpopdens) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$logpopdens) <-  c("Lower 10%", "Mean", "Upper 10%")
dist.2met.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.met, y = mean, colour =logpopdens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.met, fill = logpopdens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Log(Population Density)")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Log(Population Density)") +
  ylab("Probability of Conflict") + 
  xlab("Distance to Metro Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("plots/allconf_dist2met_me.png", dist.2met.plot)

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = seq_range(dist.2.pa.ps, n=300),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    logpopdens = quantile(pres.abs.scl$logpopdens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(post.pa.full.quad, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$dist.2.pa <- (postdraws$dist.2.pa.ps * attributes(pres.abs.scl$dist.2.pa.ps)[[3]])+attributes(pres.abs.scl$dist.2.pa.ps)[[2]]

  # Plotting Dist 2 PA's:
plot.df <- postdraws %>% 
  mutate_at(., vars(logpopdens), as.factor) %>% 
  group_by(dist.2.pa, logpopdens) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$logpopdens) <-  c("Lower 10%", "Mean", "Upper 10%")
dist.2pa.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.pa, y = mean, colour =logpopdens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.pa, fill = logpopdens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Log(Population Density)")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Log(Population Density)") +
  ylab("Probability of Conflict") + 
  xlab("Distance to Protected Area (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("plots/allconf_dist2pa_me.png", dist.2pa.plot)
p.all <- animal.dens.plot + ground.dens.plot + dist.2met.plot + dist.2pa.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

ggsave(here::here("plots/allconf_allpreds_me.png"), p.all)

# Generating raster predictions -------------------------------------------
library(terra)
fixed.effects <- fixef(post.pa.full.quad)
var.int <- ranef(post.pa.full.quad)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/SOI_CCS_10km.shp")
ccs.sf.join <- ccs.sf %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Powell River A",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

  # Load predictor rasters:
dist.2.pa <- rast("Data/processed/dist2pa_SOI_10km.tif") 
dist.2.met <- rast("Data/processed/dist2metro_SOI_10km.tif")
logpopdens <- log(rast("Data/processed/human_dens_SOI_10km.tif")+1, 10) # need to match extent
animal.dens <- rast("Data/processed/animal_production_density_cropped.tif")
rowcrop.dens <- rast("Data/processed/ground_crop_density_cropped.tif")

pop.d.crop <- crop(logpopdens, animal.dens)
logpopdens <- mask(pop.d.crop, animal.dens)
writeRaster(logpopdens, "Data/processed/pop_dens_SOI_10km.tif", overwrite=TRUE)

  # Create global intercept raster
global.int <- dist.2.met
global.int[!is.na(global.int)] <- fixed.effects[[1]] 

  # Create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.met, field='(Intercept)')

  # Scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(pres.abs.scl$dist.2.pa.ps)[[2]])/attributes(pres.abs.scl$dist.2.pa.ps)[[3]]
dist.2.met.scl <- (dist.2.met - attributes(pres.abs.scl$dist.2.met.ps)[[2]])/attributes(pres.abs.scl$dist.2.met.ps)[[3]]
pop.dens.scl <- (logpopdens - attributes(pres.abs.scl$logpopdens)[[2]])/attributes(pres.abs.scl$logpopdens)[[3]]
animal.dens.scl <- (animal.dens - attributes(pres.abs.scl$animal.farm.dens.ps)[[2]])/attributes(pres.abs.scl$animal.farm.dens.ps)[[3]]
row.crop.dens.scl <- (rowcrop.dens - attributes(pres.abs.scl$ground.crop.dens.ps)[[2]])/attributes(pres.abs.scl$ground.crop.dens.ps)[[3]]

dist.2.pa.pred <- dist.2.pa.scl * fixed.effects[['dist.2.pa.ps']]
dist.2.met.pred <- dist.2.met.scl * fixed.effects[['dist.2.met.ps']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['logpopdens']]
pop.dens.pred2 <- (pop.dens.scl)^2 * fixed.effects[['I(logpopdens^2)']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['animal.farm.dens.ps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['ground.crop.dens.ps']]

  # Combine our Rasters:
pred.stack <- c(global.int, ccs.int, dist.2.pa.pred, dist.2.met.pred, pop.dens.pred, pop.dens.pred2, animal.dens.pred, rowcrop.dens.pred)
linpred.rast <- sum(pred.stack)
prob.rast <- (exp(linpred.rast))/(1 + exp(linpred.rast))

  # Save these:
writeRaster(prob.rast, "Data/processed/prob_conflict_all.tif", overwrite=TRUE)
# saveRDS(post.int.only, "Data/processed/int_only_reg.rds")
# saveRDS(post.pa.full, "Data/processed/full_mod_reg.rds")
# saveRDS(post.pa.full.quad, "Data/processed/full_mod_quad.rds")
