
# Bear Conflict Regression Models: ----------------------------------------
  ## Here we prep the bear conflict models, plots, posterior draws, and probability of conflict raster


# Load packages: ----------------------------------------------------------
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
library(terra)
library(modelr)
theme_set(bayesplot::theme_default(base_family = "sans"))


# Bring in Data & Prep: ----------------------------------------------------------
bear.conflict <- st_read("Data/processed/warp_final.shp") %>% 
  st_buffer(., 5000)

  # Add General Conflict into Bear Data:
prob.conflict <- rast("Data/processed/prob_conflict_all.tif")
norm.current <- rast("Data/original/normalized_cum_currmap.tif")
bear.prob.conflict <- terra::extract(prob.conflict, vect(bear.conflict), mean, na.rm=TRUE)
bear.norm.current <- terra::extract(norm.current, vect(bear.conflict), mean, na.rm=TRUE)
bear.conflict$conflictprob <- bear.prob.conflict[,2]
bear.conflict$normcurrent <- bear.norm.current[,2]

bear.conflict.df <- bear.conflict %>% 
  st_drop_geometry() %>% 
  dplyr::select(., bears, CCSNAME, dst__PA, dst__GP, Anml_Fr, Grnd_Cr, Biophys, GrizzInc, BHS, Human_Dens,conflictprob, normcurrent)

colnames(bear.conflict.df) <- c("conflict", "CCSNAME.ps", "dist2pa", "dist2grizz", "livestockOps", "rowcropOps", "connectivity", "grizzinc", "habsuit", "humandens", "conflictprob", "normcurrent")

bear.conflict.df$logpopdens <- log(bear.conflict.df$humandens +1, 10)

  # Scale Data:
bear.conflict.df.scl <- bear.conflict.df %>% 
  mutate_at(c("dist2pa", "dist2grizz", "livestockOps", "rowcropOps", "connectivity", "grizzinc", "habsuit", "humandens", "logpopdens", "conflictprob", "normcurrent"), scale) 

##MW: Need to think about how to deal with the correlation between conflicprob and all of the predictors used to creat it

# Run Bear Conflict Models: -----------------------------------------------
t_prior <- student_t(df = 4, location = 0, scale = 0.35)
int_prior <- normal(location = 0, scale =2, autoscale = FALSE)

SEED<-14124869

  # Full Model:
bear.full.mod <- stan_glmer(conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps + connectivity + grizzinc + habsuit + logpopdens + conflictprob + (1 | CCSNAME.ps), 
                           data = bear.conflict.df.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED)

  # Full Model + Quadratic for GenConf:
bear.full.mod.quad <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps  + connectivity + grizzinc + habsuit + logpopdens + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

bear.full.mod.quad2 <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps + I(rowcropOps^2) + connectivity + grizzinc + habsuit + logpopdens + I(logpopdens^2) + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

bear.full.mod.quad3 <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps + I(rowcropOps^2) + connectivity + grizzinc + habsuit + I(habsuit^2) + logpopdens + I(logpopdens^2) + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

bear.full.mod.quad4 <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps + I(rowcropOps^2) + connectivity + grizzinc +I(grizzinc^2) + habsuit + I(habsuit^2) + logpopdens + I(logpopdens^2) + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)
  # Full Model - GenConf:
bear.no.conf <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps  + connectivity + grizzinc + habsuit + logpopdens + (1 | CCSNAME.ps), QR=TRUE)

  # Intercept Only Model: 
bear.int.only <- update(bear.full.mod, formula = conflict ~ (1 | CCSNAME.ps) , QR = FALSE)

saveRDS(bear.full.mod.quad, "Data/processed/bear_quad_reg.rds")
saveRDS(bear.int.only, "Data/processed/bear_int_only.rds")
saveRDS(bear.full.mod, "Data/processed/bear_full.rds")
saveRDS(bear.no.conf, "Data/processed/bear_no_conf.rds")

# Read back in for future use:
bear.full.mod.quad <- readRDS("Data/processed/bear_quad_reg.rds")
bear.int.only <- readRDS("Data/processed/bear_int_only.rds")
bear.full.mod <- readRDS("Data/processed/bear_full.rds")
bear.no.conf <- readRDS("Data/processed/bear_no_conf.rds")

# Model Comparison: -------------------------------------------------------
loo1 <- loo(bear.full.mod, save_psis = TRUE)
loo2 <- loo(bear.full.mod.quad, save_psis = TRUE)
loo3 <- loo(bear.full.mod.quad2, save_psis = TRUE)
loo4 <- loo(bear.full.mod.quad3, save_psis = TRUE)
loo5 <- loo(bear.full.mod.quad4, save_psis = TRUE)
loo6 <- loo(bear.no.conf, save_psis = TRUE)
loo0 <- loo(bear.int.only, save_psis = TRUE)

bear.loo.comp <- loo_compare(loo1, loo2, loo3, loo4,loo5,loo6,loo0)
saveRDS(bear.loo.comp, "Data/processed/bear_loo_comp.rds")

preds6 <- posterior_epred(bear.no.conf)
preds5 <- posterior_epred(bear.full.mod.quad4)
preds4 <- posterior_epred(bear.full.mod.quad3)
preds3 <- posterior_epred(bear.full.mod.quad2)
preds2 <- posterior_epred(bear.full.mod.quad)
preds1 <- posterior_epred(bear.full.mod)
preds0 <- posterior_epred(bear.int.only)

pred6 <- colMeans(preds6)
pred5 <- colMeans(preds5)
pred4 <- colMeans(preds4)
pred3 <- colMeans(preds3)
pred2 <- colMeans(preds2)
pred1 <- colMeans(preds1)
pred0 <- colMeans(preds0)

pr6 <- as.integer(pred6 >= 0.5)
pr5 <- as.integer(pred5 >= 0.5)
pr4 <- as.integer(pred4 >= 0.5)
pr3 <- as.integer(pred3 >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr1 <- as.integer(pred1 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)

round(mean(xor(pr6,as.integer(bear.conflict.df.scl$conflict==0))),3)
round(mean(xor(pr5,as.integer(bear.conflict.df.scl$conflict==0))),3)
round(mean(xor(pr4,as.integer(bear.conflict.df.scl$conflict==0))),3)
round(mean(xor(pr3,as.integer(bear.conflict.df.scl$conflict==0))),3) #.67 
round(mean(xor(pr2,as.integer(bear.conflict.df.scl$conflict==0))),3) #.67
round(mean(xor(pr1,as.integer(bear.conflict.df.scl$conflict==0))),3) #0.67
round(mean(xor(pr0,as.integer(bear.conflict.df.scl$conflict==0))),3) #0.68

ploo1 <- E_loo(preds1, loo1$psis_object, type="mean", log_ratios = -log_lik(bear.full.mod))$value

ploo2<-E_loo(preds2, loo2$psis_object, type="mean", log_ratios = -log_lik(bear.full.mod.quad))$value

ploo3<-E_loo(preds3, loo3$psis_object, type="mean", log_ratios = -log_lik(bear.no.conf))$value

ploo0<-E_loo(preds0, loo0$psis_object, type="mean", log_ratios = -log_lik(bear.int.only))$value

round(mean(xor(ploo1>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.66
round(mean(xor(ploo2>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.67
round(mean(xor(ploo3>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.66
round(mean(xor(ploo0>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.67

# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(bear.conflict.df.scl$conflict, bear.full.mod.quad4$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
#pROC::plot.roc(bear.conflict.df.scl$conflict, bear.full.mod$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=45)

pROC::plot.roc(bear.conflict.df.scl$conflict, bear.no.conf$fitted.values, percent=TRUE, col='#B090D0', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
pROC::plot.roc(bear.conflict.df.scl$conflict, bear.int.only$fitted.values, percent=TRUE, col='#FFAA00', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=35)

legend("bottomright", legend=c("Full Model",  "No Conflict Model", "Varying Intercept-only Model"),
       col=c("#377eb8","#4daf4a", "#B090D0", "#FFAA00"), lwd = 4)
par(opar)




# Plot Effects of Posterior Coefficients:
library(bayestestR)
# install.packages("see")
#install.packages("insight")
library(see)
library(insight)
library(ggplot2)

bear.quad.result <- p_direction(bear.full.mod.quad4)
bear.quad.preds.plot <- plot(bear.quad.result, title = "Predictor Effects for Bear Conflict")
bear.quad.preds.plot
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect


# Plot results ------------------------------------------------------------

posterior <- as.matrix(bear.full.mod.quad4)
parnames <- names(fixef(bear.full.mod.quad4))[2:15]
p <- mcmc_intervals(posterior,
                    pars = parnames,
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist2pa" = "Dist. to PA",
                              "dist2grizz" = "Dist. to extant grizzly bear \npopulations",
                              "livestockOps" = "Dens. of livestock ops.",
                              "rowcropOps" = "Dens. of row-crop ops.",
                              "I(rowcropOps^2)" = expression("Dens. of row-crop ops."^2),
                              "connectivity" = "Connectivity",
                              "grizzinc" = "Perceptions of \ngrizzly bears",
                              "I(grizzinc^2)" = expression("Perceptions of \ngrizzly bears"^2),
                              "habsuit" = "Grizzly bear \nhabitat suitability",
                              "I(habsuit^2)" = expression("Grizzly bear \nhabitat suitability"^2),
                              "logpopdens" = "log(Human population dens.)",
                              "I(logpopdens^2)" = expression("log(Human population dens.)"^2),
                              "conflictprob" = "Prob of wildlife conflict",
                              "I(conflictprob^2)" = expression("Prob of wildlife conflict"^2)))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = seq_range(dist2pa, n=300),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, # changing to add_elpd_draws from add_fitted_draws
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$dist2pa_un <- (postdraws$dist2pa * attributes(bear.conflict.df.scl$dist2pa)[[3]])+attributes(bear.conflict.df.scl$dist2pa)[[2]]

  # Plot Dist to PA:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2pa_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
dist2pa.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2pa_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2pa_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Distance to Protected Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/dist2pa_plot_bearmod.png", dist2pa.plot.b)

# Plot Dist to GrizzPop:
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = seq_range(dist2grizz, n=300),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$dist2grizz_un <- (postdraws$dist2grizz * attributes(bear.conflict.df.scl$dist2grizz)[[3]])+attributes(bear.conflict.df.scl$dist2grizz)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2grizz_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
dist2grizz.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2grizz_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2grizz_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Distance to Extant Grizzly Pops. (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/dist2grizz_plot_bearmod.png", dist2grizz.plot)


# Plot LIvestock dens:
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = seq_range(livestockOps, n=300),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$livestockOps_un <- (postdraws$livestockOps * attributes(bear.conflict.df.scl$livestockOps)[[3]])+attributes(bear.conflict.df.scl$livestockOps)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(livestockOps_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
livestockOps.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = livestockOps_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=livestockOps_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/livestock_plot_bearmod.png", livestockOps.plot.b)

# Plot row crop dens:
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = seq_range(rowcropOps, n=300),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$rowcropOps_un <- (postdraws$rowcropOps * attributes(bear.conflict.df.scl$rowcropOps)[[3]])+attributes(bear.conflict.df.scl$rowcropOps)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(rowcropOps_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
rowcropOps.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = rowcropOps_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=rowcropOps_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Row-crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/rowcrops_plot_bearmod.png", rowcropOps.plot.b)

# Plot biophys cum current:
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = seq_range(connectivity, n=300),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$connectivity_un <- (postdraws$connectivity * attributes(bear.conflict.df.scl$connectivity)[[3]])+attributes(bear.conflict.df.scl$connectivity)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(connectivity_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
connectivity.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = connectivity_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=connectivity_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Cumulative Current Flow (Amperes)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/connectivity_plot_bearmod.png", connectivity.plot)

# Plot Grizzinc: (should this be expanded to include a wider range)
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = seq_range(grizzinc, n=300),
                    habsuit = mean(habsuit),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$grizzinc_un <- (postdraws$grizzinc * attributes(bear.conflict.df.scl$grizzinc)[[3]]) + attributes(bear.conflict.df.scl$grizzinc)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(grizzinc_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
grizzinc.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = grizzinc_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=grizzinc_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Prop. of People Supporting Grizzly Pop. Increase")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/grizzinc_plot_bearmod.png", grizzinc.plot)

# Plot BHS:
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = seq_range(habsuit, n=300),
                    logpopdens = mean(logpopdens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                         newdata=simdata,
                                         ndraws=1000,
                                         re_formula=NA)

postdraws$habsuit_un <- (postdraws$habsuit * attributes(bear.conflict.df.scl$habsuit)[[3]])+attributes(bear.conflict.df.scl$habsuit)[[2]]

plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(habsuit_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
habsuit.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = habsuit_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=habsuit_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Predicted Grizzly Bear Habitat Suitability")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/dhabsuit_plot_bearmod.png" ,habsuit.plot )

# Plot pop dens:
  simdata <- bear.conflict.df.scl %>%
    modelr::data_grid(dist2pa = mean(dist2pa),
                      dist2grizz = mean(dist2grizz),
                      livestockOps = mean(livestockOps),
                      rowcropOps = mean(rowcropOps),
                      connectivity = mean(connectivity),
                      grizzinc = mean(grizzinc),
                      habsuit = mean(habsuit),
                      logpopdens = seq_range(logpopdens, n=300),
                      conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))
  
postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad4, 
                                           newdata=simdata,
                                           ndraws=1000,
                                           re_formula=NA)
  
postdraws$humandens_un <- (postdraws$logpopdens * attributes(bear.conflict.df.scl$logpopdens)[[3]])+attributes(bear.conflict.df.scl$logpopdens)[[2]]
  
plot.df <- postdraws %>% 
    mutate_at(., vars(conflictprob), as.factor) %>% 
    group_by(humandens_un, conflictprob) %>% 
    summarise(., mean = mean(.epred),
              lo = quantile(.epred, 0.2),
              hi = quantile(.epred, 0.8))
  
levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
humandens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = humandens_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=humandens_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Human Population Density")+
         theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
ggsave("Data/processed/humandens_plot_bearmod.png", humandens.plot)

  # Add Plots together:
biophys.p <-  connectivity.plot + habsuit.plot + dist2pa.plot.b + dist2grizz.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')         

social.p <- grizzinc.plot + humandens.plot + livestockOps.plot.b + rowcropOps.plot.b + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

plot.all <- connectivity.plot + habsuit.plot + dist2pa.plot.b + dist2grizz.plot + grizzinc.plot + humandens.plot + livestockOps.plot.b + rowcropOps.plot.b + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

ggsave("Data/processed/biophys_bear_conf_plots.png", biophys.p)
ggsave("Data/processed/social_bear_conf_plots.png", social.p)
ggsave("Data/processed/bear_conf_plots.png" , plot.all)

# generate spatial pred ---------------------------------------------------
fixed.effects <- fixef(bear.full.mod.quad4)
var.int <- ranef(bear.full.mod.quad)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")


ccs.sf <- st_read("Data/processed/SOI_CCS_10km.shp")
ccs.sf.join <- ccs.sf %>% left_join(., var.int)
ccs.sf.join$`(Intercept)`[is.na(ccs.sf.join$`(Intercept)`)] <- 0

#load predictor rasters
dist.2.pa <- rast("Data/processed/dist2pa_SOI_10km.tif") 
pop.dens <- log(rast("Data/processed/human_dens_SOI_10km.tif")+1, 10)
animal.dens <- rast("Data/processed/animal_production_density_cropped.tif")
rowcrop.dens <- rast("Data/processed/ground_crop_density_cropped.tif")
dist.2.grizz <- rast("Data/processed/dist2grizz_pop_raster.tif")
bhs <- rast("Data/processed/bhs_SOI_10km.tif")
grizinc <- rast("Data/processed/grizz_inc_SOI_10km.tif")
biophys <- rast("Data/processed/biophys_SOI_10km.tif")
conflict <- rast("Data/processed/prob_conflict_all.tif")


pop.d.crop <- crop(pop.dens, animal.dens)
pop.dens <- mask(pop.d.crop, animal.dens)
bhs <- crop(bhs, animal.dens)
grizinc <- crop(grizinc, animal.dens)
writeRaster(bhs, "Data/processed/habsuit_SOI_10km.tif", overwrite=TRUE)
writeRaster(grizinc, "Data/processed/grizinc_SOI_10km.tif", overwrite=TRUE)

#Create global intercept raster
global.int <- dist.2.pa
global.int[!is.na(global.int)] <- fixed.effects[[1]]

#create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.pa, field='(Intercept)')

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

  # Add our Rasters:
pred.stack <- c(dist2pa.pred, grizz.dist.pred, animal.dens.pred, rowcrop.dens.pred, rowcrop.dens.quad.pred, biophys.pred, grizzinc.pred, grizzinc.quad.pred, bhs.pred, bhs.quad.pred, pop.dens.pred, pop.dens.quad.pred,      conflict.pred, conflict.quad.prd)

linpred.rst <- sum(pred.stack)
prob.rast <- (exp(linpred.rst))/(1 + exp(linpred.rst))
writeRaster(prob.rast, "Data/processed/prob_conflict_bear.tif")
