# GAMMs
# Fall 2025
# Paige Monteiro


library(mgcv)
library(DHARMa)
library(performance)
library(effects)
library(spdep)
library(ggeffects)
library(ggplot2)
library(patchwork)
library(dplyr)
library(gratia)
library(sf)

# refer to FINAL_GAMs_17sept25 for code for final model selection
points <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_17sep25_v3.csv") 

points <- points %>%
  mutate(longitude = longitude, latitude = latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 32610, remove = FALSE)

points$type <- as.factor(points$type)
points$islet <- as.factor(points$islet)
points$percent_intertidal <- as.numeric(points$percent_intertidal)
points$shallow_substrate <- as.factor(points$shallow_substrate)
points$species <- as.factor(points$species)
points$tag_ID <- as.factor(points$tag_ID)
points$site_capture <- as.factor(points$site_capture)
points$region_capture <- as.factor(points$region_capture)
points$dist_to_spawn <- as.numeric(points$dist_to_spawn)
points$dist_to_fwa_stream <- as.numeric(points$dist_to_fwa_stream)
points$canopy_cover <- as.numeric(points$canopy_cover)
points$CEF_DISTURB <- as.factor(points$CEF_DISTURB)
points$structure <- as.factor(points$structure)
points$percent_CEFdisturb <- as.numeric(points$percent_CEFdisturb)

points <- as.data.frame(points)  
model_vars <- c(
  "type", "islet", "species", "shallow_substrate", "percent_intertidal",
  "canopy_cover", 
  "tag_ID", "region_capture", "longitude", "latitude", "percent_CEFdisturb")
points <- points[complete.cases(points[, model_vars]), ]
points <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 32610, remove = FALSE)

points <- points %>%
  mutate(shallow_sub_num = case_when(
    shallow_substrate == "Mud" ~ 4,
    shallow_substrate == "Rock" ~ 1,
    shallow_substrate == "Mixed" ~ 2,
    shallow_substrate == "Sand" ~ 3,
    TRUE ~ NA_real_ ))

points$shallow_sub_num <- as.numeric(points$shallow_sub_num)

# BLTU

points_bltu <- points %>% 
  dplyr::filter(species == "BLTU")

points_bltu$shallow_sub_num <- as.factor(points_bltu$shallow_sub_num)

# final model: 
gam_bltu_f2 <- gam(type ~ #dist_to_fwa_stream + 
                     #shallow_sub_num +         
                     percent_intertidal +
                     canopy_cover +
                     s(percent_CEFdisturb, bs = "cr", k = 4) +
                     s(longitude, latitude, bs = "gp", k = 100, m=2) +
                     #s(region_capture, bs = "re") +
                     s(tag_ID, bs = "re"),
                   data = points_bltu,
                   family = binomial,
                   method = "ML",
                   select=TRUE)
summary(gam_bltu_f2)
# Family: binomial 
#Link function: logit 
#
#Formula:
# type ~ percent_intertidal + canopy_cover + s(percent_CEFdisturb, 
#                                               bs = "cr", k = 4) + s(longitude, latitude, bs = "gp", k = 100, 
#                                                                     m = 2) + s(tag_ID, bs = "re")
#
#Parametric coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        -1.124748   0.326364  -3.446 0.000568 ***
#  percent_intertidal  0.053718   0.007666   7.007 2.43e-12 ***
#  canopy_cover       -0.064851   0.011256  -5.761 8.34e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#  edf Ref.df  Chi.sq  p-value    
#s(percent_CEFdisturb)  2.047      3   24.63 8.06e-07 ***
#  s(longitude,latitude) 74.693     99 1129.84  0.00751 ** 
#  s(tag_ID)             13.764     22   56.52  < 2e-16 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#R-sq.(adj) =  0.583   Deviance explained =   53%
#-ML = 867.87  Scale est. = 1         n = 2126

# figures:
pred_eff <- ggpredict(gam_bltu_f2, terms = "percent_intertidal [all]")
plot(pred_eff) 

pred_eff <- ggpredict(gam_bltu_f2, terms = "percent_intertidal [all]")
plot(pred_eff) 
pred_df <- as.data.frame(pred_eff)

ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.5) +
  geom_ribbon(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
              alpha = 0.3, fill = "dodgerblue2") +
  theme_classic() +
  labs(x = "Intertidal area (%)", y = "Predicted probability of use") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))


pred_eff <- ggpredict(gam_bltu_f2, terms = "canopy_cover [all]")
plot(pred_eff) 
pred_df <- as.data.frame(pred_eff)

ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.5) +
  geom_ribbon(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
              alpha = 0.3, fill = "dodgerblue2") +
  theme_classic() +
  labs(x = "Canopy Cover (%)", y = "Predicted probability of use") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))



pred_eff <- ggpredict(gam_bltu_f2, terms = "percent_CEFdisturb [all]")
plot(pred_eff) 
pred_df <- as.data.frame(pred_eff)

ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.5) +
  geom_ribbon(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
              alpha = 0.3, fill = "dodgerblue2") +
  theme_classic() +
  labs(x = "Disturbed area (%)", y = "Predicted probability of use") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))



# SURF

points_surf <- points %>% 
  dplyr::filter(species == "SURF")

points_surf$shallow_sub_num <- as.factor(points_surf$shallow_sub_num)

# final model
gam_surf_f2 <- gam(type ~ #s(dist_to_fwa_stream, bs = "cr", k = 4) +
                     shallow_sub_num +
                     percent_intertidal +
                     s(canopy_cover, bs = "cr", k = 4) +
                     percent_CEFdisturb +
                     s(longitude, latitude, bs = "gp", k = 100, m=2) +
                     #s(region_capture, bs = "re") +
                     s(tag_ID, bs = "re"),
                   data = points_surf,
                   family = binomial,
                   method = "ML",
                   select=TRUE)

summary(gam_surf_f2)

#Parametric coefficients:
#                      Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          -0.440280   0.315750  -1.394   0.1632    
#shallow_sub_num2     -0.365477   0.402945  -0.907   0.3644    
#shallow_sub_num3      1.088930   0.441608   2.466   0.0137 *  
#  shallow_sub_num4   -1.447790   1.157015  -1.251   0.2108    
#percent_intertidal    0.013430   0.007264   1.849   0.0645 .  
#percent_CEFdisturb   -0.032534   0.007899  -4.119 3.81e-05 ***
#
#Approximate significance of smooth terms:
#                            edf Ref.df  Chi.sq  p-value    
#s(canopy_cover)          2.357      3  126.33  < 2e-16 ***
#  s(longitude,latitude) 74.277     99 3325.81 6.06e-06 ***
#  s(tag_ID)             14.347     18   84.47  < 2e-16 ***
#
#R-sq.(adj) =  0.646   Deviance explained = 59.6%
#-ML = 948.03  Scale est. = 1         n = 2733

# figures

pred_eff <- ggpredict(gam_surf_f2, 
                      terms = "canopy_cover [all]", 
                      condition = c(longitude = median(points_surf$longitude),
                                    latitude = median(points_surf$latitude)))
plot(pred_eff) 
pred_df <- as.data.frame(pred_eff)
ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.5) +
  geom_ribbon(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
              alpha = 0.3, fill = "orange2") +
  theme_classic() +
  labs(x = "Canopy cover (%)", y = "Predicted probability of use") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))


pred_eff <- ggpredict(gam_surf_f2, 
                      terms = "percent_intertidal [all]", 
                      condition = c(longitude = median(points_surf$longitude),
                                    latitude = median(points_surf$latitude)))
plot(pred_eff) 

pred_df <- as.data.frame(pred_eff)
ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.5) +
  geom_ribbon(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
              alpha = 0.3, fill = "orange2") +
  theme_classic() +
  labs(x = "Intertidal area (%)", y = "Predicted probability of use") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))


pred_eff <- ggpredict(gam_surf_f2, 
                      terms = "percent_CEFdisturb [all]", 
                      condition = c(longitude = median(points_surf$longitude),
                                    latitude = median(points_surf$latitude)))
plot(pred_eff) 
pred_df <- as.data.frame(pred_eff)
ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.5) +
  geom_ribbon(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
              alpha = 0.3, fill = "orange2") +
  theme_classic() +
  labs(x = "Disturbed area (%)", y = "Predicted probability of use") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))


pred_eff <- ggpredict(gam_surf_f2, terms = "shallow_sub_num [all]")
plot(pred_eff)
pred_eff <- ggpredict(gam_surf_f2, 
                      terms = "shallow_sub_num [all]", 
                      condition = c(longitude = median(points_surf$longitude),
                                    latitude = median(points_surf$latitude)))
plot(pred_eff) 
pred_df <- as.data.frame(pred_eff)
ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = pmax(0, conf.low), ymax = pmin(1, conf.high)),
                width = 0.2, alpha = 0.6, color = "orange2", size = 1.5) +  # thicker bars
  geom_point(size = 4, color = "black") +
  theme_classic() +
  labs(x = "Shallow substrate size class", y = "Predicted probability of use") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1))
