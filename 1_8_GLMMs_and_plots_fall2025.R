# GLMMs
# Fall 2025
# Paige Monteiro

#install.packages("glmmTMB", type = "source")
library(glmmTMB)
library(lubridate)
library(DHARMa)
library(performance)
library(effects)
library(dplyr)
library(car)
library(ggplot2)
library(lme4)
library(sf)
library(ggeffects)

 
points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_equaltype0_21jun25_v4.csv')

points1 <- points %>% dplyr::filter(type == 1)

points1$tide_height_m <- points1$tide_height_ft * 0.3048

points1 %>%
  group_by(tide_class) %>%
  summarise(mean_tide_ft = mean(tide_height_ft, na.rm = TRUE)) %>%
  arrange(mean_tide_ft)

# CHI SQ TEST
# to determine if islet use is signifcant for both species
table_data <- table(points$type, points$islet)
chi <- chisq.test(table_data)
chi
chi$stdres

#Pearson's Chi-squared test with Yates' continuity correction
#
#data:  table_data
#X-squared = 1087.9, df = 1, p-value < 2.2e-16

#              islet
#type          n         y
#0      33.00664 -33.00664
#1     -33.00664  33.00664

# type 1y - 33 sds above expected (birds using islets more than expected)
# type 1n - non islet pts used less than expected (-33)

summary_table <- as.data.frame(table(points$type, points$islet))
colnames(summary_table) <- c("Type", "Islet", "Count")

ggplot(summary_table, aes(x = Islet, y = Count, fill = as.factor(Type))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Islet (Yes/No)", y = "Count", fill = "Point Type") +
  theme_minimal()


# by species
points_surf <- points %>% 
  dplyr::filter(species == "SURF")

table_data <- table(points_surf$type, points_surf$islet)
chi <- chisq.test(table_data)
chi
chi$stdres
# X-squared = 682.83, df = 1, p-value < 2.2e-16
#          n         y
#0  26.16048 -26.16048
#1 -26.16048  26.16048

points_bltu <- points %>% 
  dplyr::filter(species == "BLTU")

table_data <- table(points_bltu$type, points_bltu$islet)
chi <- chisq.test(table_data)
chi
chi$stdres
# X-squared = 405.65, df = 1, p-value < 2.2e-16
#          n         y
#0  20.17582 -20.17582
#1 -20.17582  20.17582

table_data
prop.table(table_data, margin=1) 

# GLMMs

points1 <- points1 %>%
  filter(!is.na(tide_class) & tide_class != "")


points1$tag_ID <- as.factor(points1$tag_ID)
points1$species <- as.factor(points1$species)
points1$day_or_night <- as.factor(points1$day_or_night)
points1$region_capture <- as.factor(points1$region_capture)
points1$islet <- ifelse(points1$islet == "y", 1, 0)
points1$islet <- as.factor(points1$islet)

points1$timestamp_utc <- ymd_hm(points1$timestamp_utc, tz = "UTC")
points1$timestamp_pst_pdt <- with_tz(points1$timestamp_utc, tzone = "America/Vancouver")
head(points1$timestamp_pst_pdt)
str(points1$timestamp_pst_pdt)
points1$hour_pt <- as.numeric(format(points1$timestamp_pst_pdt, "%H"))
points1$hour_pt <- as.factor(points1$hour_pt)
points1$time_of_day_pt <- cut(
  as.numeric(format(points1$timestamp_pst_pdt, "%H")),
  breaks = c(-1, 5, 11, 17, 21, 24),
  labels = c("Night", "Morning", "Afternoon", "Evening", "Night"),
  include.lowest = TRUE,
  right = TRUE)

points1 <- points1 %>%
  dplyr::filter(!is.na(islet) & !is.na(species) & !is.na(tide_height_ft) 
                & !is.na(time_of_day_pt) & !is.na(tag_ID))

table(points1$day_or_night)
#day night 
#4029  1642
table(points1$day_or_night, points1$tide_class)
#       high  low      very high   very low
#day   1000    1133       651       1245
#night  341    363        543        395

                
# SURF
points1$islet_binary <- ifelse(points1$islet == 1, 1, 0)

points_islet1 <- points1 %>% 
  filter(islet_binary == 1)

points_islet0 <- points1 %>% 
  filter(islet_binary == 0)

points1_surf <- points1 %>% filter(species == "SURF")

points1_surf$islet_binary <- ifelse(points1_surf$islet == 1, 1, 0)

islet_model_s <- glmmTMB(islet_binary ~ day_or_night * tide_height_m +
                           (1 | tag_ID),
                         data = points1_surf,
                         family = binomial (link = "logit"),
                         na.action = na.fail)

summary(islet_model_s)

#AIC       BIC    logLik -2*log(L)  df.resid 
#4007.5    4038.1   -1998.8    3997.5      3363 
#
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#tag_ID (Intercept) 0.7691   0.877   
#Number of obs: 3368, groups:  tag_ID, 19
#                                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       -0.43924    0.26680  -1.646  0.09969 .  
#day_or_nightnight                  1.05071    0.25879   4.060 4.91e-05 ***
#  tide_height_m                   -0.15208    0.05072  -2.999  0.00271 ** 
#  day_or_nightnight:tide_height_m -0.18338    0.07821  -2.345  0.01903 *


confint(islet_model_s, parm = "beta_", level = 0.95, method = "wald")
#                                      2.5 %      97.5 %   Estimate
#(Intercept)                     -0.9621551  0.08366922 -0.4392429
#day_or_nightnight                0.5434874  1.55792268  1.0507051
#tide_height_m                   -0.2514814 -0.05267676 -0.1520791
#day_or_nightnight:tide_height_m -0.3366605 -0.03010116 -0.1833809

performance::r2(islet_model_s)
#Conditional R2: 0.210
#Marginal R2: 0.025

# plots
preds <- ggpredict(islet_model_s, terms = c("tide_height_m [all]", "day_or_night"))
ggplot(preds, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Tide height (m)",
    y = "Predicted probability of islet use",
    color = "Time of day",
    fill = "Time of day") +
  theme_classic(base_size = 16)

ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "orange3", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "orange3") +
  facet_wrap(~group) +
  labs(
    x = "Tide height (m)",
    y = "Predicted probability of islet use"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey50") +
  facet_wrap(~group) +
  labs(
    x = "Tide height (m)",
    y = "Predicted probability of islet use") +
  theme_classic(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

aesu <- allEffects(islet_model_s)
plot(aesu)

points1_surf$islet_numeric <- ifelse(points1_surf$islet == "y", 1, 0)
points1_surf$islet <- as.numeric(points1_surf$islet)

hist(points1_surf$tide_height_ft)

# BLTU

points1_bltu <- points1 %>% filter(species == "BLTU")

islet_model_b <- glmmTMB(islet_binary ~ day_or_night * tide_height_m +
                           (1 | region_capture/tag_ID),
                         data = points1_bltu,
                         family = binomial (link = "logit"),
                         na.action = na.fail)

summary(islet_model_b)
#Family: binomial  ( logit )
#Formula:          islet_binary ~ day_or_night * tide_height_m + (1 | region_capture/tag_ID)
#Data: points1_bltu
#
#AIC       BIC    logLik -2*log(L)  df.resid 
#2690.1    2724.5   -1339.0    2678.1      2297 
#
#Random effects:
#  
#  Conditional model:
#  Groups                Name        Variance Std.Dev.
#tag_ID:region_capture (Intercept) 0.8363   0.9145  
#region_capture        (Intercept) 0.1479   0.3845  
#Number of obs: 2303, groups:  tag_ID:region_capture, 23; region_capture, 4
#
#Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                           -0.26123    0.34173  -0.764  0.44461    
#day_or_nightnight                      1.36939    0.41889   3.269  0.00108 ** 
#  tide_height_m                       -0.24279    0.05621  -4.319 1.57e-05 ***
    #  day_or_nightnight:tide_height_m -0.03017    0.12128  -0.249  0.80356    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

confint(islet_model_b, parm = "beta_", level = 0.95, method = "wald")
#                                     2.5 %     97.5 %    Estimate
#(Intercept)                     -0.9310145  0.4085539 -0.26123032
#day_or_nightnight                0.5483905  2.1903969  1.36939370
#tide_height_m                   -0.3529610 -0.1326190 -0.24278997
#day_or_nightnight:tide_height_m -0.2678808  0.2075450 -0.03016791


est <- fixef(islet_model_s)$cond 
ci <- confint(islet_model_s, parm = "beta_", level = 0.95, method = "wald")
out <- cbind(est, ci)


set.seed(123)
sim_res <- simulateResiduals(islet_model_b , n=1000)
sim_res
plot(sim_res)
testDispersion(sim_res)
testZeroInflation(sim_res)
testResiduals(sim_res)
r2(islet_model_b)
# R2 for Mixed Models

#Conditional R2: 0.274
#Marginal R2: 0.057

aeb <- allEffects(islet_model_b)
plot(aeb)

pred_day <- ggpredict(islet_model_b, terms = "day_or_night")

ggplot(pred_day, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, size = 1, color = "dodgerblue4") +
  geom_point(size = 4, color = "black") +
  labs(
    x = "Time of day",
    y = "Predicted probability of islet use") +
  theme_classic(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))



pred_tide <- ggpredict(islet_model_b, terms = "tide_height_m [all]")

ggplot(pred_tide, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "dodgerblue4") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "dodgerblue1") +
  labs(
    x = "Tide height (m)",
    y = "Predicted probability of islet use") +
  theme_classic(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))



preds <- ggpredict(islet_model_b, terms = c("tide_height_m [all]", "day_or_night"))
ggplot(preds, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Tide height (m)",
    y = "Predicted probability of islet use",
    color = "Time of day",
    fill = "Time of day"
  ) +
  theme_classic(base_size = 16)

##### time of day and night x tide height - 
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "dodgerblue1", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "dodgerblue1") +
  facet_wrap(~group) +
  labs(
    x = "Tide height (m)",
    y = "Predicted probability of islet use"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))


pred_tide <- ggpredict(islet_model_s3b, terms = "tide_height_m [all]")

ggplot(pred_tide, aes(x = x, y = predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Tide Height (m)", y = "Predicted Probability of Islet Use") +
  theme_minimal()

 


