# Shoreline analysis
# Paige Monteiro
# Fall 2025

library(sf)
library(dplyr)


shore_unitlines <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg", 
                           layer = "ShoreUnitClassification_updated_18jun25") %>%
  st_zm(drop = TRUE, what = "ZM") %>%      
  st_cast("MULTILINESTRING") %>%
  st_transform(3005)                      

wakde_95 <- st_read("/Users/paigemonteiro/Documents/ArcGIS/Habitat_covariates_4Dec24.gdb", 
                    layer = "kdes_95_Merge_17jun25") %>%
  st_transform(3005)                      

# reclassify coastal class
shore_unitlines <- shore_unitlines %>%
  mutate(coastal_class_reclass = case_when(
    COASTAL_CLASS %in% c(1,2) ~ 1,
    COASTAL_CLASS %in% c(3,4,5) ~ 2,
    COASTAL_CLASS %in% c(6,7) ~ 3,
    COASTAL_CLASS %in% c(8,9,10) ~ 4,
    COASTAL_CLASS %in% c(11,12,16,17) ~ 5,
    COASTAL_CLASS %in% c(13,14,15,18,19,20) ~ 6,
    COASTAL_CLASS %in% c(21,22,23) ~ 7,
    COASTAL_CLASS %in% c(24,25,26) ~ 8,
    COASTAL_CLASS %in% c(28,30) ~ 9,
    COASTAL_CLASS %in% c(29,31,32,33) ~ 10,
    TRUE ~ NA_real_))

summarise_shore <- function(hr_row) {
  hr_id <- hr_row$Field
  hr_geom <- st_geometry(hr_row)
  shore_clip <- st_intersection(shore_unitlines, hr_geom)
  shore_clip <- shore_clip %>%
    mutate(length_km = as.numeric(st_length(st_geometry(.))) / 1000)
  shore_summary <- shore_clip %>%
    st_drop_geometry() %>%
    group_by(id = hr_id, coastal_class_reclass) %>%
    summarise(class_length_km = sum(length_km, na.rm = TRUE), .groups = "drop") %>%
    mutate(total_length_km = sum(class_length_km, na.rm = TRUE),
           percent = (class_length_km / total_length_km) * 100)
  return(shore_summary)}

shore_summary_all <- wakde_95 %>%
  split(.$Field) %>%             
  lapply(summarise_shore) %>%
  bind_rows()

shore_summary_all <- shore_summary_all %>%
  mutate(coastal_class_5 = case_when(
    coastal_class_reclass %in% c(1,2) ~ 1,
    coastal_class_reclass %in% c(3,4) ~ 2,
    coastal_class_reclass %in% c(5,6) ~ 3,
    coastal_class_reclass %in% c(7,8,9) ~ 4,
    coastal_class_reclass %in% c(10) ~ 5,
    TRUE ~ NA_real_)) %>%
  group_by(id, coastal_class_5, total_length_km) %>%
  summarise(
    class_length_sum_5 = sum(class_length_km, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(percent_5 = (class_length_sum_5 / total_length_km) * 100) %>%
  rename(total_length_id = total_length_km) %>%
  ungroup()

shore_wide <- shore_summary_all %>%
  pivot_wider(
    id_cols = c(id, total_length_id),
    names_from = coastal_class_5,
    values_from = c(class_length_sum_5, percent_5),
    names_glue = "cc{coastal_class_5}_{.value}")

##
hr <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Home Range/home_range_data_final_9sep25.csv")

shore_wide <- shore_wide %>%
  left_join(hr %>% select(id, region, species), by = "id")

summary_df <- shore_wide %>%
  group_by(region, species) %>%
  summarise(
    across(
      contains("class_length_sum_5"),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd   = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}",
    across(
      contains("percent_5"),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd   = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"),
    total_length_mean = mean(total_length_id, na.rm = TRUE),
    total_length_sd   = sd(total_length_id, na.rm = TRUE),
    .groups = "drop")

write.csv(shore_summary_all, "/Users/paigemonteiro/Documents/Habitat Use/shore_summary_per_id_26sep25.csv", row.names = FALSE)
write.csv(shore_wide, "/Users/paigemonteiro/Documents/Habitat Use/shore_summary_wide_26sep25.csv", row.names = FALSE)
write.csv(summary_df, "/Users/paigemonteiro/Documents/Habitat Use/shore_summary_region_species_26sep25.csv", row.names = FALSE)

shore_wide <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/shore_summary_wide_26sep25.csv")

# MANOVA
library(dplyr)

shore_w_for_manova <- shore_wide %>%
  dplyr::mutate(across(starts_with("cc") & ends_with("percent_5"),
                ~replace_na(., 0))) %>%
  dplyr::select(id, region, species, starts_with("cc") & ends_with("percent_5")) %>%
  filter(complete.cases(.))

manova_m <- manova(cbind(cc1_percent_5, 
                         cc2_percent_5, 
                         cc3_percent_5,
                         cc4_percent_5,
                         cc5_percent_5) ~ region * species,
                   data = shore_w_for_manova)
manova_m
summary(manova_m)

#Call:
#  manova(cbind(cc1_percent_5, cc2_percent_5, cc3_percent_5, cc4_percent_5, 
#               cc5_percent_5) ~ region * species, data = shore_w_for_manova)

#Terms:
#                   region   species region:species Residuals
#cc1_percent_5    2090.154   637.619         18.595 14417.475
#cc2_percent_5    4867.839   600.098         71.321  2582.971
#cc3_percent_5     591.446    10.011        121.155  5274.302
#cc4_percent_5    5038.212  1535.643        411.856 11743.263
#cc5_percent_5      88.590    35.918          4.358   785.507
#Deg. of Freedom         4         1              1        38

#Residual standard errors: 19.47838 8.244569 11.78123 17.57934 4.546564
#3 out of 10 effects not estimable
#Estimated effects may be unbalanced
#> summary(manova_m)
#                  Df  Pillai  approx F  num Df den Df    Pr(>F)    
#region            4  1.19087   3.1371     20    148     3.632e-05 ***    # highly signifcant
#  species         1  0.26375   2.4360      5     34     0.05443 .        # marginally not significant
#region:species    1  0.14436   1.1473      5     34     0.35490          # not significant - the effect of region does not depend on species 
#Residuals      38                                             
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary.aov(manova_m)
#> summary.aov(manova_m)
#Response cc1_percent_5 :                           # no strong differences for class 1
#  Df  Sum Sq Mean Sq F value Pr(>F)
#region          4  2090.2  522.54  1.3772 0.2601
#species         1   637.6  637.62  1.6806 0.2027
#region:species  1    18.6   18.59  0.0490 0.8260
#Residuals      38 14417.5  379.41               

#Response cc2_percent_5 :                               # class 2 - strongest driver of differences (for region and species)
#  Df Sum Sq Mean Sq F value    Pr(>F)    
#region          4 4867.8 1216.96 17.9036 2.432e-08 ***
#  species         1  600.1  600.10  8.8285   0.00512 ** 
#  region:species  1   71.3   71.32  1.0493   0.31216    
#Residuals      38 2583.0   67.97                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Response cc3_percent_5 :                            # no strong differences for class 3 
#  Df Sum Sq Mean Sq F value Pr(>F)
#region          4  591.4 147.862  1.0653 0.3870
#species         1   10.0  10.011  0.0721 0.7897
#region:species  1  121.2 121.155  0.8729 0.3561
#Residuals      38 5274.3 138.797               

#Response cc4_percent_5 :                                   # class 4 - also contributing to differences 
#  Df  Sum Sq Mean Sq F value   Pr(>F)   
#region          4  5038.2 1259.55  4.0758 0.007596 **
#  species         1  1535.6 1535.64  4.9692 0.031792 * 
#  region:species  1   411.9  411.86  1.3327 0.255530   
#Residuals      38 11743.3  309.03                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Response cc5_percent_5 :                                 # class 5 no strong differences 
#  Df Sum Sq Mean Sq F value Pr(>F)
#region          4  88.59  22.147  1.0714 0.3841
#species         1  35.92  35.918  1.7376 0.1953
#region:species  1   4.36   4.358  0.2108 0.6487
#Residuals      38 785.51  20.671 


# post-hoc
aov_cc2 <- aov(cc2_percent_5 ~ region * species, data = shore_w_for_manova)
TukeyHSD(aov_cc2, "region")

#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = cc2_percent_5 ~ region * species, data = shore_w_for_manova)
#
#$region
#                                                      diff         lwr         upr     p adj
#haida gwaii-bamfield                               -50.9337102 -72.4817570 -29.3856633 0.0000005
#nanaimo-central gulf islands-bamfield              -34.5974683 -53.8706273 -15.3243093 0.0000805
#northern gulf islands-bamfield                     -34.0387081 -48.6078469 -19.4695693 0.0000006
#sunshine coast-bamfield                            -41.4560549 -56.3070213 -26.6050885 0.0000000
#nanaimo-central gulf islands-haida gwaii            16.3362419  -5.2118050  37.8842887 0.2127776
#northern gulf islands-haida gwaii                   16.8950021  -0.5727817  34.3627858 0.0621400
#sunshine coast-haida gwaii                           9.4776553  -8.2258717  27.1811823 0.5483136
#northern gulf islands-nanaimo-central gulf islands   0.5587602 -14.0103786  15.1278990 0.9999655
#sunshine coast-nanaimo-central gulf islands         -6.8585866 -21.7095530   7.9923799 0.6793330
#sunshine coast-northern gulf islands                -7.4173467 -15.2503762   0.4156827 0.0708677

aov_cc4 <- aov(cc4_percent_5 ~ region * species, data = shore_w_for_manova)
TukeyHSD(aov_cc4, "region")
#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = cc4_percent_5 ~ region * species, data = shore_w_for_manova)
#
#$region
#diff        lwr        upr     p adj
#haida gwaii-bamfield                                50.354755    4.40930  96.300211 0.0256395
#nanaimo-central gulf islands-bamfield               -8.781206  -49.87607  32.313659 0.9723496
#northern gulf islands-bamfield                       2.539874  -28.52492  33.604672 0.9993038
#sunshine coast-bamfield                              8.374135  -23.29158  40.039855 0.9411553
#nanaimo-central gulf islands-haida gwaii           -59.135961 -105.08142 -13.190506 0.0060267
#northern gulf islands-haida gwaii                  -47.814881  -85.06027 -10.569497 0.0061867
#sunshine coast-haida gwaii                         -41.980620  -79.72866  -4.232576 0.0228142
#northern gulf islands-nanaimo-central gulf islands  11.321080  -19.74372  42.385878 0.8335414
#sunshine coast-nanaimo-central gulf islands         17.155341  -14.51038  48.821061 0.5368636
#sunshine coast-northern gulf islands                 5.834261  -10.86758  22.536105 0.8536484
