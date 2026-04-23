# Univariate null modelling
# summer 2025
# Paige Monteiro

library(sf)
library(dplyr)
library(ggplot2)

points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_17sep25_v2.csv')   #425689, all bird pts, 10k random
head(points)

random <- points %>% dplyr::filter(type == 0)

bird <- points %>% dplyr::filter(type == 1)

# percent intertidal
mean_int_bird <- mean(bird$percent_intertidal, na.rm = TRUE)

mean_int_random <- mean(random$percent_intertidal, na.rm = TRUE)

bird_summary <- bird %>%
  group_by(tag_ID) %>%
  summarise(
    mean_intertidal = mean(percent_intertidal, na.rm = TRUE))

random_summary <- random %>%
  group_by(tag_ID) %>%
  summarise(
    mean_random = mean(percent_intertidal, na.rm = TRUE))

bird_summary <- bird_summary %>%
  left_join(random_summary, by = "tag_ID") %>%
  mutate(above_expected = mean_intertidal > mean_random)

successes <- sum(bird_summary$above_expected, na.rm = TRUE)
total_birds <- nrow(bird_summary)
successes
total_birds

binom.test(x = successes,
           n = total_birds,
           p = 0.5,         
           alternative = "greater")

#Exact binomial test
#
#data:  successes and total_birds
#number of successes = 27, number of trials = 42, p-value = 0.04421
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.5045431 1.0000000
#sample estimates:
#  probability of success 
#0.6428571 


# aspect
mean_asp_bird <- mean(bird$aspect_value, na.rm = TRUE)

mean_asp_ran <- mean(random$aspect_value, na.rm=TRUE)

southness <- function(aspect) {
  cos((aspect - 180) * pi/180) * -1 + 1}

bird <- bird %>%
  mutate(southness = cos((aspect_value - 180) * pi/180) * -1 + 1)

random <- random %>%
  mutate(southness = cos((aspect_value - 180) * pi/180) * -1 + 1)

bird_summary <- bird %>%
  group_by(tag_ID) %>%
  summarise(mean_southness = mean(southness, na.rm = TRUE))

random_summary <- random %>%
  group_by(tag_ID) %>%
  summarise(mean_southness_ran = mean(southness, na.rm = TRUE))

bird_summary <- bird_summary %>%
  left_join(random_summary, by = "tag_ID") %>%
  mutate(above_expected = mean_southness > mean_southness_ran)

successes <- sum(bird_summary$above_expected, na.rm = TRUE)
total_birds <- nrow(bird_summary)

binom.test(x = successes,
           n = total_birds,
           p = 0.5, 
           alternative = "greater")

#Exact binomial test#
#
#data:  successes and total_birds
#number of successes = 26, number of trials = 42, p-value = 0.08207
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.4804967 1.0000000
#sample estimates:
#  probability of success 
#0.6190476 



# wave exposure 
random$EXP_FINAL <- trimws(tolower(random$EXP_FINAL))
bird$EXP_FINAL <- trimws(tolower(bird$EXP_FINAL))
random_clean <- random %>% filter(!is.na(EXP_FINAL))
bird_clean <- bird %>% filter(!is.na(EXP_FINAL))

contingency_table <- data.frame(
  category = c("e", "p", "se", "sp", "vp"),
  random_count = c(sum(random_clean$EXP_FINAL == "e"),
                   sum(random_clean$EXP_FINAL == "p"),
                   sum(random_clean$EXP_FINAL == "se"),
                   sum(random_clean$EXP_FINAL == "sp"),
                   sum(random_clean$EXP_FINAL == "vp")),
  bird_count = c(sum(bird_clean$EXP_FINAL == "e"),
                 sum(bird_clean$EXP_FINAL == "p"),
                 sum(bird_clean$EXP_FINAL == "se"),
                 sum(bird_clean$EXP_FINAL == "sp"),
                 sum(bird_clean$EXP_FINAL == "vp")))
print(contingency_table)

chi_square_test <- chisq.test(contingency_table[, 2:3])
print(chi_square_test)
#	Pearson's Chi-squared test
#data:  contingency_table[, 2:3]
#X-squared = 114.18, df = 4, p-value < 2.2e-16

# proportion test for each category of wave exposure  
proportion_tests <- sapply(c("e", "p", "se", "sp", "vp"), function(cat) {
  random_prop <- sum(random_clean$EXP_FINAL == cat) / nrow(random_clean)
  bird_prop <- sum(bird_clean$EXP_FINAL == cat) / nrow(bird_clean)
  prop_test <- prop.test(
    x = c(sum(random_clean$EXP_FINAL == cat), sum(bird_clean$EXP_FINAL == cat)),
    n = c(nrow(random_clean), nrow(bird_clean)))
  return(prop_test$p.value)})

names(proportion_tests) <- c("e", "p", "se", "sp", "vp")
print(proportion_tests)
#. e            p           se           sp           vp 
#1.640592e-01 9.527711e-21 1.786937e-07 3.558386e-04 1.428611e-02 

# proportion difference 
proportion_diff <- sapply(c("e", "p", "se", "sp", "vp"), function(cat) {
  random_prop <- sum(random_clean$EXP_FINAL == cat) / nrow(random_clean)
  bird_prop <- sum(bird_clean$EXP_FINAL == cat) / nrow(bird_clean)
  return(random_prop - bird_prop)})
print(proportion_diff)

# e            p           se           sp           vp 
#0.003875018  0.105127155 -0.062095083 -0.054736201  0.007829112 


# distance to stream
mean_str_bird <- mean(bird$dist_to_fwa_stream, na.rm = TRUE)

mean_str_random <- mean(random$dist_to_fwa_stream, na.rm = TRUE)

## two sampel t-test
fwa_test <- t.test(
  bird$dist_to_fwa_stream,
  random$dist_to_fwa_stream,
  alternative = "two.sided",
  var.equal = FALSE)

fwa_test

#Welch Two Sample t-test
#data:  bird$dist_to_fwa_stream and random$dist_to_fwa_stream
#t = 36.408, df = 5904.7, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  842.8903 938.8245
#sample estimates:
#  mean of x mean of y 
#2775.337  1884.480


bird_summary_stream <- bird %>%
  group_by(tag_ID) %>%
  summarise(mean_stream = mean(dist_to_fwa_stream, na.rm = TRUE))

random_summary_stream <- random %>%
  group_by(tag_ID) %>%
  summarise(mean_random_stream = mean(dist_to_fwa_stream, na.rm = TRUE))

# success column = closer to stream than random
bird_summary_stream <- bird_summary_stream %>%
  left_join(random_summary_stream, by = "tag_ID") %>%
  mutate(closer_to_stream = mean_stream < mean_random_stream)

successes_stream <- sum(bird_summary_stream$closer_to_stream, na.rm = TRUE)
total_birds <- nrow(bird_summary_stream)

binom.test(x = successes_stream,
           n = total_birds,
           p = 0.5,           # null hypothesis: 50% closer than random
           alternative = "greater")

#	Exact binomial test
#data:  successes_stream and total_birds
#number of successes = 7, number of trials = 42, p-value = 1
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.08093248 1.00000000
#sample estimates:
#  probability of success 
#0.1666667 


# islets

total_points <- nrow(bird)
islet_points <- sum(bird$islet == "y", na.rm = TRUE)
prop_islet_bird <- islet_points / total_points

islet_p <- bird %>% dplyr::filter(islet == "y")

islet_p$tide_height_m <- islet_p$tide_height_ft * 0.3048

bird$tide_height_m <- bird$tide_height_ft * 0.3048

bird$islet1 <- as.numeric(bird$islet == "y")

bird$islet1 <- as.factor(bird$islet1)

bird_b <- bird %>% dplyr::filter(species == "BLTU")

islet_m <- glm(islet1 ~ tide_height_m,
               data = bird_b,
               family = binomial(link = "logit"),
               na.action = na.fail)

total_points <- nrow(random)
islet_points <- sum(random$islet == "y", na.rm = TRUE)
prop_islet_random <- islet_points / total_points

islet_points_bird <- sum(bird$islet == "y", na.rm = TRUE)
total_points_bird <- nrow(bird)
islet_points_random <- sum(random$islet == "y", na.rm = TRUE)
total_points_random <- nrow(random)
prop_test_islet <- prop.test(
  c(islet_points_bird, islet_points_random),
  c(total_points_bird, total_points_random),
  correct = TRUE)
print(prop_test_islet)
# chi-squared = 2628.5
#	p-value < 2.2e-16
#	95% confidence interval for difference in proportions: 
#95 percent confidence interval:
#  0.2148095 0.2398639


# calculate mean islet use per individual
bird_summary_islet <- bird %>%
  group_by(tag_ID) %>%
  summarise(mean_islet = mean(islet == "y", na.rm = TRUE))
random_summary_islet <- random %>%
  group_by(tag_ID) %>%
  summarise(mean_random_islet = mean(islet == "y", na.rm = TRUE))

bird_summary_islet <- bird_summary_islet %>%
  left_join(random_summary_islet, by = "tag_ID") %>%
  mutate(above_expected = mean_islet > mean_random_islet)

successes_islet <- sum(bird_summary_islet$above_expected, na.rm = TRUE)
total_birds <- nrow(bird_summary_islet)

binom.test(x = successes_islet,
           n = total_birds,
           p = 0.5,         
           alternative = "greater")
#Exact binomial test
#data:  successes_islet and total_birds
#number of successes = 37, number of trials = 42, p-value = 2.217e-07
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.7658435 1.0000000
#sample estimates:
#  probability of success 
#0.8809524 



bird_surf <- bird %>% dplyr::filter(species == "SURF")
bird_bltu <- bird %>% dplyr::filter(species == "BLTU")
random_surf <- random %>% dplyr::filter(species == "SURF")
random_bltu <- random %>% dplyr::filter(species == "BLTU")

total_points <- nrow(bird_surf)
islet_points <- sum(bird_surf$islet == "y", na.rm = TRUE)
prop_islet_bird_s <- islet_points / total_points

total_points <- nrow(random_surf)
islet_points <- sum(random_surf$islet == "y", na.rm = TRUE)
prop_islet_random_s <- islet_points / total_points

total_points <- nrow(bird_bltu)
islet_points <- sum(bird_bltu$islet == "y", na.rm = TRUE)
prop_islet_bird_b <- islet_points / total_points

total_points <- nrow(random_bltu)
islet_points <- sum(random_bltu$islet == "y", na.rm = TRUE)
prop_islet_random_b <- islet_points / total_points

points2aday <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_8jul25.csv')
random2 <- points2aday %>% dplyr::filter(type == 0)
bird2 <- points2aday %>% dplyr::filter(type == 1)
bird_surf <- bird2 %>% dplyr::filter(species == "SURF")
random_surf <- random2 %>% dplyr::filter(species == "SURF")

total_points <- nrow(bird_surf)
islet_points <- sum(bird_surf$islet == "y", na.rm = TRUE)
prop_islet_bird_s <- islet_points / total_points

total_points <- nrow(random_surf)
islet_points <- sum(random_surf$islet == "y", na.rm = TRUE)
prop_islet_random_s <- islet_points / total_points



# CEF disturb
total_points <- nrow(bird)
cef_points <- sum(bird$CEF_DISTURB == "y", na.rm = TRUE)
prop_cef_bird <- cef_points / total_points

total_points <- nrow(random)
cef_points <- sum(random$CEF_DISTURB == "y", na.rm = TRUE)
prop_cef_random <- cef_points / total_points

cef_points_bird <- sum(bird$CEF_DISTURB == "y", na.rm = TRUE)
total_points_bird <- nrow(bird)
cef_points_random <- sum(random$CEF_DISTURB == "y", na.rm = TRUE)
total_points_random <- nrow(random)
# two-sample proportion test - because categorical data 
prop_test_result <- prop.test(c(cef_points_bird, cef_points_random), 
                              c(total_points_bird, total_points_random))
print(prop_test_result)


bird_summary_cef <- bird %>%
  group_by(tag_ID) %>%
  summarise(mean_cef = mean(CEF_DISTURB == "y", na.rm = TRUE))
random_summary_cef <- random %>%
  group_by(tag_ID) %>%
  summarise(mean_random_cef = mean(CEF_DISTURB == "y", na.rm = TRUE))

bird_summary_cef <- bird_summary_cef %>%
  left_join(random_summary_cef, by = "tag_ID") %>%
  mutate(avoids_disturb = mean_cef < mean_random_cef)

successes_cef <- sum(bird_summary_cef$avoids_disturb, na.rm = TRUE)
total_birds <- nrow(bird_summary_cef)

binom.test(x = successes_cef,
           n = total_birds,
           p = 0.5,          
           alternative = "greater") 
#Exact binomial test

#data:  successes_cef and total_birds
#number of successes = 32, number of trials = 42, p-value = 0.0004703
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.6296825 1.0000000
#sample estimates:
#  probability of success 
#0.7619048



# percent intertidal - during the day only
bird_day <- bird %>% dplyr::filter(day_or_night == "day")
mean_int_bird_day <- mean(bird_day$percent_intertidal, na.rm = TRUE)

mean_int_random <- mean(random$percent_intertidal, na.rm = TRUE)


intday_test <- t.test(
  bird_day$percent_intertidal,
  random$percent_intertidal,
  alternative = "two.sided",  
  var.equal = FALSE)

intday_test




bird_summary_day <- bird_day %>%
  group_by(tag_ID) %>%
  summarise(mean_intertidal_day = mean(percent_intertidal, na.rm = TRUE))

random_summary <- random %>%
  group_by(tag_ID) %>%
  summarise(mean_random = mean(percent_intertidal, na.rm = TRUE))

bird_summary_day <- bird_summary_day %>%
  left_join(random_summary, by = "tag_ID") %>%
  mutate(above_expected = mean_intertidal_day > mean_random)

successes_day <- sum(bird_summary_day$above_expected, na.rm = TRUE)
total_birds <- nrow(bird_summary_day)

binom.test(x = successes_day,
           n = total_birds,
           p = 0.5,           
           alternative = "greater")

#Exact binomial test
#data:  successes_day and total_birds
#number of successes = 29, number of trials = 42, p-value = 0.00976
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.5535649 1.0000000
#sample estimates:
#  probability of success 
#0.6904762

# islets -- night only 
bird_night <- bird %>% dplyr::filter(day_or_night == "night")
total_points <- nrow(bird_night)
islet_points <- sum(bird_night$islet == "y", na.rm = TRUE)
prop_islet_bird_night <- islet_points / total_points

# random data 
# [1] 0.12

total_points <- nrow(bird_day)
islet_points <- sum(bird_day$islet == "y", na.rm=TRUE)
prop_islet_bird_day <- islet_points/total_points


bird_lowtide <- bird %>%
  dplyr::filter(tide_class %in% c("very low", "low"))
total_points <- nrow(bird_lowtide)
islet_points <- sum(bird_lowtide$islet == "y", na.rm = TRUE)
prop_islet_bird_lt <- islet_points / total_points


bird_hitide <- bird %>%
  dplyr::filter(tide_class %in% c("very high", "high"))
total_points <- nrow(bird_hitide)
islet_points <- sum(bird_hitide$islet == "y", na.rm = TRUE)
prop_islet_bird_ht <- islet_points / total_points


# canopy cover
mean_tree_bird <- mean(bird$canopy_cover, na.rm = TRUE)

mean_tree_random <- mean(random$canopy_cover, na.rm = TRUE)

cc_test <- t.test(
  bird$canopy_cover,
  random$canopy_cover,
  alternative = "two.sided", 
  var.equal = FALSE)

cc_test

bird_summary_canopy <- bird %>%
  group_by(tag_ID) %>%
  summarise(mean_canopy = mean(canopy_cover, na.rm = TRUE))
random_summary_canopy <- random %>%
  group_by(tag_ID) %>%
  summarise(mean_random_canopy = mean(canopy_cover, na.rm = TRUE))
bird_summary_canopy <- bird_summary_canopy %>%
  left_join(random_summary_canopy, by = "tag_ID") %>%
  mutate(less_than_expected = mean_canopy < mean_random_canopy)
successes_canopy <- sum(bird_summary_canopy$less_than_expected, na.rm = TRUE)
total_birds <- nrow(bird_summary_canopy)
binom.test(x = successes_canopy,
           n = total_birds,
           p = 0.5,          
           alternative = "greater")  
#Exact binomial test
#data:  successes_canopy and total_birds
#number of successes = 39, number of trials = 42, p-value = 2.816e-09
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.8256085 1.0000000
#sample estimates:
#  probability of success 
#0.9285714 


# shallow substrate 
# random 
shallow_substrate_proportions_r <- random %>%
  group_by(shallow_substrate) %>%
  summarise(proportion = n() / nrow(random) * 100)
print(shallow_substrate_proportions_r)
# mixed = 6.02%
# mud =   3.88%
# rock = 86.9%
# sand =  2.56% 

# bird
shallow_substrate_proportions_b <- bird %>%
  group_by(shallow_substrate) %>%
  summarise(proportion = n() / nrow(bird) * 100)
print(shallow_substrate_proportions_b)
# mixed = 1.67%
# mud =   1.71%
# rock = 92.7%
# sand =  3.85% 

random <- st_drop_geometry(random)
bird <- st_drop_geometry(bird)
 
random$shallow_substrate <- trimws(tolower(random$shallow_substrate))
bird$shallow_substrate <- trimws(tolower(bird$shallow_substrate))
random_clean <- random %>% filter(!is.na(shallow_substrate))
bird_clean <- bird %>% filter(!is.na(shallow_substrate))

contingency_table <- data.frame(
  category = c("mixed", "mud", "rock", "sand"),
  random_count = c(sum(random_clean$shallow_substrate == "mixed"),
                   sum(random_clean$shallow_substrate == "mud"),
                   sum(random_clean$shallow_substrate == "rock"),
                   sum(random_clean$shallow_substrate == "sand")),
  bird_count = c(sum(bird_clean$shallow_substrate == "mixed"),
                 sum(bird_clean$shallow_substrate == "mud"),
                 sum(bird_clean$shallow_substrate == "rock"),
                 sum(bird_clean$shallow_substrate == "sand")))
print(contingency_table)

chi_square_test <- chisq.test(contingency_table[, 2:3])
print(chi_square_test)

# pearons chi squared test
# data:  contingency_table[, 2:3]
# X-squared = 303.39, df = 3, p-value < 2.2e-16


# proportion test for each category of ss 
proportion_tests <- sapply(c("mixed", "mud", "rock", "sand"), function(cat) {
  random_prop <- sum(random_clean$shallow_substrate == cat) / nrow(random_clean)
  bird_prop <- sum(bird_clean$shallow_substrate == cat) / nrow(bird_clean)
  prop_test <- prop.test(
    x = c(sum(random_clean$shallow_substrate == cat), sum(bird_clean$shallow_substrate == cat)),
    n = c(nrow(random_clean), nrow(bird_clean)))
  return(prop_test$p.value)})

names(proportion_tests) <- c("Mixed", "Mud", "Rock", "Sand")
print(proportion_tests)

proportion_diff <- sapply(c("mixed", "mud", "rock", "sand"), function(cat) {
  random_prop <- sum(random_clean$shallow_substrate == cat) / nrow(random_clean)
  bird_prop <- sum(bird_clean$shallow_substrate == cat) / nrow(bird_clean)
  return(random_prop - bird_prop)})
print(proportion_diff)




# looking at only hornby
bird_h <- bird %>% dplyr::filter(region_capture == "Hornby Island")
random_h <- random %>% dplyr::filter(region_capture == "Hornby Island")

# distance to spawn
mean_str_birdh <- mean(bird_h$dist_to_spawn, na.rm = TRUE)
mean_str_random_h <- mean(random_h$dist_to_spawn, na.rm = TRUE)

## two samp t-test
spw_test <- t.test(
  bird_h$dist_to_spawn,
  random_h$dist_to_spawn,
  alternative = "two.sided", 
  var.equal = FALSE)

spw_test
#Welch Two Sample t-test
#data:  bird$dist_to_spawn and random$dist_to_spawn
#t = 12.177, df = 5938.6, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  814.0309 1126.4100
#sample estimates:
#  mean of x mean of y 
#7254.398  6284.177 


bird_summary_spawn <- bird_h %>%
  group_by(tag_ID) %>%
  summarise(mean_dist_spawn = mean(dist_to_spawn, na.rm = TRUE))
random_summary_spawn <- random_h %>%
  group_by(tag_ID) %>%
  summarise(mean_random_spawn = mean(dist_to_spawn, na.rm = TRUE))
bird_summary_spawn <- bird_summary_spawn %>%
  left_join(random_summary_spawn, by = "tag_ID") %>%
  mutate(closer_than_expected = mean_dist_spawn < mean_random_spawn)
successes_spawn <- sum(bird_summary_spawn$closer_than_expected, na.rm = TRUE)
total_birds <- nrow(bird_summary_spawn)

binom.test(x = successes_spawn,
           n = total_birds,
           p = 0.5,           
           alternative = "greater")  
#Exact binomial test
#data:  successes_spawn and total_birds
#number of successes = 5, number of trials = 23, p-value = 0.9987
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.08980908 1.00000000
#sample estimates:
# probability of success 
#0.2173913





