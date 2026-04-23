# Home Range - Linear Models
# Summer 2025
# Paige Monteiro

hr <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Home Range/home_range_data_final_12jun25.csv")

hr <- hr %>% rename(KDE_95_km2 = X95_KDE_km2,
                    KDE_50_km2 = X50_KDE_km2)
hr <- hr %>% rename(MCP_95_km2 = MCP_area_km2)

hist(hr$MCP_95_km2,
     main = "Histogram of MCP 95% Home Range",
     xlab = "MCP 95% area (km²)",
     col = "lightblue",
     breaks = 20)

install.packages("e1071") 
library(e1071)
skewness(hr$MCP_95_km2, na.rm = TRUE)


## log1p - (-0.5)
hr$log1p_MCP_95 <- log1p(hr$MCP_95_km2)
skewness(hr$log1p_MCP_95, na.rm = TRUE)
shapiro.test(hr$log1p_MCP_95)

# SC reference level
hr$region <- factor(hr$region)
hr$region <- relevel(hr$region, ref = "sunshine coast")

mod_mcp95 <- lm(log1p_MCP_95 ~ region * species, data = hr)
summary(mod_mcp95)
plot(mod_mcp95)
#Call:
#lm(formula = log1p_MCP_95 ~ region * species, data = hr)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.0580 -0.9192  0.2876  0.7931  2.5772 
#
#Coefficients: (3 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                      2.4882     0.5385   4.621 4.31e-05 ***
#  regionbamfield                                  -0.8123     0.9831  -0.826    0.414    
#regionhaida gwaii                                1.2622     1.1423   1.105    0.276    
#regionnanaimo-central gulf islands               1.3694     0.9498   1.442    0.158    
#regionnorthern gulf islands                      1.4889     0.6888   2.161    0.037 *  
#  speciesSURF                                      0.5754     0.7180   0.801    0.428    
#regionbamfield:speciesSURF                           NA         NA      NA       NA    
#regionhaida gwaii:speciesSURF                        NA         NA      NA       NA    
#regionnanaimo-central gulf islands:speciesSURF       NA         NA      NA       NA    
#regionnorthern gulf islands:speciesSURF         -0.6299     0.9503  -0.663    0.511    
#
#Residual standard error: 1.425 on 38 degrees of freedom
#Multiple R-squared:  0.2491,	Adjusted R-squared:  0.1305 
#F-statistic: 2.101 on 6 and 38 DF,  p-value: 0.07583


# kde 95
skewness(hr$KDE_95_km2, na.rm = TRUE) 
shapiro.test(hr$KDE_95_km2)

## sqrt gives best value here (0.07)
hr$sqrt_KDE_95 <- sqrt(hr$KDE_95_km2)
skewness(hr$sqrt_KDE_95, na.rm = TRUE)   
shapiro.test(hr$sqrt_KDE_95)

mod_kde95 <- lm(sqrt_KDE_95 ~ region * species + tag_type, data = hr)
summary(mod_kde95)
plot(mod_kde95)
#Call:
#lm(formula = sqrt_KDE_95 ~ region * species, data = hr)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-10.1804  -3.3914  -0.0168   3.9804   7.2956 
#
#Coefficients: (3 not defined because of singularities)
#                                                Estimate Std. Error t value Pr(>|t|)   
#(Intercept)                                       5.857      1.981   2.957  0.00532 **
#  regionbamfield                                   -2.686      3.617  -0.743  0.46216   
#regionhaida gwaii                                 4.846      4.202   1.153  0.25599   
#regionnanaimo-central gulf islands                5.267      3.494   1.508  0.13993   
#regionnorthern gulf islands                       8.582      2.534   3.387  0.00166 **
#  speciesSURF                                       3.048      2.641   1.154  0.25563   
#regionbamfield:speciesSURF                           NA         NA      NA       NA   
#regionhaida gwaii:speciesSURF                        NA         NA      NA       NA   
#regionnanaimo-central gulf islands:speciesSURF       NA         NA      NA       NA   
#regionnorthern gulf islands:speciesSURF          -3.457      3.496  -0.989  0.32892   
#
#Residual standard error: 5.241 on 38 degrees of freedom
#Multiple R-squared:  0.3877,	Adjusted R-squared:  0.291 
#F-statistic: 4.009 on 6 and 38 DF,  p-value: 0.003312


# kde 50
skewness(hr$KDE_50_km2, na.rm = TRUE)

# sqrt 
hr$sqrt_KDE_50 <- sqrt(hr$KDE_50_km2)
skewness(hr$sqrt_KDE_50, na.rm = TRUE)  
shapiro.test(hr$sqrt_KDE_50)

mod_kde50 <- lm(sqrt_KDE_50 ~ region * species, data = hr)
summary(mod_kde50)
plot(mod_kde50)
#Call:
#lm(formula = sqrt_KDE_50 ~ region * species, data = hr)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4.9894 -1.8184 -0.0469  1.9585  5.0047 
#
#Coefficients: (3 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)                                      2.6326     0.9720   2.709  0.01008 * 
#  regionbamfield                                  -1.3970     1.7746  -0.787  0.43603   
#regionhaida gwaii                                2.0746     2.0619   1.006  0.32071   
#regionnanaimo-central gulf islands               3.1146     1.7144   1.817  0.07715 . 
#regionnorthern gulf islands                      4.3389     1.2434   3.490  0.00124 **
#  speciesSURF                                      0.8185     1.2960   0.632  0.53143   
#regionbamfield:speciesSURF                           NA         NA      NA       NA   
#regionhaida gwaii:speciesSURF                        NA         NA      NA       NA   
#regionnanaimo-central gulf islands:speciesSURF       NA         NA      NA       NA   
#regionnorthern gulf islands:speciesSURF         -1.3497     1.7152  -0.787  0.43621   
#
#Residual standard error: 2.572 on 38 degrees of freedom
#Multiple R-squared:  0.4095,	Adjusted R-squared:  0.3163 
#F-statistic: 4.393 on 6 and 38 DF,  p-value: 0.001826

