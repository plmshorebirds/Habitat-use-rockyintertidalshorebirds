# Kernel density estimations for home range 
# Summer 2025
# Paige Monteiro

library(ctmm)
library(sf)
library(raster)

# Movebank data 
shorebirds <- read.csv ("/Users/paigemonteiro/Documents/Habitat Use/Home Range/2025_movebank/Rocky Intertidal Shorebirds 2025.csv")

shorebirds <- shorebirds %>% filter(individual.local.identifier == "268694")

# convert to telemetry object
shorebird <- as.telemetry(shorebirds,timeformat="auto",timezone="UTC",projection=NULL,datum="WGS84",
                          dt.hot=NA,timeout=Inf,na.rm="row",mark.rm=FALSE,keep=FALSE,drop=TRUE)

GUESS <- ctmm.guess(shorebird,interactive=FALSE)
FIT <- ctmm.fit(shorebird,GUESS)

# 95%

UD <- akde(shorebird,FIT, weights = TRUE)

plot(shorebird,UD=UD)

plot(shorebird,UD=UD,col.bg="white",col="red",lwd=1,pch=1,type='p',
     error=TRUE,transparency.error=0.25,DF="CDF",col.DF="blue",
     col.grid="white",convex=FALSE,level=0.95,level.UD=0.95,col.level="black",
     lwd.level=1,border.SP=TRUE,col.R="green",
     fraction=1,units=TRUE,add=FALSE)

summary(UD, level.UD=0.50)
summary(UD, level.UD=0.95)

UD_sf <- as.sf(UD, level.UD = 0.95)

st_write(UD_sf, "/Users/paigemonteiro/Documents/Habitat Use/Home Range/2025_homeranges/268694_95_wakde.shp", delete_layer = TRUE)

# 50% 

plot(shorebird,UD=UD,col.bg="white",col="red",lwd=1,pch=1,type='p',
     error=TRUE,transparency.error=0.25,DF="CDF",col.DF="blue",
     col.grid="white",convex=FALSE,level=0.95,level.UD=0.50,col.level="black",
     lwd.level=1,border.SP=TRUE,col.R="green",
     fraction=1,units=TRUE,add=FALSE)

UD_sf <- as.sf(UD, level.UD = 0.50)

st_write(UD_sf, "/Users/paigemonteiro/Documents/Habitat Use/Home Range/2025_homeranges/268694_50_wakde.shp", delete_layer = TRUE)




