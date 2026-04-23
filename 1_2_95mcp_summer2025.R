## 6 Jun 2025 
# Paige Monteiro
## Individal 95% MCPs

library(sf)
library(dplyr)
install.packages("adehabitatHR")
library(adehabitatHR)

allfixes <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Home Range/2025_movebank/Rocky Intertidal Shorebirds 2025_rerun_MCPs.csv")

# filter for individuals with at least 5 relocations
individual_counts <- allfixes %>%
  group_by(individual.local.identifier) %>%
  summarise(count = n())

sufficient_data <- individual_counts %>%
  filter(count >= 5) %>%
  pull(individual.local.identifier)

allfixes <- allfixes %>%
  filter(individual.local.identifier %in% sufficient_data)

# convert to sf object with WGS84 
allfixes_sf <- st_as_sf(allfixes, coords = c("location.long", "location.lat"), crs = 4326)

# transform to BC Albers
#allfixes_sf <- st_transform(allfixes_sf, crs = 3005)

allfixes_sf$individual.local.identifier <- as.character(allfixes_sf$individual.local.identifier)


mcp_list <- list()

individuals <- unique(allfixes_sf$individual.local.identifier)

for (ind in individuals) {
  ind_data <- allfixes_sf %>% filter(individual.local.identifier == ind)
  sp_ind <- as(ind_data, "Spatial")
  sp_ind@data$id <- ind 
  mcp_result <- mcp(sp_ind, percent = 95)
  mcp_sf <- st_as_sf(as(mcp_result, "SpatialPolygonsDataFrame"))
  st_crs(mcp_sf) <- 4326 
  mcp_sf <- mcp_sf %>%
    mutate(id = ind,
           area_m2 = st_area(.),
           area_km2 = as.numeric(area_m2) / 1e6)
  mcp_list[[ind]] <- mcp_sf}

mcp_sf <- do.call(rbind, mcp_list)

mcp_export <- mcp_sf %>% dplyr::select(-area_m2)

st_write(mcp_export,
         "/Users/paigemonteiro/Documents/Habitat Use/Home Range/mcp_95_nonbreeding_2025_plus2_2024_12jun25.shp",
         delete_layer = TRUE)

write.csv(st_drop_geometry(mcp_export), "/Users/paigemonteiro/Documents/Habitat Use/Home Range/mcp_95_nonbreeding_2025_plus2_2024_12jun25.csv", row.names = FALSE)

