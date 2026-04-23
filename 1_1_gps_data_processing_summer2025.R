## 2025 gps data processing 
# Paige Monteiro

install.packages("santoku")
install.packages("zoo")
install.packages("Rtools44")
install.packages("fuzzyjoin")
install.packages("suncalc")

library(suncalc)
library(zoo)
library(fuzzyjoin)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(lubridate)
library(santoku)
library(sf)
library(stringr)
library(terra)

gps25 <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/gps_cleaned_withbandingdata_2025_13jun25.csv')

gps25$Year <- year(gps25$timestamp_utc)
gps25$Month <- month(gps25$timestamp_utc)
gps25$Day <- day(gps25$timestamp_utc)
gps25$Hour <- hour(gps25$timestamp_utc)
gps25$Minute <- minute(gps25$timestamp_utc)
gps25$Second <- second(gps25$timestamp_utc)
gps25 <- gps25 %>% dplyr::select(-Date, -Time)

## save and manually add tide stations data in arc
write.csv(gps25, "/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/gps_cleaned_withbandingdata_2025_13jun25_v2.csv")

alltides <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/Tide_Data/2025_tide_data.csv")
alltides %>% ggplot(aes(x = tide_station, y = height_ft))+geom_boxplot()
head(alltides)
# create timestamp_utc column:
alltides <- alltides %>% mutate(timestamp_utc = ymd_hm(paste(date,time), tz= "UTC"))
alltides <- alltides %>% rename(tide_height_ft = height_ft)
# add the tideclass column to the alltides data frame - code adapted from Ware 2020
alltides <- alltides %>%
  group_by(tide_station, week = week(timestamp_utc)) %>%
  mutate(tideclass = {
    qtls <- quantile(tide_height_ft, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    if (length(unique(qtls)) == length(qtls)) {
      cut(tide_height_ft, 
          breaks = qtls, 
          labels = c("very low", "low", "high", "very high"), 
          include.lowest = TRUE) } else {NA_character_}  
  }) %>% ungroup()

## falling vs rising tide
find_switch_point <- function(x) {
  ifelse(x != lag(x, default = first(x)), 1, 0)}

alltides <- alltides %>%
  group_by(tide_station) %>%
  mutate(tidechange = tide_height_ft - lag(tide_height_ft), 
         direction = ifelse(tidechange > 0, "rising", "falling")) %>%
  fill(direction, .direction = "down") %>% 
  mutate(switch_points = find_switch_point(direction),
         cycle_id = cumsum(switch_points)) %>%
  group_by(tide_station, cycle_id) %>%
  mutate(high_or_low = row_number() == n()) %>%
  ungroup()

View(alltides)
write.csv(alltides, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/Tide_Data/2025_tide_data_categorized_16jun25.csv")

# gps data 
bird <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/bird_2025_data_withtidestations_16jun25.csv")
tides <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/Tide_Data/2025_tide_data_categorized_16jun25.csv")

bird$Second <- 1
bird <- bird %>%
  mutate(Second = 1,
    timestamp_utc = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
                                       Year, Month, Day, Hour, Minute, Second),
                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
if (all(!grepl(":", tides$timestamp_utc))) {
  tides$timestamp_utc <- paste0(tides$timestamp_utc, ":00")}
tides$timestamp_utc <- as.POSIXct(tides$timestamp_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

bird <- bird %>%
  mutate(tide_station = ifelse(tide_station == "port mellon or gibsons", "gibsons", tide_station))

find_closest_tide <- function(ts, station, tides_df) {
  subset <- tides_df[tides_df$tide_station == station, ]
  if (nrow(subset) == 0 || is.na(ts)) return(NA)
  idx <- which.min(abs(difftime(subset$timestamp_utc, ts, units = "secs")))
  return(subset[idx, ])}

closest_tides <- bird %>%
  rowwise() %>%
  mutate(tide_row = list(find_closest_tide(timestamp_utc, tide_station, tides))) %>%
  unnest_wider(tide_row, names_sep = "_tide") %>%
  ungroup()

# clean
colnames(closest_tides)
closest_tides <- closest_tides %>% select(-tide_row_tideX, -tide_row_tidetide_station, 
                                          -tide_row_tidedate, -tide_row_tidetime, 
                                          -tide_row_tidetimezone, -tide_row_tideweek, 
                                          -tide_row_tidecycle_id, -tide_row_tidehigh_or_low)
data <- closest_tides %>% rename(tide_height_ft = tide_row_tidetide_height_ft,
                                 tide_time_utc = tide_row_tidetimestamp_utc,
                                 tide_class = tide_row_tidetideclass,
                                 tide_change = tide_row_tidetidechange,
                                 tide_direction = tide_row_tidedirection)
data <- data %>% select(-tide_row_tideswitch_points)

write.csv(data, "/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/bird_2025_data_withtidestations_16jun25_v2.csv")

# ********
## adding day or night column
data <- read.csv("/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/bird_2025_data_withtidestations_16jun25_v2.csv")
colnames(data)

get_sun_position_day_night <- function(timestamp_utc, latitude, longitude) {
  sun_pos <- getSunlightPosition(
    date = as.POSIXct(timestamp_utc, tz = "UTC"), 
    lat = latitude, 
    lon = longitude)
  day_or_night <- ifelse(sun_pos$altitude > 0, "day", "night")
  return(data.frame(altitude = sun_pos$altitude, azimuth = sun_pos$azimuth, day_or_night = day_or_night))}

data2 <- data %>%
  rowwise() %>%
  mutate(sun_info = list(get_sun_position_day_night(timestamp_utc, latitude, longitude))) %>%
  unnest(cols = c(sun_info))

write.csv(data2, "/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/bird_2025_data_withtidestations_16jun25_v3.csv")

# *******
# manually combined 2024 and 2025 data 
d24 <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/pinpoint_milsar_nonbreeding_2024_11jun25_v3.csv')

d24 <- d24 %>%
  mutate(timestamp_utc_parsed = ymd_hm(timestamp_utc),
    Year = year(timestamp_utc_parsed),
    Month = month(timestamp_utc_parsed),
    Day = day(timestamp_utc_parsed),
    Hour = hour(timestamp_utc_parsed),
    Minute = minute(timestamp_utc_parsed),
    Second = 0,
    Milisecond = 0,
    date_captured_clean = gsub("T", " ", date_captured),
    date_captured_clean = gsub("Z", "", date_captured_clean),
    date_captured_parsed = ymd_hms(date_captured_clean),
    date_captured = as.Date(date_captured_parsed),
    time_capture = format(date_captured_parsed, "%H:%M")) %>%
  select(-timestamp_utc_parsed, -date_captured_parsed, -date_captured_clean)

tag_band_lookup <- data.frame(
  tag_ID = c(234547, 234552, 234551, 234550, 234545, 234548, 234546, 234549, 241516, 241520, 244889, 225001, 225002, 225003, 225004, 225005,
             225007, 225009, 225006, 225008, 225010, 225011, 225012, 239538,
             241517, 244888, 241518, 239544, 244887, 244886, 239545, 239542,
             239539, 239546),
  band_id_fill = c("1873-15500", "1873-15418", "1873-15414", "1873-15411", "1873-15405", "1873-15404", "1462-15002", "1462-15001", 
                   "1462-15011", "1462-03202", "1462-15010", "1462-03207", "1873-15502", "1873-15503", "1873-15504",
                   "1462-15012", "1462-15013", "1462-15014", "1873-15423", "1873-15422", "1873-15424", "1873-15425",
                   "1873-15426", "1873-15506", "1873-15507", "1873-15508", "1873-15509", "1873-15510", "1873-15511",
                   "1873-15512", "1873-15513", "1873-15514", "1873-15515", "1873-15516"))

d24 <- d24 %>%
  left_join(tag_band_lookup, by = "tag_ID") %>%
  mutate(band_id = ifelse(is.na(band_id) | band_id == "", band_id_fill, band_id)) %>%
  select(-band_id_fill)

unique(d24$tag_ID[!d24$tag_ID %in% tag_band_lookup$tag_ID])
write.csv(d24, '/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/pinpoint_milsar_nonbreeding_2024_11jun25_v4.csv')

# ******
kde <- st_read("/Users/paigemonteiro/Documents/ArcGIS/Habitat_covariates_4Dec24.gdb",
        layer = "kdes_95_Merge_17jun25")

kde$ID <- sub("^([0-9]{6}).*", "\\1", kde$Field)

st_write(kde, "/Users/paigemonteiro/Documents/ArcGIS/kde_polygons_95.gpkg", layer = "kde_95_all_18jun25", driver = "GPKG")
kde <- st_read("/Users/paigemonteiro/Documents/ArcGIS/kde_polygons_95.gpkg", layer = "kde_95_all_18jun25")

kde_dissolved <- kde %>%
  group_by(ID) %>%
  summarise(geom = st_union(geom), .groups = "drop")

st_write(kde_dissolved, "/Users/paigemonteiro/Documents/ArcGIS/kde_polygons_95.gpkg", layer = "kde_95_onepolyperID_18jun25", driver = "GPKG")
# *******

bird <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Final Data Sets/bird_data_nonbreed_18jun25_v2.csv')
bird_sf <- st_as_sf(bird, coords = c("longitude", "latitude"), crs = 4326)

bird <- st_read("/Users/paigemonteiro/Documents/Habitat Use//ArcPro/rockyshbird_20Jun24_v2/points_2025_edited_v2.gpkg",
                layer="bird_data_within250mshore_18jun25")

shoreline <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg", 
                     layer = "ShoreUnitClassification_updated_18jun25")

shoreline <- st_zm(shoreline, drop = TRUE, what = "ZM")
shoreline <- st_cast(shoreline, to = "MULTILINESTRING")
shore_unitlines <- st_transform(shore_unitlines, crs = 3005)

kde <- st_read("/Users/paigemonteiro/Documents/ArcGIS/kde_polygons_95.gpkg", 
               layer = "kde_95_onepolyperID_18jun25")

kde <- st_transform(kde, crs = 3005)

combined_shoreline <- st_union(shoreline)
combined_shoreline <- st_line_merge(combined_shoreline)
combined_shoreline_2d <- st_zm(combined_shoreline, drop = TRUE, what = "ZM")

combined_shoreline_sf <- st_sf(geometry = combined_shoreline_2d)

shorelines_within_polygons <- st_intersection(combined_shoreline_sf, kde)

shorelines_with_id <- shorelines_within_polygons %>%
  dplyr::select(ID, geometry)

shorelines_with_id_2d <- st_zm(shorelines_with_id, drop = TRUE, what = "ZM")

output_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/shoreline snippets"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
for (i in 1:nrow(shorelines_with_id_2d)) {
  line_i <- shorelines_with_id_2d[i, ]
  line_id <- line_i$ID
  file_name <- paste0("line_ID_", line_id, ".shp")
  file_path <- file.path(output_dir, file_name)
  st_write(line_i, file_path)}

input_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/shoreline snippets"
output_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/randompoints_with250m_snippets_18jun25"
layer_info <- st_layers(input_dir)$name
for (layer_name in layer_info) {
  message("Processing ", layer_name)
  line_sf <- st_read(input_dir, layer = layer_name, quiet = TRUE)
  line_id <- str_extract(layer_name, "\\d{6}$") 
  buffer <- st_buffer(line_sf, dist = 250) 
  set.seed(42) 
  points <- st_sample(buffer, size = 10000, type = "random")
  points_sf <- st_sf(
    id = line_id,
    geometry = points,
    crs = st_crs(line_sf))
  output_path <- file.path(output_dir, paste0("random_points_ID_", line_id, ".shp"))
  st_write(points_sf, output_path, delete_dsn = TRUE, quiet = TRUE)}

line_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/shoreline snippets"
point_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/randompoints_with250m_snippets_18jun25"
output_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/randompoints_snappedto_snippets_18jun25"
point_files <- list.files(point_dir, pattern = "^random_points_ID_\\d{6}\\.shp$", full.names = TRUE)

for (point_file in point_files) {
  id <- str_extract(point_file, "\\d{6}")
  message("Snapping points for ID: ", id)
  points <- st_read(point_file, quiet = TRUE)
  line <- st_read(line_dir, layer = paste0("line_ID_", id), quiet = TRUE)
  snapped_geom <- st_nearest_points(points, line) %>%
    st_cast("POINT") %>%
    .[seq(2, length(.), by = 2)]  
  snapped_points <- st_sf(id = id, geometry = snapped_geom, crs = st_crs(points))
  output_path <- file.path(output_dir, paste0("snapped_points_ID_", id, ".shp"))
  st_write(snapped_points, output_path, delete_dsn = TRUE, quiet = TRUE)}

snapped_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/randompoints_snappedto_snippets_18jun25"
snapped_files <- list.files(snapped_dir, pattern = "^snapped_points_ID_\\d{6}\\.shp$", full.names = TRUE)

all_snapped_points <- do.call(rbind, lapply(snapped_files, function(file) {
  st_read(file, quiet = TRUE)}))

st_write(all_snapped_points, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/random_points_snapped_combined_18jun25.shp")

#-------
bird <- st_read("/Users/paigemonteiro/Documents/Habitat Use//ArcPro/rockyshbird_20Jun24_v2/points_2025_edited_v2.gpkg",
                layer="bird_data_within250mshore_18jun25")

bird <- bird %>%
  filter(!tag_ID %in% c("225008", "225019", "234545"))

bird <- st_transform(bird, crs = 3005)

shoreline_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/shoreline snippets"
output_dir <- "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/birdpoints_snappedto_snippets_18jun25"
dir.create(output_dir, showWarnings = FALSE)
tag_ids <- unique(bird$tag_ID)

for (id in tag_ids) {
  message("Snapping bird points for tag_ID: ", id)
  points <- bird %>% filter(tag_ID == id)
  shoreline_layer <- paste0("line_ID_", id)
  line_path <- file.path(shoreline_dir, paste0(shoreline_layer, ".shp")) 
  if (!file.exists(line_path)) {
    warning("Shoreline file not found for tag_ID ", id, ": ", line_path)
    next}
  line <- st_read(line_path, quiet = TRUE)
  snapped_geom <- st_nearest_points(points, line) %>%
    st_cast("POINT") %>%
    .[seq(2, length(.), by = 2)]  
  snapped_points <- st_sf(points %>% st_drop_geometry(), geometry = snapped_geom, crs = st_crs(points))
  output_path <- file.path(output_dir, paste0("snapped_birdpoints_ID_", id, ".shp"))
  st_write(snapped_points, output_path, delete_dsn = TRUE, quiet = TRUE)}

snapped_files <- list.files(output_dir, pattern = "^snapped_birdpoints_ID_\\d{6}\\.shp$", full.names = TRUE)
all_snapped_bird_points <- do.call(rbind, lapply(snapped_files, function(file) {
  st_read(file, quiet = TRUE)}))

st_write(all_snapped_bird_points, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/snapped_birdpoints_combined_19jun25.shp")

####

bird <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/snapped_birdpoints_combined_19jun25.shp")
bird <- bird %>% rename(timestamp_utc = tmstmp_)
range(bird$timestamp_utc, na.rm = TRUE)

bird <- st_transform(bird, crs = 4326)

coords <- st_coordinates(bird)
bird <- bird %>%
  mutate(longitude = coords[, 1],
         latitude = coords[, 2])

st_write(bird, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "snapped_birdpoints_combined_19jun25")

# -----
random10k <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/random_points_snapped_combined_18jun25.shp")
bird <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "snapped_birdpoints_combined_19jun25")

random10k <- st_transform(random10k, crs = 4326) 
bird <- st_transform(bird, crs = 4326) 
bird$type <- "1"
random10k$type <- "2"
coords <- st_coordinates(random10k)
random10k <- random10k %>%
  mutate(longitude = coords[, 1],
         latitude = coords[, 2])

random10k <- random10k %>%
  mutate(tag_ID = as.numeric(id)) %>%
  select(latitude, longitude, type, tag_ID, geometry)
names(bird)[which(names(bird) == attr(bird, "sf_column"))] <- "geometry"
attr(bird, "sf_column") <- "geometry"
attr(random10k, "sf_column") <- "geometry"

missing_cols <- setdiff(names(bird), names(random10k))
random10k[missing_cols] <- NA  

combined_points <- bind_rows(bird, random10k) %>%
  st_as_sf()

st_write(combined_points, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_19jun25")

##### *****
points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                  layer = "unfilteredbird_random10k_19jun25")

points$unique_id <- seq_len(nrow(points))

# VRI
vri <- st_read("/Users/paigemonteiro/Documents/ArcGIS/Habitat_covariates_final/habitat_covariates_31Jan25.gpkg", layer = "VRI_polygons_2Jan25")
vri <- st_transform(vri, crs = 4326)
points <- st_join(points, vri %>% dplyr::select(SHRUB_HEIGHT, SHRUB_CROWN_CLOSURE, LAND_COVER_CLASS_CD_1, 
                                                EST_COVERAGE_PCT_1, CROWN_CLOSURE, CROWN_CLOSURE_CLASS_CD, 
                                                PROJ_HEIGHT_1, PROJ_HEIGHT_CLASS_CD_1))

# aspect
library(terra)
aspect <- rast("/Users/paigemonteiro/Documents/ArcGIS/aspect_dem_all.tif")
aspect_values <- extract(aspect, points)
points$aspect_value <- aspect_values[,2]

points <- points %>%
  mutate(aspect_category = case_when(
    aspect_value == -1 ~ "flat",
    aspect_value >= 0 & aspect_value < 22.5 ~ "north",
    aspect_value >= 22.5 & aspect_value < 67.5 ~ "northeast",
    aspect_value >= 67.5 & aspect_value < 112.5 ~ "east",
    aspect_value >= 112.5 & aspect_value < 157.5 ~ "southeast",
    aspect_value >= 157.5 & aspect_value < 202.5 ~ "south",
    aspect_value >= 202.5 & aspect_value < 247.5 ~ "southwest",
    aspect_value >= 247.5 & aspect_value < 292.5 ~ "west",
    aspect_value >= 292.5 & aspect_value < 337.5 ~ "northwest",
    aspect_value >= 337.5 & aspect_value <= 360 ~ "north",
    TRUE ~ NA_character_))

points <- points %>%
  mutate(aspect_rank = case_when(
    aspect_category == "north" ~ 0,
    aspect_category == "northeast" ~ 45,
    aspect_category == "northwest" ~ 45,
    aspect_category == "east" ~ 90,
    aspect_category == "west" ~ 90,
    aspect_category == "southeast" ~ 135,
    aspect_category == "southwest" ~ 135,
    aspect_category == "south" ~ 180,
    TRUE ~ NA_real_))

st_write(points, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_20jun25")

# islets and islands:
isl <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg",
               layer="shoreunitlines_isletsandislands_19jun25")

isl$hec <- as.numeric(st_area(isl)) / 10000

shoreline <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg", 
                     layer = "ShoreUnitClassification_updated_18jun25")

shoreline <- st_transform(shoreline, st_crs(isl))
overlap_index <- st_intersects(shoreline, isl, sparse = FALSE)
shoreline_mainland <- shoreline[!apply(overlap_index, 1, any), ]

isl_centroids <- st_centroid(isl)
isl_centroids <- st_transform(isl_centroids, st_crs(shoreline_mainland))
shoreline_mainland <- st_transform(shoreline_mainland, st_crs(isl_centroids))
nearest_distances <- st_distance(isl_centroids, shoreline_mainland)

min_dists <- apply(nearest_distances, 1, min)
isl$dist_to_mainland <- min_dists 

isl <- isl %>%
  group_by(structure) %>%
  mutate(structure_number = row_number()) %>%
  ungroup()

points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                  layer = "unfilteredbird_random10k_20jun25")

isl <- st_transform(isl, st_crs(points))
points_with_isl <- st_join(points, isl, left = TRUE)
points_with_isl$structure[is.na(points_with_isl$structure)] <- "mainland"
points_with_isl$dist_to_mainland[is.na(points_with_isl$dist_to_mainland)] <- 0

st_write(points_with_isl, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_20jun25_v2")

# ********

points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                   layer = "unfilteredbird_random10k_20jun25_v2")

points <- st_transform(points, crs = 3005) 
# intertidal area
gdb_path <- "/Users/paigemonteiro/Documents/ArcGIS/Working_Nov2024.gdb"
feature_class <- "intertidal_apr2025"
int <- st_read(dsn = gdb_path, layer = feature_class)
int <- st_make_valid(int)
int <- st_transform(int, st_crs(points_buffered))

points_buffered <- points_buffered %>%
  mutate(buffer_area = as.numeric(st_area(.)))

bbox <- st_bbox(points_buffered) %>% st_as_sfc() %>% st_buffer(1000)

int_small <- st_filter(int, bbox)

intersections <- st_intersection(points_buffered %>% dplyr::select(unique_id, buffer_area), int_small)

intertidal_summary <- intersections %>%
  mutate(overlap_area = as.numeric(st_area(.))) %>%
  group_by(unique_id, buffer_area) %>%
  summarise(
    total_overlap_area = sum(overlap_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percent_intertidal = (total_overlap_area / buffer_area) * 100)

points2 <- left_join(points, st_drop_geometry(intertidal_summary) %>% 
                      dplyr::select(unique_id, percent_intertidal), by = "unique_id")

st_write(points2, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_20jun25_v3")


# *******

points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                  layer = "unfilteredbird_random10k_20jun25_v3")

# CEF human disturbance 
CEF <- st_read("/Users/paigemonteiro/Documents/ArcGIS/Habitat_covariates_final/habitat_covariates_31Jan25.gpkg", 
               layer = "BC_CEF_Clipped")
st_geometry_type(CEF) %>% table()
CEF <- st_cast(CEF, "MULTIPOLYGON")
invalid_geometries <- CEF[!st_is_valid(CEF), ]
CEF <- st_make_valid(CEF) 
CEF_polygons <- CEF %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

points <- st_join(points, CEF_polygons %>%
                    dplyr::select(CEF_DISTURB_GROUP, CEF_DISTURB_SUB_GROUP, CEF_DISTURB_SUB_GROUP_RANK, CEF_HUMAN_DISTURB_FLAG))

points <- points %>%
  mutate(CEF_DISTURB_RANK = case_when(
    CEF_DISTURB_GROUP == "Agriculture_and_Clearing" ~ 10,
    CEF_DISTURB_GROUP == "BTM - Alpine SubAlpine Barren" ~ 12,
    CEF_DISTURB_GROUP == "BTM - Forest Land" ~ 12,
    CEF_DISTURB_GROUP == "BTM - Salt Water" ~ 12,
    CEF_DISTURB_GROUP == "BTM - Wetlands Estuaries" ~ 12,
    CEF_DISTURB_GROUP == "BTM - Range Lands" ~ 12, 
    CEF_DISTURB_GROUP == "Cutblocks" ~ 9,
    CEF_DISTURB_GROUP == "Mining_and_Extraction" ~ 1,
    CEF_DISTURB_GROUP == "Power" ~ 4,
    CEF_DISTURB_GROUP == "Recreation" ~ 7,
    CEF_DISTURB_GROUP == "RESULTS_Reserves" ~ 11,
    CEF_DISTURB_GROUP == "ROW" ~ 5,
    CEF_DISTURB_GROUP == "Urban" ~ 6,
    CEF_DISTURB_GROUP == "OGC_Infrastructure" ~ 3,
    TRUE ~ NA_real_))

points <- points %>%
  mutate(CEF_DISTURB = case_when(
    CEF_DISTURB_RANK %in% c(10, 9, 1, 4, 7, 11, 5, 6, 3) ~ "y",
    CEF_DISTURB_RANK == 12 ~ "n",
    TRUE ~ NA_character_))
points <- points %>%
  distinct(unique_id, .keep_all = TRUE)

st_write(points, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_20jun25_v5")

# *****

points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                  layer = "unfilteredbird_random10k_20jun25_v5")

# shallow substrate
SOG_substrate <- rast("/Users/paigemonteiro/Documents/ArcGIS/SOG_substrate_20m.tif")
HG_substrate <- rast("/Users/paigemonteiro/Documents/ArcGIS/HG_substrate_20m.tif")

substrate_values <- extract(HG_substrate, points)
points$shallow_substrate <- substrate_values[,2]
substrate_values <- extract(SOG_substrate, points)
points$shallow_substrate2 <- substrate_values[,2]

points <- points %>%
  mutate(shallow_sub = coalesce(shallow_substrate, shallow_substrate2))
WCVI_substrate <- rast("/Users/paigemonteiro/Documents/ArcGIS/WCVI_substrate_20m.tif")
substrate_values <- extract(WCVI_substrate, points)
points$shallow_substrateW <- substrate_values[,2]

points <- points %>%
  mutate(shallow_substrate = coalesce(shallow_sub, shallow_substrateW))

points <- points %>%
  dplyr::select(-shallow_substrate2, -shallow_sub, -shallow_substrateW)
table(points$shallow_substrate, useNA = "ifany")

# *******
# streams
streams <- st_read("/Users/paigemonteiro/Documents/ArcGIS/BCGW_02001F02_1739145117262_26552/FWA_STREAM_NETWORKS_SP/FWSTRMNTWR_line.shp")
points <- st_transform(points, st_crs(streams))

nearest_idx <- st_nearest_feature(points, streams)

points$dist_to_stream <- st_distance(points, streams[nearest_idx, ], by_element = TRUE)

# herring spawn
# DataBC: Herring Spawn Sites - CRIMS_HERRING_SPAWN layer, downloaded from bc gov depository
spawn <- st_read("/Users/paigemonteiro/Documents/ArcGIS/Habitat_covariates_4Dec24.gdb",
                 layer="CRIMS_HERRING_SPAWN")
points <- st_transform(points, st_crs(spawn))
nearest_idx <- st_nearest_feature(points, spawn)
points$dist_to_spawn <- as.numeric(st_distance(points, spawn[nearest_idx, ], by_element = TRUE))

library(units)

points <- points %>%
  mutate(spawn_500m = ifelse(dist_to_spawn <= 500, "y", "n"))

points <- points %>%
  mutate(spawn_250m = ifelse(dist_to_spawn <= 250, "y", "n"))

points <- points %>%
  mutate(islet = case_when(
    structure == "islet" ~ "y",
    structure == "islet near shore" ~ "y",
    structure %in% c("mainland", "island") ~ "n",
    TRUE ~ NA_character_))

st_write(points, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_20jun25_v6")

# ******

points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                  layer = "unfilteredbird_random10k_20jun25_v6")
# shorezone
sz <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg", 
                     layer = "ShoreUnitClassification_updated_18jun25")
sz <- sz %>%
  dplyr::select(REP_TYPE, REP_TYPE_NAME, COASTAL_CLASS_NAME, COASTAL_CLASS,
                EXP_FINAL, MAX_DIRECTION, MAX_FETCH, LEFT45, PERPENDICULAR90, 
                RIGHT45, SHORENORMAL, EFT_FETCH, TIDE_LARGE, TIDE_MEAN, TIDE_STATION,
                SHORENAME_NAME)
sz <- st_transform(sz, st_crs(points))
points_with_sz <- st_join(points, sz, left = TRUE)
st_crs(points)
st_crs(sz)

points_with_sz <- points_with_sz %>%
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
points_with_sz <- points_with_sz %>%
  distinct(unique_id, .keep_all = TRUE)

st_write(points_with_sz, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_20jun25_v7")

write.csv(st_drop_geometry(points_with_sz), "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25_v7.csv", row.names = FALSE)

# manually did some cleaning up in excel - changing type 2 to type 0 and filling in banding info 
# fixed column names 
# deleted random points for 234545 because didnt use these bird points (there were only four)

points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25_v9.csv')
points <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)  

vri <- st_read("/Users/paigemonteiro/Documents/ArcGIS/Habitat_covariates_final/habitat_covariates_31Jan25.gpkg", layer = "VRI_polygons_2Jan25")

points <- st_transform(points, st_crs(vri))
points_buffered <- st_buffer(points, dist = 250)
vri <- st_make_valid(vri)
 
intersections <- st_intersection(points_buffered, vri)

intersections <- st_intersection(points_buffered %>% dplyr::select(unique_id), 
                                 vri %>% dplyr::select(CROWN_CLOSURE))
intersections <- intersections %>%
  mutate(
    overlap_area = as.numeric(st_area(.)),
    crown_cover = as.numeric(CROWN_CLOSURE),
    weighted_cover = crown_cover * overlap_area)

canopy_cover_summary <- intersections %>%
  group_by(unique_id) %>%
  summarise(
    total_overlap_area = sum(overlap_area, na.rm = TRUE),
    total_weighted_cover = sum(weighted_cover, na.rm = TRUE),
    canopy_cover = total_weighted_cover / total_overlap_area)

points2 <- left_join(points, st_drop_geometry(canopy_cover_summary), 
                     by = "unique_id")

st_write(points2, "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
         layer = "unfilteredbird_random10k_21jun25")

points <- st_read("/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25.gpkg",
                  layer = "unfilteredbird_random10k_21jun25")


write.csv(st_drop_geometry(points2), "/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_21jun25_fixed_v2.csv", row.names = FALSE)

pointsll <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_19jun25_v9.csv')

points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_21jun25_fixed_v2.csv')

points <- points %>%
  left_join(pointsll %>% select(unique_id, latitude, longitude), by = "unique_id")

write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_22jun25.csv')
points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/Habitat variables/points_22jun25.csv')

# ******
write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_21jun25_v4.csv')

points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_21jun25_v4.csv')


set.seed(123)

type1_counts <- points %>%
  filter(type == 1) %>%
  count(tag_ID, name = "n_type1")

points_reduced <- points %>%
  left_join(type1_counts, by = "tag_ID") %>%
  group_by(tag_ID, type) %>%
  filter(
    type == 1 | (type == 0)) %>%
  group_by(tag_ID, type) %>%
  mutate(row_id = row_number()) %>%  
  group_modify(~ {
    if (.y$type == 0) {
      n_keep <- unique(.x$n_type1)
      if (length(n_keep) == 0 || is.na(n_keep)) return(tibble())
      .x %>% slice_sample(n = n_keep)} else {.x}}) %>%
  ungroup() %>%
  select(-n_type1, -row_id) 

write.csv(points_reduced, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_equaltype0_21jun25_v4.csv')

#### 
points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_21jun25_v4.csv')
points$timestamp_utc <- parse_date_time(points$timestamp_utc,
                                        orders = c("ymd HMS", "ymd HM"),
                                        tz = "UTC")
points_1 <- points %>% filter(type == 1)

points_1 <- points_1 %>%
  mutate(date = as.Date(timestamp_utc))

daily_counts <- points_1 %>%
  group_by(tag_ID, date) %>%
  tally(name = "n_fixes") %>%
  filter(n_fixes > 1)
tag_ids_multiple_per_day <- unique(daily_counts$tag_ID)

points_filtered <- points_1 %>%
  filter(tag_ID %in% tag_ids_multiple_per_day, !is.na(timestamp_utc))

get_closest_point <- function(data, target_hour) {
  data %>%
    mutate(target_time = ymd_hm(paste(date, sprintf("%02d:00", target_hour))),
           time_diff = abs(difftime(timestamp_utc, target_time, units = "mins"))) %>%
    group_by(tag_ID, date) %>%
    slice_min(order_by = time_diff, n = 1, with_ties = FALSE) %>%
    ungroup()}

points_noon <- get_closest_point(points_filtered, 20)
points_midnight <- get_closest_point(points_filtered, 8)

points_daily_2 <- bind_rows(points_noon, points_midnight) %>%
  distinct(tag_ID, date, timestamp_utc, .keep_all = TRUE)

points_no_type1 <- points %>%
  filter(!(type == 1 & tag_ID %in% tag_ids_multiple_per_day)) %>%
  mutate(date = NA, time_diff = NA)

points_2aday <- bind_rows(points_no_type1, points_daily_2)

type1_counts <- points_2aday %>%
  filter(type == 1) %>%
  count(tag_ID, name = "n_type1")

set.seed(123)
points_balanced <- points_2aday %>%
  left_join(type1_counts, by = "tag_ID") %>%
  group_by(tag_ID, type) %>%
  mutate(row_id = row_number()) %>%
  group_modify(~ {
    if (.y$type == 0) {
      n_keep <- unique(.x$n_type1)
      if (length(n_keep) == 0 || is.na(n_keep)) return(tibble())
      .x %>% slice_sample(n = n_keep)} else {.x}}) %>%
  ungroup() %>%
  select(-n_type1, -row_id)

write.csv(points_balanced, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_8jul25.csv')


#### 
points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_21jun25_v4.csv')

library(lubridate)
points$timestamp_utc <- parse_date_time(points$timestamp_utc,
                                        orders = c("ymd HMS", "ymd HM"),
                                        tz = "UTC")
points_1 <- points %>% filter(type == 1)

points_1_day <- points_1 %>%
  filter(day_or_night == "day")

points_1_day <- points_1_day %>%
  mutate(date = as.Date(timestamp_utc))

daily_counts <- points_1_day %>%
  group_by(tag_ID, date) %>%
  tally(name = "n_fixes")

multi_fix_days <- daily_counts %>%
  filter(n_fixes > 1)

tag_ids_multiple_per_day <- unique(multi_fix_days$tag_ID)

points_filtered <- points_1_day %>%
  filter(tag_ID %in% c("225001", "225002", "225003", "225005", "225007", "225009", "225010", 
                       "225012", "225014", "225015", "225016", "225017", "225018", "225021", 
                       "234548", "234549", "234550", "234551", "234552", "239539", "239540", 
                       "241516", "241517", "241520", "244886", "244887", "267746", "267748", 
                       "268692", "268693", "268694")) %>%
  filter(!is.na(timestamp_utc)) %>%  
  mutate(date = as_date(timestamp_utc),
         time_diff = abs(difftime(timestamp_utc, ymd_hm(paste(date, "20:00")), units = "mins")))

points_daily_closest <- points_filtered %>%
  group_by(tag_ID, date) %>%
  slice_min(order_by = time_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

points_no <- points %>% 
  dplyr::filter(!(type == 1 & tag_ID %in% c("225001", "225002", "225003", "225005", "225007", "225009", "225010", 
                                 "225012", "225014", "225015", "225016", "225017", "225018", "225021", 
                                 "234548", "234549", "234550", "234551", "234552", "239539", "239540", 
                                 "241516", "241517", "241520", "244886", "244887", "267746", "267748", 
                                 "268692", "268693", "268694")))
points_no$date <- NA
points_no$time_diff <- NA

points_filtered1aday <- rbind(points_no, points_daily_closest)

#####
set.seed(123)

type1_counts <- points_filtered1aday %>%
  filter(type == 1) %>%
  count(tag_ID, name = "n_type1")

points_reduced <- points_filtered1aday %>%
  left_join(type1_counts, by = "tag_ID") %>%
  group_by(tag_ID, type) %>%
  filter(
    type == 1 | (type == 0)) %>%
  group_by(tag_ID, type) %>%
  mutate(row_id = row_number()) %>% 
  group_modify(~ {
    if (.y$type == 0) {
      n_keep <- unique(.x$n_type1)
      if (length(n_keep) == 0 || is.na(n_keep)) return(tibble())  
      .x %>% slice_sample(n = n_keep) } else {.x}}) %>%
  ungroup() %>%
  select(-n_type1, -row_id) 

write.csv(points_reduced, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird1aday_equaltype0_21jun25_v4.csv')


# adding in the stream within 100m:
library(sf)
streams <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg",
                   layer = "fwa_stream_100m_shoreline")

## do each point layer one at a time:

# bird pt 1 a day, equal 0
#points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird1aday_equaltype0_21jun25_v4.csv')

# all bird pts, equal 0
#points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_equaltype0_21jun25_v4.csv')

# all bird pts, all 0s
#points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_21jun25_v4.csv')

# bird 2 pts a day, equal 0
points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_8jul25.csv')

points <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)  # WGS84!

points <- st_transform(points, st_crs(streams))

nearest_idx <- st_nearest_feature(points, streams)

points$dist_to_fwa_stream <- st_distance(points, streams[nearest_idx, ], by_element = TRUE)
st_crs(points)

points$longitude <- st_coordinates(points)[, 1]
points$latitude <- st_coordinates(points)[, 2]
points <- st_drop_geometry(points)

#write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird1aday_equaltype0_9jul25_v3.csv')

# all bird pts, equal 0
#write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_equaltype0_9jul25_v3.csv')

# all bird pts, 10k 
#write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_9jul25_v3.csv')

# bird pts 2 a day, equal 0
write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_9jul25_v3.csv')



# for bird pts 2 a day - calculating dist_to_islet 

isl <- st_read("/Users/paigemonteiro/Documents/ArcGIS/shoreline_2025_v2.gpkg",
               layer="shoreunitlines_isletsandislands_19jun25")
isl2 <- isl %>% 
  dplyr::filter(structure == "islet")

p <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_9jul25_v3.csv')

p <- st_as_sf(p, coords = c("longitude", "latitude"), crs = 3005) 

p <- st_transform(p, st_crs(isl2))
nearest_idx <- st_nearest_feature(p, isl2)

p$dist_to_islet <- st_distance(p, isl2[nearest_idx, ], by_element = TRUE)

p$longitude <- st_coordinates(p)[, 1]
p$latitude <- st_coordinates(p)[, 2]
points <- st_drop_geometry(p)

write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_11jul25.csv')


# fixing islet column:
# run for each points layer:
# 2 a day:
#points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_11jul25.csv')
# 1 a day: 
#points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird1aday_equaltype0_9jul25_v3.csv')
# all 
#points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_equaltype0_9jul25_v3.csv')
# all random
points <- read.csv('/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_9jul25_v3.csv')

table(points$islet)

points <- points %>%
  mutate(islet = case_when(
    structure == "islet" ~ "y",
    structure == "islet near shore" ~ "n", # should be NO these are islets that are right next to mainland - eg rocks 
    structure %in% c("mainland", "island") ~ "n",
    TRUE ~ NA_character_))

table(points$islet)

#write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird2aday_equaltype0_11jul25_v2.csv')
#write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/bird1aday_equaltype0_9jul25_v4.csv')
#write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_equaltype0_9jul25_v4.csv')
write.csv(points, '/Users/paigemonteiro/Documents/Habitat Use/Analysis/final_versions_for_gam_21jun25/allbirdpoints_avail10k_9jul25_v3.csv')