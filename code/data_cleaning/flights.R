library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Setup the Universe ------------------------------------------------------
univ <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id)
cbsa <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  select(cbsa_fips,geometry) %>% 
  inner_join(.,univ) %>% 
  arrange(cbsa_fips) %>% 
  st_centroid_xy()

# Read in Flights Data ----------------------------------------------------
df <- read_csv("data/base/source/enplane.csv") %>% janitor::clean_names() %>% 
  rename(planes = cy_05_enplanements) %>% 
  mutate(lat = as.numeric(str_replace(lat,"N",""))/3600,
         lon = as.numeric(str_replace(lon,"W",""))/-3600) %>% 
  filter(!is.na(lat)) %>% 
  select(airport_name,planes,lat,lon)

air_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = 2163) %>% 
  st_centroid_xy() 

flights <- st_intersection(air_sf,cbsa) %>% 
  select(airport_name,cbsa_fips,planes) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_fips) %>% 
  summarise(enplanements = sum(planes)) %>% 
  write_csv("data/base/generated/flights.csv")


# Map it ------------------------------------------------------------------

cbsa48 <- cbsa %>% filter(!cbsa_fips %in% c(11260,21820,46520)) %>% 
  left_join(.,flights) %>% 
  st_centroid_xy()
us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK","PR","HI")) %>% st_transform(crs = 2163)


ggplot() +
  geom_sf(data = us) +
  geom_point(data = cbsa48, aes(x,y,fill = enplanements),
             shape = 21, size = 2) + 
  viridis::scale_fill_viridis() + 
  theme_void()
