library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(crsuggest)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

indy <- places(state = "IN", cb = T) %>% 
  filter(NAME == "Indianapolis city (balance)") %>% 
  st_transform(2834)
columbus <- places(state = "OH", cb = T) %>% 
  filter(NAME == "Columbus") %>% 
  st_transform(2834)
cleveland <- places(state = "OH", cb = T) %>% 
  filter(NAME == "Cleveland") %>% 
  st_transform(2834)

ics <- school_districts(state = "IN") %>% 
  st_transform(2834)
ccs <- school_districts(state = "OH") %>% 
  st_transform(2834)
#crs_oh <- suggest_crs(ccs)

indy_school <- st_intersection(ics,indy) %>% 
  mutate(dist_m2 = st_area(.),
         dist_hectare = as.numeric(dist_m2)/10000) %>% 
  filter(dist_hectare >= 5) %>% 
  select(NAME,dist_hectare,geometry) %>% 
  st_collection_extract(type = "POLYGON")
cbus_school <- st_intersection(ccs,columbus) %>% 
  mutate(dist_m2 = st_area(.),
         dist_hectare = as.numeric(dist_m2)/10000) %>% 
  filter(dist_hectare >= 5) %>% 
  select(NAME,dist_hectare,geometry) %>% 
  st_collection_extract(type = "POLYGON")
clev_school <- st_intersection(ccs,cleveland) %>% 
  mutate(dist_m2 = st_area(.),
         dist_hectare = as.numeric(dist_m2)/10000) %>% 
  filter(dist_hectare >= 5,
         NAME != "School District Not Defined") %>% 
  select(NAME,dist_hectare,geometry) %>% 
  st_collection_extract(type = "POLYGON")

ggplot() +
  geom_sf(data = cbus_school, color = "black", aes(fill = as.factor(NAME)), alpha = .5) +
  geom_sf(data = columbus, size = .2, alpha = 0, color = "black") +
  labs(fill = "School District") +
  theme_void() +
  #guides(fill=FALSE) + 
  ggsave("plot/cbus_school.png", height = 12, width = 15)

ggplot() +
  geom_sf(data = clev_school, color = "black", aes(fill = as.factor(NAME)), alpha = .5) +
  geom_sf(data = cleveland, size = .2, alpha = 0, color = "black") +
  labs(fill = "School District") +
  theme_void() +
  #guides(fill=FALSE) + 
  ggsave("plot/clev_school.png", height = 12, width = 15)

ggplot() +
  geom_sf(data = indy_school, color = NA, aes(fill = as.factor(NAME)), alpha = .5) +
  geom_sf(data = indy, size = .2, alpha = 0, color = "black") +
  theme_void() +
  labs(fill = "School District") +
  ggsave("plot/indy_school.png", height = 12, width = 15)
