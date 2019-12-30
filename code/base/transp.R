library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(lubridate)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Setup the Universe ------------------------------------------------------
univ <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id)
cbsa <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  select(cbsa_fips,geometry) %>% 
  inner_join(.,univ) %>% 
  arrange(cbsa_fips) %>% 
  st_transform(crs = 2163) %>% 
  st_centroid_xy()

### SOURCE: https://www.bts.gov/browse-statistical-products-and-data
freight <- read_csv("data/base/source/freight.csv") %>% rename_all(tolower) %>% 
  select(name,mode_type,longitude,latitude) %>% 
  fastDummies::dummy_cols(., select_columns = "mode_type") %>%
  rename_all(~stringr::str_replace_all(., "mode_type_", "")) %>% 
  janitor::clean_names() %>% 
  st_as_sf(.,coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 2163) %>% 
  st_intersection(cbsa,.) %>% 
  select(-(name:mode_type)) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_fips) %>% 
  summarize(intermodal_freight = n(),
            n_rail_truck = sum(rail_truck),
            n_truck_port_rail = sum(truck_port_rail),
            n_port_truck = sum(port_truck),
            n_truck_port_rail_air = sum(truck_port_rail_air),
            n_rail_port = sum(rail_port),
            n_truck_port_air = sum(truck_port_air),
            n_air_truck = sum(air_truck),
            n_truck_truck = sum(truck_truck),
            n_truck_air_rail = sum(truck_air_rail)) %>% 
  left_join(univ,.) %>% 
  replace_na(list(intermodal_freight = 0,
                  n_rail_truck = 0,
                  n_truck_port_rail = 0,
                  n_port_truck = 0,
                  n_truck_port_rail_air = 0,
                  n_rail_port = 0,
                  n_truck_port_air = 0,
                  n_air_truck = 0,
                  n_truck_truck = 0,
                  n_truck_air_rail = 0,)) %>% 
  select(cbsa_fips,name,intermodal_freight,everything()) %>% 
  write_csv("data/base/generated/freight.csv")


  


ipcd <- read_csv("data/base/source/ipcd.csv") %>% rename_all(tolower)
