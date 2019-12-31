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
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  select(cbsa_fips,geometry) %>% 
  inner_join(.,univ) %>% 
  arrange(cbsa_fips) %>% 
  st_centroid_xy()

### SOURCE: https://sedac.ciesin.columbia.edu/data/set/superfund-epa-national-priorities-list-ciesin-mod-v2
df <- read_csv("data/base/source/epa.csv") %>% 
  select(SITE_NAME,NPL_STATUS,NPL_STATUS_DATE,LONGITUDE,LATITUDE) %>% 
  mutate(date_of = mdy(NPL_STATUS_DATE),
         count = 1) %>% 
  filter(date_of < '2006-01-01') %>% 
  rename_all(tolower)

epa_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 2163) %>% 
  st_centroid_xy() 

area <- read_csv("data/base/generated/density_2005.csv") %>% select(1:3)


superfunds <- st_intersection((epa_sf %>% select(site_name,date_of,count)),
                              (cbsa %>% select(cbsa_fips,name))) %>% 
  select(date_of:name) %>% 
  st_drop_geometry() %>%
  group_by(cbsa_fips) %>% 
  summarise(npl_sites = sum(count)) %>%
  left_join(univ,.) %>% 
  replace_na(list(npl_sites = 0)) %>% 
  left_join(.,area) %>% 
  mutate(sfund_per_sqmi = npl_sites/sqmi) %>% 
  write_csv("data/base/generated/npl.csv")
