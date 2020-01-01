library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

univ <- read_csv("data/base/univ.csv") 
cbsa <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  select(cbsa_fips,geometry) %>% 
  inner_join(.,univ) %>% 
  arrange(cbsa_fips) 

### SOURCE: https://irma.nps.gov/DataStore/Reference/Profile/2210280

df03 <- st_read("hidden/too_big/register/a0000001d.gdbtable") %>% rename_all(tolower) #NR_Main
df04 <- st_read("hidden/too_big/register/a00000004.gdbtable") %>% rename_all(tolower) #GDB_Items
df01 <- st_read("hidden/too_big/register/a0000001a.gdbtable") %>% rename_all(tolower) #crstru_pt
df02 <- st_read("hidden/too_big/register/a0000001b.gdbtable") %>% rename_all(tolower) #crstru_py
df12 <- st_read("hidden/too_big/register/a00000012.gdbtable") %>% rename_all(tolower) #crbldg_py
df13 <- st_read("hidden/too_big/register/a00000013.gdbtable") %>% rename_all(tolower) #crbldg_pt
df14 <- st_read("hidden/too_big/register/a00000014.gdbtable") %>% rename_all(tolower) #crsite_pt
df15 <- st_read("hidden/too_big/register/a00000015.gdbtable") %>% rename_all(tolower) #crsite_py
df16 <- st_read("hidden/too_big/register/a00000016.gdbtable") %>% rename_all(tolower) #crdist_pt
df17 <- st_read("hidden/too_big/register/a00000017.gdbtable") %>% rename_all(tolower) #crdist_py
df18 <- st_read("hidden/too_big/register/a00000018.gdbtable") %>% rename_all(tolower) #crobj_pt
df19 <- st_read("hidden/too_big/register/a00000019.gdbtable") %>% rename_all(tolower) #crobj_py

hist_strct <- df01 %>%  
  rename(geometry = shape) %>% 
  mutate(date = lubridate::ymd(src_date)) %>% 
  select(resname,geometry,date) %>% 
  filter(date < "2006-01-01") %>% 
  st_transform(crs = 2163) %>%
  mutate(hist_strct = 1)
hist_bldgs <- df12 %>%  
  rename(geometry = shape) %>% 
  mutate(date = lubridate::ymd(src_date)) %>% 
  select(resname,geometry,date) %>%
  filter(date < "2006-01-01") %>% 
  st_transform(crs = 2163) %>%
  mutate(hist_bldg = 1)
hist_sites <- df14 %>%  
  rename(geometry = shape) %>% 
  mutate(date = lubridate::ymd(src_date)) %>% 
  select(resname,geometry,date) %>% 
  filter(date < "2006-01-01") %>% 
  st_transform(crs = 2163) %>%
  mutate(hist_site = 1)
hist_dists <- df16 %>%  
  rename(geometry = shape) %>% 
  mutate(date = lubridate::ymd(src_date)) %>% 
  select(resname,geometry,date) %>% 
  filter(date < "2006-01-01") %>% 
  st_transform(crs = 2163) %>%
  mutate(hist_dist = 1)
hist_objct <- df18 %>%  
  rename(geometry = shape) %>% 
  mutate(date = lubridate::ymd(src_date)) %>% 
  select(resname,geometry,date) %>% 
  filter(date < "2006-01-01") %>% 
  st_transform(crs = 2163) %>%
  mutate(hist_objct = 1)


sf_hist_bldgs <- st_intersection(cbsa,hist_bldgs) %>% st_drop_geometry() %>% select(1)
sf_hist_strct <- st_intersection(cbsa,hist_strct) %>% st_drop_geometry() %>% select(1)
sf_hist_sites <- st_intersection(cbsa,hist_sites) %>% st_drop_geometry() %>% select(1)
sf_hist_objct <- st_intersection(cbsa,hist_objct) %>% st_drop_geometry() %>% select(1)
sf_hist_dists <- st_intersection(cbsa,hist_dists) %>% st_drop_geometry() %>% select(1)

bldgs <- sf_hist_bldgs %>% arrange(cbsa_fips) %>% group_by(cbsa_fips) %>% summarise(bldgs = n()) 
strct <- sf_hist_strct %>% arrange(cbsa_fips) %>% group_by(cbsa_fips) %>% summarise(strct = n()) 
sites <- sf_hist_sites %>% arrange(cbsa_fips) %>% group_by(cbsa_fips) %>% summarise(sites = n()) 
objct <- sf_hist_objct %>% arrange(cbsa_fips) %>% group_by(cbsa_fips) %>% summarise(objct = n()) 
dists <- sf_hist_dists %>% arrange(cbsa_fips) %>% group_by(cbsa_fips) %>% summarise(dists = n())

hist_register <- st_drop_geometry(cbsa) %>% 
  left_join(.,bldgs, by = "cbsa_fips") %>% 
  left_join(.,strct, by = "cbsa_fips") %>% 
  left_join(.,sites, by = "cbsa_fips") %>% 
  left_join(.,objct, by = "cbsa_fips") %>% 
  left_join(.,dists, by = "cbsa_fips") %>% 
  replace_na(.,list(bldgs=0, strct=0, sites=0, objct=0, dists=0)) %>% 
  mutate(hist_reg = bldgs+strct+sites+objct+dists) %>% 
  write_csv("data/base/generated/hist_register.csv")

rm(df03,df04,df01,df02,df12,df13,df14,df15,df16,df17,df18,df19,
   hist_bldgs,hist_strct,hist_sites,hist_objct,hist_dists)

# Definitions -------------------------------------------------------------
# This feature class (CR or Cultural Resources point) describes properties listed on the National Register of Historic Places, classified as historic buildings, and depicted as points. The National Register of Historic Places requires the submission a single coordinate pair for properties under 10 acres. Resources are identified as one of the following feature types: buildings, districts, objects, sites, and structures. A building, such as a house, barn, church, hotel, or similar construction, is created principally to shelter any form of human activity. A building may also be used to refer to a historically and functionally related unit, such as a courthouse and jail or a house and barn. Buildings include: houses, barns, stables, sheds, garages, courthouses, city halls, social halls, commercial buildings, libraries, factories, mills, train depots, stationary mobile homes, hotels, theaters, schools, stores and churches. A district possesses a significant concentration, linkage, or continuity of sites, buildings, structures, or objects united historically or aesthetically by plan or physical development. Districts include: college campuses, central business districts, residential areas, commercial areas, large forts, industrial complexes, civic centers, rural villages, canal systems, collections of habitation and limited activity sites, irrigation systems, large farms, ranches, estates, plantations, transportation networks and large landscaped parks. An object is a feature that is primarily artistic in nature or is relatively small in scale and simply constructed. Although an object may be, by nature or design moveable, an object is associated with a specific setting or environment. Objects include: sculpture, monuments, boundary markers, statuary and fountains. A site is the location of a significant event, a prehistoric or historic occupation or activities, or a building or structure, whether standing, ruined or vanished, where the location itself possess historic, cultural or archaeological value regardless of the value of any existing structure. Sites include: habitation sites, rock shelters, village sites, ceremonial sites, petroglyphs, gardens, grounds, battlefields, ruins of historic buildings and structures, campsites, areas of land, shipwrecks, cemeteries, designed landscapes, archaeological sites and landscapes having cultural significance. A structure is a building whose functional construction is made usually for purposes other than creating human shelter. Structures include: bridges, tunnels, gold dredges, fire towers, canals, turbines, dams, power plants, corncribs, silos, roadways, shot towers, windmills, grain elevators, kilns, mounds, cairns, palisade fortifications, earthworks, railroad grades, systems of roadways and paths, boats and ships, railroad locomotives and cars, telescopes, carousels, bandstands, gazebos and aircraft. Attribute data in this dataset are intentionally limited to those necessary for spatial data maintenance and feature level metadata necessary to document the lineage of the geography itself.
