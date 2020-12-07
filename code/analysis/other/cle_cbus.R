library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(ggsn)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Columbus 1950 Boundaries -------------------------------------------------
cbus_sf <- st_read("data/shp/cbus.kml") %>% 
  rename_all(tolower) %>% 
  select(-description) %>% 
  st_transform(crs = 2163)
sfcolumbus <- cbus_sf %>% filter(name != "Bexley") %>% st_polygonize()
sfbexley <- cbus_sf %>% filter(name == "Bexley") %>% st_polygonize()
cbus1950 <- st_difference(sfcolumbus,sfbexley) %>% st_transform(4269)
franklin <- counties("OH", cb = T) %>% filter(NAME == "Franklin")
cuyahoga <- counties("OH", cb = T) %>% filter(NAME == "Cuyahoga")
franklin_rds <- st_intersection(franklin, (primary_secondary_roads(state = "OH"))) %>% 
  st_collection_extract(type = "LINESTRING")
cuyahoga_rds <- st_intersection(cuyahoga, (primary_secondary_roads(state = "OH"))) %>% 
  st_collection_extract(type = "LINESTRING")
franklin_h2o <- area_water("OH","Franklin")
cuyahoga_h2o <- area_water("OH","Cuyahoga")
cbus2010 <- places("OH", cb = T) %>% filter(NAME == "Columbus")
clev2010 <- places("OH", cb = T) %>% filter(NAME == "Cleveland")
ohio <- counties("39", cb = T)
oh <- states(cb = T, resolution = "20m") %>% filter(STUSPS == "OH")
franken_franklin = st_union(franklin,cbus2010)
franken_cbus = st_intersection(franklin,cbus2010)

ggplot() + 
  geom_sf(data = franklin, fill = "black", alpha = .05, color = "black", size = .2) +
  geom_sf(data = franken_cbus, fill = "black", alpha = .3, color = NA) +
  geom_sf(data = cbus1950, fill = "#BB0000", alpha = .6, color = "black") +
  geom_sf(data = franklin_rds, color = "black") + 
  theme_void() + 
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  ggsave("plot/columbus_comp.png", height = 10, width = 10)
ggplot() + 
  geom_sf(data = cuyahoga, fill = "black", alpha = .05, color = "black", size = .2) +
  geom_sf(data = clev2010, fill = "#BB0000", alpha = .6, color = "black") +
  geom_sf(data = cuyahoga_rds, color = "black") + 
  theme_void() + 
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  ggsave("plot/clev_comp.png", height = 10, width = 10)

cle_col <- st_union(cbus2010,clev2010)

ggplot() + 
  geom_sf(data = ohio, fill = "black", alpha = 0, color = "gray50", size = .2) +
  geom_sf(data = cuyahoga, fill = "black", alpha = .2, color = "gray50") +
  geom_sf(data = clev2010, fill = "#BB0000", alpha = .6, color = "black") +
  geom_sf(data = cuyahoga_rds, color = "black", size = .3) + 
  geom_sf(data = franklin, fill = "black", alpha = .2, color = "gray50") +
  geom_sf(data = franken_cbus, fill = "black", alpha = .4, color = NA) +
  geom_sf(data = cbus1950, fill = "#BB0000", alpha = .7, color = "black") +
  geom_sf(data = franklin_rds, color = "black", size = .3) + 
  theme_void() + 
  coord_sf(xlim = c(-84, -81), ylim = c(39.7, 41.7), expand = FALSE) +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  ggsave("plot/columbus_cle.png")



#ggplot() + 
#  geom_sf(data = cbus1950, fill = "#FFBF89", color = "black") +
#  geom_sf(data = franklin_h2o, color = "navy", fill = "#9CCCEA", lwd = 0.05) + 
#  geom_sf(data = franklin_rds, color = "#C6C6C6") + 
#  geom_sf(data = franklin_rds2, color = "white") + 
#  geom_sf(data = cbus1950, fill = NA, color = "black") +
#  coord_sf(xlim = c(-83.11, -82.9), ylim = c(39.915, 40.07), expand = FALSE) +
#  theme_void() + 
#  theme(panel.background = element_rect(fill = "#D2D2D2")) +
#  ggsave("plot/columbus_1950.png", height = 10, width = 15)

c50 <- cbus1950 %>% st_transform(crs = 2163)
rm(cbus_sf,cbus1950,franklin_h2o,franklin_rds,franklin_rds2,sfbexley,sfcolumbus)

# Census Variables --------------------------------------------------------
v10 <- load_variables(2010, "sf1", cache = T)
v00 <- load_variables(2000, "sf1", cache = T)
v90 <- load_variables(1990, "sf1", cache = T)

# Ohio Block Groups & C1950 Intersect -------------------------------------
oh_bg <- block_groups(state = "OH", cb = T) %>% 
  st_transform(crs = 2163)
c50_bg <- st_intersection(c50, oh_bg) %>% 
  rename(bgid = GEOID) %>% 
  select(bgid, geometry)

# Get Block Group Populations ---------------------------------------------
pop90 <- get_decennial(year = 1990, state = "OH", variables = "P0010001",
                       geography = "block group") %>% 
  rename(bgid = GEOID, pop90 = value) %>% 
  select(bgid, pop90)

pop00 <- get_decennial(year = 2000, variables = "P001001",
                       state = "OH", geography = "block group") %>% 
  rename(bgid = GEOID, pop00 = value) %>% 
  select(bgid, pop00)

pop10 <- read_csv("data/p1.csv") %>% 
  mutate(bgid = (GEO.id2), pop10 = as.numeric(D001)) %>% 
  filter(!is.na(pop10)) %>% 
  select(bgid, pop10)

cbus_1990 <- left_join(c50_bg, pop90) %>% 
  st_drop_geometry() %>% 
  filter(pop90 > 0) %>% 
  summarise(total_1990 = sum(pop90))
cbus_1990 # Total: 135,925

cbus_2000 <- left_join(c50_bg, pop00) %>% 
  st_drop_geometry() %>% 
  filter(pop00 > 0) %>% 
  summarise(total_2000 = sum(pop00))
cbus_2000 # Total: 263,210

cbus_2010 <- left_join(c50_bg, pop10) %>% 
  st_drop_geometry() %>% 
  filter(pop10 > 0) %>% 
  summarise(total_2010 = sum(pop10))
cbus_2010 # Total: 297,131

ggplot() +
  geom_sf(data = (left_join(c50_bg, pop00) %>% st_transform(crs = 4269)),
          aes(fill = pop00), color = NA) + 
  viridis::scale_fill_viridis() + 
  theme_void()
