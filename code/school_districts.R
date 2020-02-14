library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(networkD3)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

indy <- places(state = "IN", cb = T) %>% 
  filter(NAME == "Indianapolis city (balance)")
columbus <- places(state = "OH", cb = T) %>% 
  filter(NAME == "Columbus")
cleveland <- places(state = "OH", cb = T) %>% 
  filter(NAME == "Cleveland")


ics <- school_districts(state = "IN")
ccs <- school_districts(state = "OH")

indy_school <- st_intersection(ics,indy)
cbus_school <- st_intersection(ccs,columbus)
clev_school <- st_intersection(ccs,cleveland)

ggplot() +
  geom_sf(data = cbus_school, color = NA, aes(fill = as.factor(NAME))) +
  geom_sf(data = columbus, size = 1.1, fill = NA, color = "black") +
  theme_void() +
  ggsave("plot/cbus_school.png", height = 12, width = 15)

ggplot() +
  geom_sf(data = clev_school, color = NA, aes(fill = as.factor(NAME))) +
  geom_sf(data = cleveland, size = 1.1, fill = NA, color = "black") +
  theme_void() +
  ggsave("plot/clev_school.png", height = 12, width = 15)

ggplot() +
  geom_sf(data = indy_school, color = NA, aes(fill = as.factor(NAME))) +
  geom_sf(data = indy, size = 1.1, fill = NA, color = "black") +
  theme_void() +
  ggsave("plot/indy_school.png", height = 12, width = 15)
