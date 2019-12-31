library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(httr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)
url <- "https://www.transportation.gov/sites/dot.gov/files/docs/THT_Data_508.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- readxl::read_xlsx(tf,sheet = 2) 
rm(tf,url)

access <- df %>% 
  select(1,5,9) %>% 
  rename(cbsa = 1,
         transit_score = 2,
         walk_score = 3) %>% 
  mutate(access = (as.numeric(transit_score)+as.numeric(walk_score))/2,
         cbsa = str_replace(cbsa,"Macon, GA","Macon-Bibb County, GA")) %>% 
  filter(access > 0) %>% 
  arrange(desc(access)) %>% 
  select(cbsa,access) %>% 
  write_csv("data/base/generated/access.csv")

