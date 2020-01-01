library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ipumsr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

univ <- read_csv("data/base/univ.csv") 

mfg <- read_csv("hidden/confidential/moodys.csv")%>% 
  left_join(univ,.) %>% select(-cbsa) %>% 
  rename(gmp2007_sector = 4,
         emp2007_sector = 6) %>% 
  select(1,7:8) %>% 
  write_csv("data/base/generated/mfg.csv")
