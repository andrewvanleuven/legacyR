library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Cluster-Discriminant Function -------------------------------------------
clustr_assign <- dget("code/functions/clustr_assign.R")

# Read in data & specify variables ----------------------------------------
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)
df_spec <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  

# Run clustering function -------------------------------------------------
clusters <- (clustr_assign(df_spec))$combined %>% 
  select(1,2,7,8)

msa_pop <- read_csv("data/base/generated/pop15.csv") %>% 
  inner_join(clusters, by = "cbsa_fips") %>% 
  select(1,3,2,4,5) %>% 
  arrange(desc(population_2015)) %>% 
  write_csv("data/analysis/msa_pop_clusters.csv")
