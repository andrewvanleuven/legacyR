library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

v00 <- load_variables(2000, "sf1", cache = T)
us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("PR")) %>% pull(STUSPS)

college <- get_decennial(year = 2000, variables = "P001001",
                         state = us, geography = "county") 