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
us <- states(cb = TRUE, resolution = "20m") %>% filter(!STUSPS %in% c("PR")) %>% pull(STUSPS)

### SOURCE: https://seer.cancer.gov/popdata/
df <- read_fwf("hidden/too_big/pop.txt", 
               fwf_widths(c(4,2,2,3,2,1,1,1,2,8),
                          c("year","st","st_fips","cty_fips","registry",
                            "race","origin","sex","age","population")))
xw <- read_csv("data/xw.csv") 
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))
  
pop90 <- df %>% filter(year == 1990) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_1990 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) 
pop95 <- df %>% filter(year == 1995) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_1995 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) 
pop00 <- get_decennial(year = 2000, variables = "P001001", state = us, geography = "county") %>% 
  mutate(cty_fips = GEOID,
         population = value) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2000 = sum(population)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) 
pop05 <- df %>% filter(year == 2005) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2005 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) 
pop10 <- df %>% filter(year == 2010) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2010 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) %>% 
  write_csv("data/base/generated/pop10.csv")
pop15 <- df %>% filter(year == 2015) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2015 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) %>% 
  write_csv("data/base/generated/pop15.csv")

populations <- pop00 %>% 
  #inner_join(.,pop00) %>% 
  inner_join(.,pop05) %>% 
  #inner_join(.,pop10) %>% 
  #inner_join(.,pop15) %>% 
  left_join(univ,.) %>% 
  mutate(pctchg_00_05 = (population_2005-population_2000)/population_2000) %>% 
  write_csv("data/base/generated/populations.csv")

#pop_05 <- df %>% filter(year == 2005) %>% 
#  mutate(cty_fips = paste0(st_fips,cty_fips),
#         population = as.numeric(population)) %>% 
#  group_by(cty_fips) %>% 
#  summarize(pop = sum(population)) %>% 
#  left_join(.,xw) %>% 
#  group_by(cbsa_fips) %>% 
#  summarize(population_2005 = sum(pop)) %>% 
#  inner_join(.,msaxw) %>% 
#  select(1,3,2) %>% 
#  write_csv("data/base/generated/pop_2005.csv")


# Density Calculation -----------------------------------------------------

densities <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  select(cbsa_fips,geometry) %>% 
  inner_join(.,univ) %>% 
  arrange(cbsa_fips) %>% 
  mutate(area = st_area(.),
         sqmi = as.numeric(str_replace_all(area," [m^2]",""))*0.0000003861) %>% 
  st_drop_geometry() %>% 
  left_join(.,populations) %>% 
  select(1,2,4,population_2005) %>% 
  mutate(density = population_2005/sqmi) %>% 
  write_csv("data/base/generated/density_2005.csv")


