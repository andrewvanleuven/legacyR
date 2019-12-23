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
xw <- read_csv("data/xw.csv") 
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))

# Vacancy Rate ------------------------------------------------------------
houses <- get_decennial(year = 2000, variables = "H001001", state = us, geography = "county") 
vacant <- get_decennial(year = 2000, variables = c("H001001","H005001"), state = us, geography = "county") %>% 
  pivot_wider(names_from = variable) %>% 
  rename(cty_fips = GEOID) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_units_2000 = sum(H001001),
            total_vacant_2000 = sum(H005001)) %>% 
  inner_join(.,msaxw) %>% 
  mutate(vacancy = total_vacant_2000/total_units_2000) %>% 
  select(cbsa_fips,cbsa,total_vacant_2000,total_units_2000,vacancy) %>% 
  write_csv("data/base/generated/vacancy_msa.csv")

city_pop <- get_decennial(year = 2000, variables = "P001001", state = us, geography = "place") %>% 
  rename(city_fips = GEOID, city = NAME, pop = value) %>% select(city_fips,city,pop)

cxw <- read_csv("data/city_xw.csv") %>% 
  filter(cbsa_type == "Metropolitan Statistical Area",
         !str_detect(cbsa, ', PR')) %>%
  mutate(city_fips = str_pad(city_fips,7,"left","0")) %>% 
  select(-city) %>% 
  left_join(.,city_pop,by = "city_fips") %>% 
  group_by(cbsa_fips) %>% 
  filter(pop == max(pop))%>% 
  select(-cbsa) 

vacanc <- get_decennial(year = 2000, variables = c("H001001","H005001"), state = us, geography = "place") %>% 
  pivot_wider(names_from = variable) %>% 
  rename(city_fips = GEOID)

vacant_city <- inner_join(cxw,vacanc, by = "city_fips") %>% 
  inner_join(.,msaxw, by = "cbsa_fips") %>% 
  select(cbsa_fips,cbsa,city_fips,city,H005001,H001001) %>% 
  mutate(vacancy = H005001/H001001) %>% 
  select(-H005001,-H001001) %>% 
  write_csv("data/base/generated/vacancy_city.csv")


# Under 18 and Over 65 ----------------------------------------------------
m <- get_decennial(year = 2000, variables = "P012002", state = us, geography = "county") %>% 
  rename(tm = value) %>% select(GEOID,4)
f <- get_decennial(year = 2000, variables = "P012026", state = us, geography = "county") %>% 
  rename(tf = value) %>% select(GEOID,4)
ym <- get_decennial(year = 2000, variables = paste0("P0",12003:12006), state = us, geography = "county") %>% 
  group_by(GEOID) %>% 
  summarize(ym = sum(value))
om <- get_decennial(year = 2000, variables = paste0("P0",12020:12025), state = us, geography = "county") %>% 
  group_by(GEOID) %>% 
  summarize(om = sum(value))
yf <- get_decennial(year = 2000, variables = paste0("P0",12027:12030), state = us, geography = "county") %>% 
  group_by(GEOID) %>% 
  summarize(yf = sum(value))
of <- get_decennial(year = 2000, variables = paste0("P0",12044:12049), state = us, geography = "county") %>% 
  group_by(GEOID) %>% 
  summarize(of = sum(value))

ages <- inner_join(m,f) %>% 
  inner_join(.,ym, by = "GEOID") %>% 
  inner_join(.,om, by = "GEOID") %>% 
  inner_join(.,yf, by = "GEOID") %>% 
  inner_join(.,of, by = "GEOID") %>% 
  rename(cty_fips = GEOID) %>% 
  mutate(u18 = ym+yf,
         o65 = om+of,
         all = tm+tf) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_young = sum(u18),
            total_old = sum(o65),
            total = sum(all)) %>% 
  inner_join(.,msaxw) %>% 
  mutate(pct_u18 = total_young/total,
         pct_o64 = total_old/total) %>% 
  select(cbsa_fips,cbsa,everything()) %>% 
  write_csv("data/base/generated/ages.csv")



