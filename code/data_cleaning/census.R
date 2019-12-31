library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Load Data ---------------------------------------------------------------
vsf1 <- load_variables(2000, "sf1", cache = T)
vsf3 <- load_variables(2000, "sf3", cache = T)
us <- states(cb = TRUE, resolution = "20m") %>% filter(!STUSPS %in% c("PR")) %>% pull(STUSPS)
xw <- read_csv("data/xw.csv") 
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))
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
  mutate(vacancy_msa = total_vacant_2000/total_units_2000) %>% 
  select(cbsa_fips,cbsa,total_vacant_2000,total_units_2000,vacancy_msa) %>% 
  write_csv("data/base/generated/vacancy_msa.csv")

vacanc <- get_decennial(year = 2000, variables = c("H001001","H005001"), state = us, geography = "place") %>% 
  pivot_wider(names_from = variable) %>% 
  rename(city_fips = GEOID)

vacant_city <- inner_join(cxw,vacanc, by = "city_fips") %>% 
  inner_join(.,msaxw, by = "cbsa_fips") %>% 
  select(cbsa_fips,cbsa,city_fips,city,H005001,H001001) %>% 
  mutate(vacancy_city = H005001/H001001) %>% 
  select(-H005001,-H001001,-cbsa) %>% 
  left_join(.,vacant, by = "cbsa_fips") %>% 
  select(cbsa_fips,cbsa,vacancy_msa,vacancy_city) %>% 
  write_csv("data/base/generated/vacancy_city_msa.csv")


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


# Poverty Rate ------------------------------------------------------------
poverty <- get_decennial(year = 2000,
                         variables = paste0("P0",87001:87017), 
                         state = us, 
                         geography = "county") %>% 
  pivot_wider(names_from = variable) %>% 
  rename(cty_fips = GEOID) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_in_poverty = sum(P087002),
            total = sum(P087001)) %>% 
  inner_join(.,msaxw) %>% 
  mutate(poverty_rate = total_in_poverty/total) %>% 
  select(cbsa_fips,cbsa,total_in_poverty,total,poverty_rate) %>% 
  write_csv("data/base/generated/poverty.csv")



# Educational Attainment --------------------------------------------------
education <- get_decennial(year = 2000,
                         variables = paste0("P0",37001:37035), 
                         state = us, 
                         geography = "county") %>% 
  pivot_wider(names_from = variable) %>% 
  mutate(total = P037002+P037019,
         no_hs = P037003 + P037004 + P037005 + P037006 + P037007 + 
           P037008 + P037009 + P037010 + P037020 + P037021 + P037022 + 
           P037023 + P037024 + P037025 + P037026 + P037027,
         bachelors = P037015+P037032,
         masters = P037016+P037033,
         prof = P037017+P037034,
         phd = P037018+P037035,
         college_degree = bachelors + masters + prof + phd) %>% 
  rename(cty_fips = GEOID) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_bach = sum(bachelors),
            total_mast = sum(masters),
            total_prof = sum(prof),
            total_doct = sum(phd),
            total_nohs = sum(no_hs),
            total_degr = sum(college_degree),
            total_ov25 = sum(total)) %>% 
  inner_join(.,msaxw) %>% 
  mutate(bachelors_plus = total_degr/total_ov25,
         no_hs_diploma = total_nohs/total_ov25) %>% 
  select(cbsa_fips,cbsa,no_hs_diploma,bachelors_plus,everything()) %>% 
  write_csv("data/base/generated/education.csv")


# Nativity ----------------------------------------------------------------
nativity <- get_decennial(year = 2000,
                         variables = paste0("P0",21001:21015), 
                         state = us, 
                         geography = "county") %>% 
  pivot_wider(names_from = variable) %>% 
  rename(cty_fips = GEOID) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(foreign_born = sum(P021013),
            native = sum(P021002),
            total = sum(P021001)) %>% 
  inner_join(.,msaxw) %>% 
  mutate(not_foreign_born = total-foreign_born,
         nativity = not_foreign_born/total) %>% 
  select(cbsa_fips,cbsa,total,foreign_born,not_foreign_born,native,nativity) %>% 
  write_csv("data/base/generated/nativity.csv")

# Housing Stock Age -------------------------------------------------------
# H034010	Total:  Built 1939 or earlier	(Year Structure Built)
housing_stock <- get_decennial(year = 2000,
                         variables = paste0("H0",34001:34010), 
                         state = us, 
                         geography = "county") %>% 
  pivot_wider(names_from = variable) %>% 
  rename(cty_fips = GEOID) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_prewar = sum(H034010),
            total = sum(H034001)) %>% 
  inner_join(.,msaxw) %>% 
  mutate(pct_prewar_msa = total_prewar/total) %>% 
  select(cbsa_fips,pct_prewar_msa)

h_stock_city <- get_decennial(year = 2000,
                               variables = c("H034001","H034010"),
                               state = us, 
                               geography = "place") %>%
  pivot_wider(names_from = variable) %>% 
  rename(city_fips = GEOID,
         total = H034001,
         total_prewar = H034010) %>% 
  inner_join(cxw,., by = "city_fips") %>% 
  inner_join(.,msaxw, by = "cbsa_fips") %>% 
  mutate(pct_prewar_city = total_prewar/total) %>% 
  select(cbsa_fips,cbsa,city_fips,city,pct_prewar_city) %>% 
  left_join(.,housing_stock,by = "cbsa_fips") %>% 
  write_csv("data/base/generated/prewar_housing_stock.csv")

# Housing Value -------------------------------------------------------
# H085001 Median Value (Dollars) for All Owner-occupied Housing Units 
housing_value <- get_decennial(year = 2000,
                               variables = "H085001", 
                               state = us, 
                               geography = "place") %>% 
  rename(city_fips = GEOID,
         median_value = value)


h_value_city <- inner_join(cxw,housing_value, by = "city_fips") %>% 
  inner_join(.,msaxw, by = "cbsa_fips") %>% 
  select(cbsa_fips,cbsa,city_fips,city,median_value) %>% 
  write_csv("data/base/source/housing_value.csv")


