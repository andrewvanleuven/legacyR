library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Cluster-Discriminant ----------------------------------------------------
clustr_assign <- dget("code/functions/clustr_assign.R")
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)
df_spec <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  
clusters <- (clustr_assign(df_spec))$combined %>% 
  select(1,2,7)
rm(df_spec,id,df,clustr_assign)

# Outcome 1: Population Growth --------------------------------------------
#Population (Number of persons)
pop <- read_csv("data/base/source/bea_pop.csv") %>%
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "19430", "19380"))) %>% # bad FIPS for Dayton, OH
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "39150", "39140"))) %>% # bad FIPS for Prescott, AZ
  select(-cbsa) %>% 
  inner_join(clusters,., by = "cbsa_fips") 

clustr_pop_avg <- pop %>% 
  mutate(pop_chg_msa = (pop_2015-pop_2005)/pop_2005) %>% 
  group_by(clusterm_10) %>% 
  summarise(pop_chg_avg = mean(pop_chg_msa), 
            pop_05_avg = mean(pop_2005),
            pop_15_avg = mean(pop_2015)) 

clustr_pop <- pop %>% 
  group_by(clusterm_10) %>% 
  summarise(cpop_05 = sum(pop_2005),
            cpop_15 = sum(pop_2015)) %>% 
  mutate(pop_chg_gross = (cpop_15-cpop_05)/cpop_05) %>% 
  left_join(clustr_pop_avg, by = "clusterm_10") %>% 
  select(1,4,5)

# Outcome 2: Employment Growth --------------------------------------------
#Total employment (Number of jobs) 
emp <- read_csv("data/base/source/bea_emp.csv") %>% 
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "19430", "19380"))) %>% # bad FIPS for Dayton, OH
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "39150", "39140"))) %>% # bad FIPS for Prescott, AZ
  select(-cbsa) %>% 
  inner_join(clusters,., by = "cbsa_fips") 

clustr_emp_avg <- emp %>% 
  mutate(emp_chg_msa = (emp_2015-emp_2005)/emp_2005) %>% 
  group_by(clusterm_10) %>% 
  summarise(emp_chg_avg = mean(emp_chg_msa), 
            emp_05_avg = mean(emp_2005),
            emp_15_avg = mean(emp_2015)) 

clustr_emp <- emp %>% 
  group_by(clusterm_10) %>% 
  summarise(cemp_05 = sum(emp_2005),
            cemp_15 = sum(emp_2015)) %>% 
  mutate(emp_chg_gross = (cemp_15-cemp_05)/cemp_05) %>% 
  left_join(clustr_emp_avg, by = "clusterm_10") %>% 
  select(1,4,5)

# Outcome 3: GDP Per-Capita -----------------------------------------------
#Real GDP: All industry total (Thousands of chained 2012 dollars) 
gdp <- read_csv("data/base/source/bea_gdp.csv") %>% 
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "19430", "19380"))) %>% # bad FIPS for Dayton, OH
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "39150", "39140"))) %>% # bad FIPS for Prescott, AZ
  select(-cbsa) %>% 
  inner_join(pop %>% select(1,4,5), by = "cbsa_fips") %>% 
  mutate(gdppk_2005 = gdp_2005/pop_2005,
         gdppk_2015 = gdp_2015/pop_2015) %>% 
  select(-(2:5)) %>% 
  inner_join(clusters,., by = "cbsa_fips")

clustr_gdp_avg <- gdp %>% 
  mutate(gdppk_chg_msa = (gdppk_2015-gdppk_2005)/gdppk_2005) %>% 
  group_by(clusterm_10) %>% 
  summarise(gdppk_chg_avg = mean(gdppk_chg_msa), 
            gdppk_05_avg = mean(gdppk_2005),
            gdppk_15_avg = mean(gdppk_2015)) 

clustr_gdp <- gdp %>% 
  group_by(clusterm_10) %>% 
  summarise(cgdppk_05 = sum(gdppk_2005),
            cgdppk_15 = sum(gdppk_2015)) %>% 
  mutate(gdppk_chg_gross = (cgdppk_15-cgdppk_05)/cgdppk_05) %>% 
  left_join(clustr_gdp_avg, by = "clusterm_10") %>% 
  select(1,4,5)

# Outcome 4: Income Per-Capita --------------------------------------------
#Per capita personal income (Dollars)
inc <- read_csv("data/base/source/bea_inc.csv") %>% 
  mutate(inc_2005 = inc_2005*1.2, # https://www.bls.gov/data/inflation_calculator.htm
         cbsa_fips = as.numeric(str_replace(cbsa_fips, "19430", "19380"))) %>% # bad FIPS for Dayton, OH
  mutate(cbsa_fips = as.numeric(str_replace(cbsa_fips, "39150", "39140"))) %>% # bad FIPS for Prescott, AZ
  select(-cbsa) %>% 
  inner_join(clusters,., by = "cbsa_fips") 

clustr_inc_avg <- inc %>% 
  mutate(inc_chg_msa = (inc_2015-inc_2005)/inc_2005) %>% 
  group_by(clusterm_10) %>% 
  summarise(inc_chg_avg = mean(inc_chg_msa), 
            inc_05_avg = mean(inc_2005),
            inc_15_avg = mean(inc_2015)) 

clustr_inc <- inc %>% 
  group_by(clusterm_10) %>% 
  summarise(cinc_05 = sum(inc_2005),
            cinc_15 = sum(inc_2015)) %>% 
  mutate(inc_chg_gross = (cinc_15-cinc_05)/cinc_05) %>% 
  left_join(clustr_inc_avg, by = "clusterm_10") %>% 
  select(1,4,5)

# Merge All ---------------------------------------------------------------

merged <- clustr_pop %>% 
  left_join(clustr_emp, by = "clusterm_10") %>% 
  left_join(clustr_gdp, by = "clusterm_10") %>% 
  left_join(clustr_inc, by = "clusterm_10") %>% 
  rename(cluster = clusterm_10) %>% 
  select(1,2,4,6,8,3,5,7,9) 

merged2 <- clustr_pop_avg %>% 
  left_join(clustr_emp_avg, by = "clusterm_10") %>% 
  left_join(clustr_gdp_avg, by = "clusterm_10") %>% 
  left_join(clustr_inc_avg, by = "clusterm_10") %>% 
  rename(cluster = clusterm_10) 

popx <- pop %>% 
  mutate(pop_chg = (pop_2015-pop_2005)/pop_2005)
empx <- emp %>% 
  mutate(emp_chg = (emp_2015-emp_2005)/emp_2005)
gdpx <- gdp %>% 
  mutate(gdppk_chg = (gdppk_2015-gdppk_2005)/gdppk_2005) 
incx <- inc %>% 
  mutate(inc_chg = (inc_2015-inc_2005)/inc_2005)

list_of_datasets <- list("Change_05_15" = merged, "Raw_05_15" = merged2, "Population" = popx, "Employment" = empx,
                         "GMP Per Capita" = gdpx, "Real Per Capita Income" = incx)
openxlsx::write.xlsx(list_of_datasets, file = "data/analysis/outcomes_raw.xlsx")
