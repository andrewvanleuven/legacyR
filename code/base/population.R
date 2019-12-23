library(tidyverse)

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
  select(1,3,2) 
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
pop00 <- df %>% filter(year == 2000) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2000 = sum(pop)) %>% 
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
  select(1,2) 
pop15 <- df %>% filter(year == 2015) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2015 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,2) 

populations <- inner_join(pop90,pop95) %>% 
  inner_join(.,pop00) %>% 
  inner_join(.,pop05) %>% 
  inner_join(.,pop10) %>% 
  inner_join(.,pop15) %>% 
  write_csv("data/base/generated/populations.csv")

pop_05 <- df %>% filter(year == 2005) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         population = as.numeric(population)) %>% 
  group_by(cty_fips) %>% 
  summarize(pop = sum(population)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(population_2005 = sum(pop)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,3,2) %>% 
  write_csv("data/base/generated/pop_2005.csv")
