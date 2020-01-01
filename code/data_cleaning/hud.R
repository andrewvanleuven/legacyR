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

### SOURCE: https://www.huduser.gov/portal/picture/query.html
### https://www.huduser.gov/portal/picture2000/dictionary.pdf
### HUD's PICTURE OF SUBSIDIZED HOUSEHOLDS FOR 2004-2008
df <- read_csv("data/base/source/hud.csv") %>% 
  rename_all(tolower) %>% 
  mutate(cty_fips = str_pad(code, 5, pad = "0")) %>% 
  filter(!program_label %in% c("VO","S236")) %>% 
  group_by(cty_fips) %>% 
  summarise(hud_units = sum(total_units)) %>% 
  mutate(hud_units = replace(hud_units, which(hud_units<0), 0))

housing <- get_decennial(year = 2000, variables = "H001001",
                         state = us, geography = "county") %>% 
  rename(cty_fips = GEOID,
         houses = value) %>% 
  select(cty_fips,houses)

hud <- df %>% 
  left_join(.,housing) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_units_2005 = sum(houses),
            hud_units_2005 = sum(hud_units)) %>% 
  inner_join(.,msaxw) %>% 
  select(1,4,2,3) %>% 
  mutate(pct_ph = hud_units_2005/total_units_2005) %>% 
  write_csv("data/base/generated/hud_units.csv")
