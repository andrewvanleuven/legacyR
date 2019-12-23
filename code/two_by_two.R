library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggrepel)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# Per-Capita GMP ----------------------------------------------------------
gmp <- read_csv("data/percap_gmp.csv") %>% 
  mutate(gmp_delta00 = (y11-y01)/y01,
         gmp_delta06 = (y16-y06)/y06) %>% 
  rename(cbsa_fips = fips) %>% 
  select(cbsa_fips,name,gmp_delta00,gmp_delta06)

# Population --------------------------------------------------------------
sts <- states(cb = T) %>%
  filter(GEOID < 57) %>% 
  pull(STUSPS)
pop00 <- get_decennial(year = 2000, variables = "P001001", state = sts, geography = "county") %>% 
  rename(pop2000 = value) %>% select(-variable)
pop10 <- get_decennial(year = 2010, variables = "P001001", state = sts, geography = "county") %>% 
  rename(pop2010 = value) %>% select(GEOID, pop2010)
xw <- cbsaxw %>% select(cty_fips,cbsa_fips:cbsa_type)
pop <- inner_join(pop00,pop10) %>% 
  rename_all(tolower) %>% rename(cty_fips = geoid) %>% 
  left_join(.,xw) %>% group_by(cbsa_fips) %>% 
  summarize(cbsapop00 = sum(pop2000),
            cbsapop10 = sum(pop2010)) %>% 
  left_join(.,(xw %>% select (-cty_fips))) %>% distinct() %>% 
  filter(cbsa_type == "Metro") %>% 
  mutate(pop_delta00 = (cbsapop10-cbsapop00)/cbsapop00) %>% 
  select(cbsa_fips,pop_delta00) 

# Join --------------------------------------------------------------------
join <- inner_join(gmp,pop) %>% 
  select(-gmp_delta06) %>% 
  rename(gmp = gmp_delta00,
         pop = pop_delta00) %>% 
  mutate(a1 = if_else(pop < 0 & gmp < 0, 1, 0),
         a2 = if_else(pop < 0 & gmp > 0, 2, 0),
         b1 = if_else(pop > 0 & gmp < 0, 3, 0),
         b2 = if_else(pop > 0 & gmp > 0, 4, 0),
         quadrant = as.factor(a1+a2+b1+b2),
         name = str_replace_all(name, " \\(Metropolitan Statistical Area\\)","")) %>% 
  select(cbsa_fips, name, everything()) %>% 
  write_csv("data/2by2.csv")

# Map ---------------------------------------------------------------------
cbsa <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  inner_join(.,join) %>% 
  st_centroid_xy()

us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK","PR","HI")) %>% st_transform(crs = 2163)

labs <- c("Shrinking Population, Shrinking GMP",
          "Shrinking Population, Growing GMP",
          "Growing Population, Shrinking GMP",
          "Growing Population, Growing GMP")
clrs <- c("#e41a1c","yellow","#4daf4a","#377eb8")


# CBSA Map ----------------------------------------------------------------
ggplot() + 
  geom_sf(data = us,
          color = "black") +
  geom_point(data = cbsa, aes(x,y, fill = as.factor(quadrant)), 
             size = 2, shape = 21, color = "black") +
  scale_fill_manual(name = "",
                    values = clrs,
                    labels = labs) +
  ggtitle("Growth & Decline in an MSA's Population and Productivity",
          subtitle = "2000 to 2010 (in the continental U.S.)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        text = element_text(family = "IBM Plex Mono")) +
  ggsave("plot/2by2.png", height = 8, width = 15)

# CBSA 2x2 ----------------------------------------------------------------
cbsa_city <- cbsaxw %>% 
  filter(cbsa_type == "Metro") %>% 
  group_by(cbsa_fips) %>% 
  filter(cty_pop_10 == max(cty_pop_10)) %>% 
  arrange(cbsa) %>% 
  mutate(city = paste0(cbsa_main_city,", ",state)) %>% 
  select(cbsa_fips,city)

lab_repel <- read_csv("data/labs.csv") %>% 
  left_join(.,cbsa_city)

ggplot() + 
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(data = lab_repel, (aes(x = pop, y = gmp, fill = as.factor(quadrant))),
             size = 2, shape = 21, color = "black", alpha = .4) +
  geom_text_repel(data = subset(lab_repel, label > 0), 
                  aes(pop, gmp, label = city), family="Roboto Condensed") +
  scale_fill_manual(values = clrs) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 5L)) +
  ggtitle("Growth & Decline in an MSA's Population and Productivity",
          subtitle = "2000 to 2010 (in the continental U.S.)") +
  labs(x ="% Change in Population", y = "% Change in Productivity") +
  theme_bw() +
  theme(plot.title = element_text(size=26, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size=18, hjust = 0.5),
        axis.title.x = element_text(size=20, face="bold", margin = margin(t = 12)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(r = 12)),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.position = "none",
        text=element_text(family="Roboto Condensed")) +
  ggsave("plot/2x2.png", height = 8)

  
  
  

