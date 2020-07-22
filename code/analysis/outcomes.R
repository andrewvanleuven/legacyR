library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(scales)
library(ggpubr)
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
  janitor::adorn_totals("row") %>% 
  mutate(pop_chg = (pop_2015-pop_2005)/pop_2005)
empx <- emp %>% 
  janitor::adorn_totals("row") %>% 
  mutate(emp_chg = (emp_2015-emp_2005)/emp_2005)
gdpx <- gdp %>% 
  janitor::adorn_totals("row") %>% 
  mutate(gdppk_chg = (gdppk_2015-gdppk_2005)/gdppk_2005) 
incx <- inc %>% 
  janitor::adorn_totals("row") %>% 
  mutate(inc_chg = (inc_2015-inc_2005)/inc_2005)

keep <- merged %>% select(1,6:9) %>% 
  mutate(pop_chg = pop_chg_avg-popx[352,6],
         emp_chg = emp_chg_avg-empx[352,6],
         gdp_chg = gdppk_chg_avg-gdpx[352,6],
         inc_chg = inc_chg_avg-incx[352,6]) %>% 
  select(1,6:9)

list_of_datasets <- list("New" = keep, 
                         "Change_05_15" = merged, 
                         "Raw_05_15" = merged2, 
                         "Population" = popx, 
                         "Employment" = empx,
                         "GMP Per Capita" = gdpx, 
                         "Real Per Capita Income" = incx)

df <- popx %>% 
  left_join(empx %>% select(-2,-3), by = "cbsa_fips") %>% 
  left_join(gdpx %>% select(-2,-3), by = "cbsa_fips") %>% 
  left_join(incx %>% select(-2,-3), by = "cbsa_fips") %>% 
  mutate(cluster = as.factor(clusterm_10)) %>% 
  select(1:2,cluster,everything(),-clusterm_10)

#openxlsx::write.xlsx(list_of_datasets, file = "data/analysis/outcomes_raw.xlsx")
rm(pop,emp,inc,gdp,merged,merged2,keep,list_of_datasets,clusters,
   clustr_emp,clustr_pop,clustr_emp_avg,clustr_gdp,clustr_gdp_avg,
   clustr_inc,clustr_inc_avg,clustr_pop_avg,popx,empx,gdpx,incx)
clr_cons()

# Visualization of Distribution -------------------------------------------
zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)
df_cbsa <- df %>% filter(cbsa_fips < 50000)
df_legacy <- df %>% filter(cluster %in% c(2,8))
fillcols <- rev(c('red','green','blue','yellow'))
df_fake2 = data.frame(p = as.numeric(c(NA, NA)),v = c("variable1", "variable2"),
                      l = as.numeric(c(NA, NA)))

ggplot() +
  geom_density(data = df, aes(x = pop_chg), adjust = 1L,   fill = fillcols[1], alpha = .3) +
  geom_density(data = df, aes(x = emp_chg), adjust = 1L,   fill = fillcols[2], alpha = .3) +
  geom_density(data = df, aes(x = gdppk_chg), adjust = 1L, fill = fillcols[3], alpha = .3) +
  geom_density(data = df, aes(x = inc_chg), adjust = 1L,   fill = fillcols[4], alpha = .3) +
  theme_minimal() +
  scale_x_continuous(limits = c(-.5, 1)) 

ggplot() +
 geom_density(data = df_cbsa, aes(x = gdppk_2015, fill = cluster)) +
 scale_fill_brewer(palette = "Set3") +
 labs(x = "Gross Metropolitan Product Per-Capita, 2015", y = "") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(cluster))

(plot_empl <- ggplot() +
    geom_density(data = df_legacy, aes(x = emp_2015, fill = cluster), 
                 alpha = .4, show.legend = FALSE) +
    geom_point(data = df_fake2, aes(x = p, y = l, color = v),size=8, alpha = .4) +
    labs(x = "Employment, 2015", 
         y = "", 
         color = "Cluster") +
    scale_color_manual(values = c('red','blue'),
                       labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                  'High Skill, High Density')) +
    scale_fill_manual(values = c('red','blue'),labels = c('red','blue')) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
          plot.subtitle = element_text(hjust = 0.5),
          text=element_text(family="Futura Medium"),
          axis.text.y=element_blank(),
          legend.key = element_rect(size = 6, fill = NA, color = NA),
          legend.key.height = unit(2, "cm"),
          legend.key.width = unit(1, "cm")))

(plot_popl <- ggplot() +
    geom_density(data = df_legacy, aes(x = pop_2015, fill = cluster), 
                 alpha = .4, show.legend = FALSE) +
    geom_point(data = df_fake2, aes(x = p, y = l, color = v),size=8, alpha = .4) +
    labs(x = "Population, 2015", 
         y = "", 
         color = "Cluster") +
    scale_color_manual(values = c('red','blue'),
                       labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                  'High Skill, High Density')) +
    scale_fill_manual(values = c('red','blue'),labels = c('red','blue')) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
          plot.subtitle = element_text(hjust = 0.5),
          text=element_text(family="Futura Medium"),
          axis.text.y=element_blank(),
          legend.key = element_rect(size = 6, fill = NA, color = NA),
          legend.key.height = unit(2, "cm"),
          legend.key.width = unit(1, "cm")))

(plot_pop <- ggplot() +
  geom_density(data = df_legacy, aes(x = pop_chg, fill = cluster), 
               alpha = .4, show.legend = FALSE) +
  geom_point(data = df_fake2, aes(x = p, y = l, color = v),size=8, alpha = .4) +
  labs(x = "Percent Change in Population, 2005-15", 
       y = "", 
       color = "Cluster") +
  scale_x_continuous(labels = percent, limits = c(-.125, .2)) +
  scale_color_manual(values = c('red','blue'),
                     labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                'High Skill, High Density')) +
  scale_fill_manual(values = c('red','blue'),labels = c('red','blue')) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text=element_text(family="Futura Medium"),
        axis.text.y=element_blank(),
        legend.position = "none",
        legend.key = element_rect(size = 6, fill = NA, color = NA),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"))) 

(plot_emp <- ggplot() +
    geom_density(data = df_legacy, aes(x = emp_chg, fill = cluster), 
                 alpha = .4, show.legend = FALSE) +
    geom_point(data = df_fake2, aes(x = p, y = l, color = v),size=8, alpha = .4) +
    labs(x = "Percent Change in Employment, 2005-15", 
         y = "", 
         color = "Cluster") +
    scale_x_continuous(labels = percent, limits = c(-.175, .3)) +
    scale_color_manual(values = c('red','blue'),
                       labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                  'High Skill, High Density')) +
    scale_fill_manual(values = c('red','blue'),labels = c('red','blue')) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
          plot.subtitle = element_text(hjust = 0.5),
          text=element_text(family="Futura Medium"),
          axis.text.y=element_blank(),
          legend.position = "none",
          legend.key = element_rect(size = 6, fill = NA, color = NA),
          legend.key.height = unit(2, "cm"),
          legend.key.width = unit(1, "cm")))

(plot_gdp <- ggplot() +
    geom_density(data = df_legacy, aes(x = gdppk_chg, fill = cluster), 
                 alpha = .4, show.legend = FALSE) +
    geom_point(data = df_fake2, aes(x = p, y = l, color = v),size=8, alpha = .4) +
    labs(x = "Percent Change in GDP Per-Capita, 2005-15", 
         y = "", 
         color = "Cluster") +
    scale_x_continuous(labels = percent, limits = c(-.25, .7)) +
    scale_color_manual(values = c('red','blue'),
                       labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                  'High Skill, High Density')) +
    scale_fill_manual(values = c('red','blue'),labels = c('red','blue')) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
          plot.subtitle = element_text(hjust = 0.5),
          text=element_text(family="Futura Medium"),
          axis.text.y=element_blank(),
          legend.position = "none",
          legend.key = element_rect(size = 6, fill = NA, color = NA),
          legend.key.height = unit(2, "cm"),
          legend.key.width = unit(1, "cm")))


(plot_inc <- ggplot() +
    geom_density(data = df_legacy, aes(x = inc_chg, fill = cluster), 
                 alpha = .4, show.legend = FALSE) +
    geom_point(data = df_fake2, aes(x = p, y = l, color = v),size=8, alpha = .4) +
    labs(x = "Percent Change in Personal Income Per-Capita, 2005-15", 
         y = "", 
         color = "Cluster") +
    scale_x_continuous(labels = percent, limits = c(-.125, .35)) +
    scale_color_manual(values = c('red','blue'),
                       labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                  'High Skill, High Density')) +
    scale_fill_manual(values = c('red','blue'),labels = c('red','blue')) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
          plot.subtitle = element_text(hjust = 0.5),
          text=element_text(family="Futura Medium"),
          legend.title = element_text(family="Futura Bold"),
          axis.text.y=element_blank(),
          legend.key = element_rect(size = 6, fill = NA, color = NA),
          legend.key.height = unit(2, "cm"),
          legend.key.width = unit(1, "cm")))

(p <- ggarrange(plot_pop,plot_emp,plot_gdp,plot_inc,#plot_popl,plot_empl, 
          common.legend = T, legend="bottom"))

annotate_figure(p,top = text_grob("Visualizing Ten-Year Changes in Outcome Variables for Two Clusters of Legacy Regions", 
                                family="Futura Bold", face = "bold", size = 24)) +
  ggsave("plot/mscore/array.png", width = 20, height = 12)

