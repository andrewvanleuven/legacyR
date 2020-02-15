library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(networkD3)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Cluster-Discriminant Function -------------------------------------------
clustr_assign <- dget("code/functions/clustr_assign.R")
zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)
is.binary <- function(j) {x <- unique(j)
length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)}

# Read in data & specify variables ----------------------------------------
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)
df_spec <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  

# Run clustering function -------------------------------------------------
clusters <- clustr_assign(df_spec) 
rm(clustr_assign,df,id,df_spec)
df <- clusters$combined
rm(clusters)

# Create Indices ----------------------------------------------------------

assets <- df %>% select(access:ln_hist_bldg) %>% 
  mutate_if(Negate(is.binary),zscore) %>% 
  mutate(asst_score = rowSums(.)/ncol(.)) %>% 
  select(tail(names(.), 1)) %>% 
  cbind((df %>% select(1:2)),.)

liabilities <- df %>% select(city_dec_since_peak_nonzero:edu_bachelors_plus) %>% 
  mutate(pctchg_00_05 = pctchg_00_05*-1,
         med_val_cbsa = med_val_cbsa*-1,
         edu_bachelors_plus = edu_bachelors_plus*-1) %>% 
  mutate_if(Negate(is.binary),zscore) %>% 
  mutate(liab_score = rowSums(.)/ncol(.)) %>% 
  select(tail(names(.), 1)) %>% 
  cbind((df %>% select(1:2)),.)

twobytwo_10 <- inner_join(assets,liabilities) %>% 
  inner_join(df %>% select(1:2,clusterm_10)) %>% 
  group_by(clusterm_10) %>% 
  summarise(asset = mean(asst_score),
            liability = mean(liab_score))

twobytwo_18 <- inner_join(assets,liabilities) %>% 
  inner_join(df %>% select(1:2,clusterm_18)) %>% 
  group_by(clusterm_18) %>% 
  summarise(asset = mean(asst_score),
            liability = mean(liab_score))

ggplot(data = twobytwo_10, aes(x = asset, y = liability, label = paste0("Cluster ",clusterm_10))) +
  geom_vline(xintercept = 0, linetype="dashed", color = "#c9c9c9") +
  geom_hline(yintercept = 0, linetype="dashed", color = "#c9c9c9") +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(family="Roboto Condensed",color = "#4a4a4a") +
  scale_x_continuous(breaks=c(-.5,0,.5,1), labels=c(-.5,0,.5,1)) + 
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal() +
  labs(x ="Asset Score", y = "Liability Score") +
  theme(plot.title = element_text(size=26, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=20, face="bold", margin = margin(t = 12)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(r = 12)),
        legend.title=element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.text=element_text(size=16),
        legend.position = "none",
        text=element_text(family="Roboto Condensed")) +
  ggsave("plot/2by2.png", width = 12, height = 9)

ggplot(data = twobytwo_18, aes(x = asset, y = liability, label = paste0("Cluster ",clusterm_18))) +
  geom_vline(xintercept = 0, linetype="dashed", color = "#c9c9c9") +
  geom_hline(yintercept = 0, linetype="dashed", color = "#c9c9c9") +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(family="Roboto Condensed",color = "#4a4a4a") +
  scale_x_continuous(breaks=c(-.5,0,.5,1), labels=c(-.5,0,.5,1)) + 
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal() +
  labs(x ="Asset Score", y = "Liability Score") +
  theme(plot.title = element_text(size=26, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=20, face="bold", margin = margin(t = 12)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(r = 12)),
        legend.title=element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.text=element_text(size=16),
        legend.position = "none",
        text=element_text(family="Roboto Condensed")) +
  ggsave("plot/2by2_18.png", width = 12, height = 9)


