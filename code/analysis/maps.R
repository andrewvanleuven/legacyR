library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(networkD3)
library(ggpubr)
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

# Make some maps ----------------------------------------------------------

join <- df %>% select(1:2,clusterm_10) %>% 
  rename(cluster = clusterm_10) 
cbsa <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  inner_join(.,join) %>% 
  st_centroid_xy()

us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK","PR","HI")) %>% st_transform(crs = 2163)

clust_names <- paste("Cluster",1:10,sep = " ")
clrs <- rand_ncolors(enframe(clust_names))

ggplot() +
  geom_sf(data = us) + 
  geom_sf(data = cbsa, color = NA, aes(fill = factor(cluster))) +
  scale_fill_manual(name = "Cluster Number",
                    values = clrs,
                    labels = clust_names) +
  ggtitle("Cluster Assignments for MSAs in the Continental U.S.",
          subtitle = "N = 351, K = 10") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        text = element_text(family = "IBM Plex Mono")) +
  ggsave("plot/clusters_10.png", height = 8, width = 15)

for (i in 01:10) {
  title <- sprintf("Cluster %s",i)
  ii <- str_pad(i,width=2, side="left", pad="0")
  plt <- ggplot() + 
    geom_sf(data = us, color = "gray60", fill = "gray90") +
    geom_point(data = cbsa %>% filter(cluster == i), 
               aes(x,y), size = 2, color = "black")+
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), 
          text = element_text(family = "IBM Plex Mono")) 
  nam <- paste0("clustr_map",ii)
  assign(nam, plt)
}
ggarrange(plotlist = lapply(sprintf("clustr_map%s", str_pad(01:10, 2, pad = "0")) , get)) +
  ggsave("plot/clustr_array.png", height = 15, width = 20)


# For the GitHub Page -----------------------------------------------------
ggplot() +
  geom_sf(data = us) + 
  geom_point(data = cbsa, size = 2, aes(x,y,color = factor(cluster))) +
  scale_color_manual(values = clrs) +
  theme_void() +
  theme(legend.position = "none") +
  ggsave("plot/cluster_map.png", height = 8, width = 12)

 
