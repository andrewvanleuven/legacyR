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

# Read in data & specify variables ----------------------------------------
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)
df_spec <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  

# Run clustering function -------------------------------------------------
clusters <- clustr_assign(df_spec) 

# Sankey Plot Data ------------------------------------------------------
cluster_flow <- clusters$combined %>% 
  select(7:9) %>% 
  select(noquote(order(colSums(.,na.rm=TRUE)))) %>% 
  rename(group_1 = 1,
         group_2 = 2,
         group_3 = 3) %>% 
  group_by(group_3) %>% 
  summarise(k1 = mean(group_1),
            k2 = mean(group_2),
            n = n()) %>% 
  mutate(k1 = paste0("C",str_pad(n_distinct(k1),2,side = "left",pad = "0"),"-",
                     str_pad((k1),2,side = "left",pad = "0")),
         k2 = paste0("C",str_pad(n_distinct(k2),2,side = "left",pad = "0"),"-",
                     str_pad((k2),2,side = "left",pad = "0")),
         k3 = paste0("C",str_pad(n_distinct(group_3),2,side = "left",pad = "0"),"-",
                     str_pad((group_3),2,side = "left",pad = "0"))) %>% 
  select(k1,k2,k3,n) %>% 
  arrange(k1,k2,k3)

a <- clusters$combined %>% 
  select(7:9) %>% 
  select(noquote(order(colSums(.,na.rm=TRUE)))) %>% 
  rename(group_1 = 1,
         group_2 = 2,
         group_3 = 3) %>% 
  group_by(group_1) %>% 
  summarise(n = n()) %>% 
  mutate(k0 = "Universe",
         k1 = paste0("C",str_pad(n_distinct(group_1),2,side = "left",pad = "0"),"-",
                     str_pad((group_1),2,side = "left",pad = "0"))) %>% 
  select(k0,k1,n) %>% 
  arrange(k0,k1) %>% 
  rename(source = 1,
         target = 2,
         value = 3)

ab <- clusters$combined %>% 
  select(7:9) %>% 
  select(noquote(order(colSums(.,na.rm=TRUE)))) %>% 
  rename(group_1 = 1,
         group_2 = 2,
         group_3 = 3) %>% 
  group_by(group_2) %>% 
  summarise(k1 = mean(group_1),
            n = n()) %>% 
  mutate(k1 = paste0("C",str_pad(n_distinct(k1),2,side = "left",pad = "0"),"-",
                     str_pad((k1),2,side = "left",pad = "0")),
         k2 = paste0("C",str_pad(n_distinct(group_2),2,side = "left",pad = "0"),"-",
                     str_pad((group_2),2,side = "left",pad = "0"))) %>% 
  select(k1,k2,n) %>% 
  arrange(k1,k2) %>% 
  rename(source = 1,
         target = 2,
         value = 3)

bc <- clusters$combined %>% 
  select(7:9) %>% 
  select(noquote(order(colSums(.,na.rm=TRUE)))) %>% 
  rename(group_1 = 1,
         group_2 = 2,
         group_3 = 3) %>% 
  group_by(group_3) %>% 
  summarise(k1 = mean(group_1),
            k2 = mean(group_2),
            n = n()) %>% 
  mutate(k1 = paste0("C",str_pad(n_distinct(k1),2,side = "left",pad = "0"),"-",
                     str_pad((k1),2,side = "left",pad = "0")),
         k2 = paste0("C",str_pad(n_distinct(k2),2,side = "left",pad = "0"),"-",
                     str_pad((k2),2,side = "left",pad = "0")),
         k3 = paste0("C",str_pad(n_distinct(group_3),2,side = "left",pad = "0"),"-",
                     str_pad((group_3),2,side = "left",pad = "0"))) %>% 
  select(k2,k3,n) %>% 
  arrange(k2,k3)%>% 
  rename(source = 1,
         target = 2,
         value = 3)

# Generate Sankey Plot ----------------------------------------------------
node_names <- read_csv("data/viz/node_names.csv")

links_fresh <- data.frame(
  rbind(a,ab,bc) %>% 
    arrange(target) #%>% rename(source = 2, target = 1) # This reverses the flow (flips it horizontally)
)

links <- links_fresh %>% 
  left_join(node_names, by = c("target" = "name")) %>% 
  rename(target2 = long_name) %>% 
  left_join(node_names, by = c("source" = "name")) %>% 
  rename(source2 = long_name) %>% 
  select(-source,-target) %>% 
  rename(target = target2,
         source = source2) %>% 
  select(3,2,1)

nodes <- data.frame(name=c(as.character(links$source), 
         as.character(links$target)) %>% unique())

node_cols <- data.frame(name=c(as.character(links_fresh$source),
             as.character(links_fresh$target)) %>% unique()) %>% 
  mutate(group1 = if_else(str_detect(name, "Universe"),1,0),
         group2 = if_else(str_detect(name, "C03"),2,0),
         group3 = if_else(str_detect(name, "C10"),3,0),
         group4 = if_else(str_detect(name, "C18"),4,0),
         group = as.factor(group1+group2+group3+group4)) %>% 
  select(name,group) %>% 
  as.data.frame() %>% 
  left_join(node_names) %>% 
  select(3,2) %>% 
  rename(name = long_name) 

col_nodes <- nodes %>% 
  as.data.frame() %>% 
  left_join(node_cols) 

my_color <- 'd3.scaleOrdinal() .domain([1,2,3,4]) .range(["blue","green","red","yellow"])'

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
sankeyNetwork(Links = links,
              Nodes = col_nodes,
              Source = "IDsource", 
              Target = "IDtarget",
              Value = "value", 
              NodeID = "name", 
              NodeGroup="group",
              colourScale=my_color, 
              sinksRight=TRUE,
              fontFamily = "Roboto Condensed",
              nodeWidth = 20,
              fontSize = 18)

#saveNetwork(sn, "sn.html")
#webshot::webshot("sn.html", "sankey.png")

# Sankey TXT --------------------------------------------------------------
sankey <- links %>% 
  mutate(sankeymatic = paste0(source," [",value,"] ",target)) %>% 
  select(6) %>%
  mutate_all(list(~str_replace(., "C03-0", "Cluster "))) %>% 
  mutate_all(list(~str_replace(., "C10-", "Cluster-"))) %>% 
  mutate_all(list(~str_replace(., "C18-", "Cluster "))) %>% 
  mutate_all(list(~str_replace(., "Universe", "Universe"))) %>% 
  write_csv("plot/sankeymatic.csv")



# Dead Code ---------------------------------------------------------------


#hc <- hclust(dist(scale(cluster_groups), method = "euclidean"), method = "ward.D2")
#plot(as.dendrogram(hc), type = "rectangle", ylab = "Height", ylim = c(.5,30), horiz = TRUE)


