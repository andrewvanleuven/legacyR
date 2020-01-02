library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Read in data ------------------------------------------------------------
df <- read_csv("data/master.csv") %>% select(read_csv("data/analysis/vars.csv") %>% pull()) 
id <- df %>% select(1:2)

# Functions ---------------------------------------------------------------
clustr <- function(scaled_data) {
  d <- dist(scaled_data, method = "euclidean")
  hclust(d, method = "ward.D2")}
agg_sched <- function(clustr){  
  as.data.frame(table(clustr$height)) %>% 
    mutate(stage = as.numeric(rownames(.)),
           clusters = (nrow(.)+1)-stage,
           aggcoeff = as.numeric(levels(Var1))[Var1],
           lag = as.numeric(lag(aggcoeff)),
           slope = (aggcoeff - lag) / lag,
           lag2 = as.numeric(lag(slope)),
           accel = (slope - lag2) / lag2,) %>% 
    select(stage,clusters,aggcoeff,slope,accel) %>% 
    filter(clusters <= 20)
}
solutions <- function(agg_schedule){
  agg_schedule %>% 
    mutate(rank = rank(-(abs(accel))),
           solution = clusters + 1) %>% 
    arrange(rank) %>% 
    filter(rank <= 5) %>% 
    select(rank,accel,solution)
}
clustr.dend <-function(clustr,num_k){
  plot(clustr, cex = 0.6, hang = -1, labels = FALSE)
  rect.hclust(clustr, k = num_k, border = "red")
}
clustr.assign <- function(scaled_data,num_k,data = NULL,method = NULL){
  if(is.null(method)) {
    jeff <- paste0("cluster_",num_k)
  } else {
    jeff <- paste0("cluster",method,"_",num_k)
  }
  c <- clustr(scaled_data)
  cluster <- cutree(c, k = num_k)
  if(is.null(data)) {
    final <- cbind(id,cluster)
  } else {
    final <- cbind(id,cluster,data)
  }
  final %>% rename(., !!jeff := cluster)
}
discrim.means <- function(data,discriminant){
  clustr_grp <- data %>% pull(cluster)
  scores <- predict(discriminant, data)$x
  cbind(clustr_grp,scores) %>% as.data.frame() %>% 
    group_by(clustr_grp) %>% 
    summarise_all(.funs = mean)
}
discrim.scores <- function(data,discriminant){
  clustr_grp <- data %>% pull(cluster)
  scores <- predict(discriminant, data)$x
  cbind(clustr_grp,scores) %>% as.data.frame()
}
discr.mtrx <- function(discriminant,data){
  pred <- predict(discriminant)
  data$lda <- pred$class
  tbl <- table(data$cluster,data$lda)
  tbldf <- as.data.frame.array(tbl)
  tbldf$Actual <- rowSums(tbldf)
  tbldf["Predicted" ,] <- colSums(tbldf)
  #write.csv(tbldf,"hit ratio.csv")
  tbldf
}
hit.ratio <- function(discriminant,data){
  pred <- predict(discriminant)
  data$lda <- pred$class
  mtx <- table(data$cluster,data$lda)
  n <- sum(rowSums(mtx))
  preds <- sum(diag(mtx))
  (preds/n)*100
}
zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)

# Clustering --------------------------------------------------------------
dfs <- df %>% select(-(1:2))
adj_vars <- df %>% select(st_cap) #%>% mutate(r1 = r1/7)
z <- dfs %>% select(-st_cap) %>% 
  mutate_all(.,zscore) %>% 
  cbind(.,adj_vars) %>% 
  select(1:2, st_cap, everything())
m <- dfs %>% select(-st_cap) %>% 
  mutate_all(.,mscore) %>% 
  cbind(.,adj_vars) %>% 
  select(1:2, st_cap, everything())
#agg_sched(clustr(z)) %>% write_csv("data/analysis/agg_sched_z.csv")
#agg_sched(clustr(m)) %>% write_csv("data/analysis/agg_sched_m.csv")
zsols <- solutions(agg_sched(clustr(z))) # Ideal K: 15 and 5
zsols
msols <- solutions(agg_sched(clustr(m))) # Ideal K: 16 and 6 (and 13)
msols

clustr_z1 <- clustr.assign(z,zsols[1,3],method = "z")
clustr_z2 <- clustr.assign(z,zsols[2,3],method = "z") %>% select(3)
clustr_z3 <- clustr.assign(z,zsols[3,3],method = "z") %>% select(3)
clustr_z4 <- clustr.assign(z,zsols[4,3],method = "z") %>% select(3)
clustr_m1 <- clustr.assign(m,msols[1,3],method = "m") %>% select(3)
clustr_m2 <- clustr.assign(m,msols[2,3],method = "m") %>% select(3)
clustr_m3 <- clustr.assign(m,msols[3,3],method = "m") %>% select(3)
clustr_m4 <- clustr.assign(m,msols[4,3],method = "m") %>% select(3)

cluster_assignments <- cbind(clustr_z1,clustr_z2,clustr_z3,clustr_z4,clustr_m1,clustr_m2,clustr_m3,clustr_m4) %>% 
  write_csv("data/analysis/cluster_assignments.csv")

rm(clustr_z1,clustr_z2,clustr_z3,clustr_z4,clustr_m1,clustr_m2,clustr_m3,clustr_m4,adj_vars)

# Discriminant HRs---------------------------------------------------------
cdf <- cbind(cluster_assignments,dfs) %>% write_csv("data/clustering.csv")

df_z1 <- cdf %>% select(03,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_z2 <- cdf %>% select(04,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_z3 <- cdf %>% select(05,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_z4 <- cdf %>% select(06,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_m1 <- cdf %>% select(07,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_m2 <- cdf %>% select(08,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_m3 <- cdf %>% select(09,density:edu_bachelors_plus) %>% rename(cluster = 1)
df_m4 <- cdf %>% select(10,density:edu_bachelors_plus) %>% rename(cluster = 1)

disc_z1 <- MASS::lda(cluster~., data = df_z1)
disc_z2 <- MASS::lda(cluster~., data = df_z2)
disc_z3 <- MASS::lda(cluster~., data = df_z3)
disc_z4 <- MASS::lda(cluster~., data = df_z4)
disc_m1 <- MASS::lda(cluster~., data = df_m1)
disc_m2 <- MASS::lda(cluster~., data = df_m2)
disc_m3 <- MASS::lda(cluster~., data = df_m3)
disc_m4 <- MASS::lda(cluster~., data = df_m4)

eig_z1 <- disc_z1$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_z2 <- disc_z2$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_z3 <- disc_z3$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_z4 <- disc_z4$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_m1 <- disc_m1$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_m2 <- disc_m2$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_m3 <- disc_m3$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))
eig_m4 <- disc_m4$svd %>% enframe(name = NULL) %>% mutate(pct_of_variance = 100*(value^2/sum(value^2)))

eig_z1
eig_z2
eig_z3
eig_z4
eig_m1
eig_m2
eig_m3
eig_m4

disc_z1$counts
disc_z2$counts
disc_z3$counts
disc_z4$counts
disc_m1$counts
disc_m2$counts
disc_m3$counts
disc_m4$counts

hr_cluster_z1 <- hit.ratio(disc_z1,df_z1)
hr_cluster_z2 <- hit.ratio(disc_z2,df_z2)
hr_cluster_z3 <- hit.ratio(disc_z3,df_z3)
hr_cluster_z4 <- hit.ratio(disc_z4,df_z4)
hr_cluster_m1 <- hit.ratio(disc_m1,df_m1)
hr_cluster_m2 <- hit.ratio(disc_m2,df_m2)
hr_cluster_m3 <- hit.ratio(disc_m3,df_m3)
hr_cluster_m4 <- hit.ratio(disc_m4,df_m4)

hit_ratios <- c(hr_cluster_z1,hr_cluster_z2,hr_cluster_z3,hr_cluster_z4,
                hr_cluster_m1,hr_cluster_m2,hr_cluster_m3,hr_cluster_m4)
clst_names <- c("hr_cluster_z1","hr_cluster_z2","hr_cluster_z3","hr_cluster_z4",
                "hr_cluster_m1","hr_cluster_m2","hr_cluster_m3","hr_cluster_m4")
k_solution <- c(tail(disc_z1$lev,n=1),tail(disc_z2$lev,n=1),tail(disc_z3$lev,n=1),tail(disc_z4$lev,n=1),
                tail(disc_m1$lev,n=1),tail(disc_m2$lev,n=1),tail(disc_m3$lev,n=1),tail(disc_m4$lev,n=1))
hit.ratios <- data.frame(clst_names,hit_ratios,k_solution) %>% 
  write_csv("data/analysis/hit_ratios.csv")

rm(hit_ratios,clst_names,id,m,z,hr_cluster_z1,hr_cluster_z2,hr_cluster_z3,hr_cluster_z4,
   hr_cluster_m1,hr_cluster_m2,hr_cluster_m3,hr_cluster_m4,df_z1,df_z2,df_z3,df_z4,df_m1,
   df_m2,df_m3,df_m4,disc_z1,disc_z2,disc_z3,disc_z4,disc_m1,disc_m2,disc_m3,disc_m4,k_solution,
   eig_z1,eig_z2,eig_z3,eig_z4,eig_m1,eig_m2,eig_m3,eig_m4)

# Map cluster assignments -------------------------------------------------
cbsa <- core_based_statistical_areas(cb = T) %>% 
  select(GEOID:geometry) %>% st_transform(crs = 2163) %>% 
  mutate(cbsa_fips = as.numeric(GEOID)) %>% 
  select(cbsa_fips,geometry) %>% 
  inner_join(.,cluster_assignments) %>% 
  arrange(cbsa_fips) %>% 
  st_centroid_xy()
cbsa48 <- cbsa %>% filter(!cbsa_fips %in% c(11260,21820,46520))
us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK","PR","HI")) %>% st_transform(crs = 2163)
clrsz <- rand_ncolors(cbsa48 %>% st_drop_geometry() %>% select(3) %>% distinct())
for (i in 01:length(clrsz)) {
  title <- sprintf("Cluster %s",i)
  ii <- str_pad(i,width=2, side="left", pad="0")
  plt <- ggplot() + 
    geom_sf(data = us, color = "gray60", fill = "gray90") +
    geom_point(data = cbsa48 %>% filter(.[[3]] == i), 
               aes(x,y), size = 2, color = clrsz[i]) +
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), 
          text = element_text(family = "IBM Plex Mono")) 
  nam <- paste0("clustr_map_z",ii)
  assign(nam, plt)
}
zlist_df = lapply(sprintf("clustr_map_z%s", str_pad(01:length(clrsz), 2, pad = "0")) , get)

clrsm <- rand_ncolors(cbsa48 %>% st_drop_geometry() %>% select(7) %>% distinct())
for (i in 01:length(clrsm)) {
  title <- sprintf("Cluster %s",i)
  ii <- str_pad(i,width=2, side="left", pad="0")
  plt <- ggplot() + 
    geom_sf(data = us, color = "gray60", fill = "gray90") +
    geom_point(data = cbsa48 %>% filter(.[[7]] == i), 
               aes(x,y), size = 2, color = clrsm[i]) +
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), 
          text = element_text(family = "IBM Plex Mono")) 
  nam <- paste0("clustr_map_m",ii)
  assign(nam, plt)
}
mlist_df = lapply(sprintf("clustr_map_m%s", str_pad(01:length(clrsm), 2, pad = "0")) , get)

z_best <- ggarrange(plotlist = zlist_df) +
  ggsave("plot/clustr_array_z.png", height = 10, width = 20)
m_best <- ggarrange(plotlist = mlist_df) +
  ggsave("plot/clustr_array_m.png", height = 10, width = 20)

rm(i,ii,nam,title,clrsz,clrsm,cbsa,us,cbsa48,plt,zlist_df,mlist_df,z_best,m_best,
   list = ls(pattern = "clustr_map_"))