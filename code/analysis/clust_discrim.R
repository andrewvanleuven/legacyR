library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)
# Data --------------------------------------------------------------------
charitable <- read_csv("data/base/charitable.csv") %>% 
  select(1,3) %>% rename(ID = cbsa_fips)
df <- read_csv("data/clustrdata.csv") %>% 
  mutate(DECL_SNC_PEAK = abs(DECL_SNC_PEAK)) %>% 
  left_join(.,charitable) %>% select(-CHRASTS) %>% 
  mutate(CHRASTS = charitable_assets/POP_EST_15)
id <- df %>% select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id)
vn <- read_csv("data/varnames.csv") %>% pull(varnames)

# Functions ---------------------------------------------------------------
clustr.subset <- function(data, ...) {
  data %>% select(...)}
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

# Clustering ---------------------------------------------------------------
dfs <- clustr.subset(df,vn) %>% 
  select(#-TRANSP,
         #-H2O_VIOL,
         -PCT_SMOKE,
         #-LAND_USE_MIX,
         -HSNG_STOCK_AGE,
         -BELOW_DIPL,
         #-CHRASTS
         ) %>% 
  rename_all(tolower)
rm(df,charitable)
zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)
adj_vars <- dfs %>% select(r1:st_cap) #%>% mutate(r1 = r1/7)
z <- dfs %>% select(-(r1:st_cap)) %>% 
  mutate_all(.,zscore) %>% 
  cbind(.,adj_vars) %>% 
  select(1:2, r1:st_cap, everything())
m <- dfs %>% select(-(r1:st_cap)) %>% 
  mutate_all(.,mscore) %>% 
  cbind(.,adj_vars) %>% 
  select(1:2, r1:st_cap, everything())
#agg_sched(clustr(z)) %>% write_csv("data/agg_sched_z.csv")
#agg_sched(clustr(m)) %>% write_csv("data/agg_sched_m.csv")
zsols <- solutions(agg_sched(clustr(z))) # Ideal K: 15 and 5
zsols
msols <- solutions(agg_sched(clustr(m))) # Ideal K: 16 and 6 (and 13)
msols

clustr_z1 <- clustr.assign(z,zsols[1,3],method = "z")
clustr_z2 <- clustr.assign(z,zsols[2,3],method = "z") %>% select(3)
clustr_z3 <- clustr.assign(z,zsols[3,3],method = "z") %>% select(3)
clustr_z4 <- clustr.assign(z,zsols[4,3],method = "z") %>% select(3)
clustr_m1 <- clustr.assign(m,zsols[1,3],method = "m") %>% select(3)
clustr_m2 <- clustr.assign(m,zsols[2,3],method = "m") %>% select(3)
clustr_m3 <- clustr.assign(m,zsols[3,3],method = "m") %>% select(3)
clustr_m4 <- clustr.assign(m,zsols[4,3],method = "m") %>% select(3)

cluster_assignments <- cbind(clustr_z1,clustr_z2,clustr_z3,clustr_z4,clustr_m1,clustr_m2,clustr_m3,clustr_m4) %>% 
  write_csv("data/cluster_assignments.csv")

rm(clustr_z1,clustr_z2,clustr_z3,clustr_z4,clustr_m1,clustr_m2,clustr_m3,clustr_m4,adj_vars)

# Discriminant HRs---------------------------------------------------------
cdf <- cbind(cluster_assignments,dfs) %>% write_csv("data/clustering.csv")

df_z1 <- cdf %>% select(03,age:immigrant) %>% rename(cluster = 1)
df_z2 <- cdf %>% select(04,age:immigrant) %>% rename(cluster = 1)
df_z3 <- cdf %>% select(05,age:immigrant) %>% rename(cluster = 1)
df_z4 <- cdf %>% select(06,age:immigrant) %>% rename(cluster = 1)
df_m1 <- cdf %>% select(07,age:immigrant) %>% rename(cluster = 1)
df_m2 <- cdf %>% select(08,age:immigrant) %>% rename(cluster = 1)
df_m3 <- cdf %>% select(09,age:immigrant) %>% rename(cluster = 1)
df_m4 <- cdf %>% select(10,age:immigrant) %>% rename(cluster = 1)

disc_z1 <- MASS::lda(cluster~., data = df_z1)
disc_z2 <- MASS::lda(cluster~., data = df_z2)
disc_z3 <- MASS::lda(cluster~., data = df_z3)
disc_z4 <- MASS::lda(cluster~., data = df_z4)
disc_m1 <- MASS::lda(cluster~., data = df_m1)
disc_m2 <- MASS::lda(cluster~., data = df_m2)
disc_m3 <- MASS::lda(cluster~., data = df_m3)
disc_m4 <- MASS::lda(cluster~., data = df_m4)

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
  write_csv("data/hit_ratios.csv")

rm(hit_ratios,clst_names,id,m,z,vn,hr_cluster_z1,hr_cluster_z2,hr_cluster_z3,hr_cluster_z4,
   hr_cluster_m1,hr_cluster_m2,hr_cluster_m3,hr_cluster_m4,df_z1,df_z2,df_z3,df_z4,df_m1,
   df_m2,df_m3,df_m4,disc_z1,disc_z2,disc_z3,disc_z4,disc_m1,disc_m2,disc_m3,disc_m4,k_solution)

# Centroid Scores ---------------------------------------------------------
tstat <- function(x, na.rm = F) mean(x)/sd(x)#/(sqrt(n()))
df_m16 <- cdf %>% select(cluster_11,age:immigrant) %>% rename(cluster = cluster_11)
discrim <- MASS::lda(cluster~., data = df_z11)
freq <- freqTab(df_z11,"cluster",Inf) %>% 
  arrange(CLUSTER) %>% 
  mutate(cluster = as.integer(CLUSTER)) %>% 
  select(cluster,N) %>% write_csv("data/freq_table.csv")
scores <- predict(discrim)$x %>% as_tibble() %>% 
  cbind(df_z11[1],.) %>% 
  group_by(cluster) %>%
  summarise_all(tstat) %>% 
  left_join(.,freq) %>% 
  mutate(sig90 = qt(.1,N),
         sig95 = qt(.05,N),
         sig99 = qt(.01,N),
         sig1_01 = if_else(((abs(LD1)>=abs(sig90))&(abs(LD1)<abs(sig95))),"*",""),
         sig1_02 = if_else(((abs(LD2)>=abs(sig90))&(abs(LD2)<abs(sig95))),"*",""),
         sig1_03 = if_else(((abs(LD3)>=abs(sig90))&(abs(LD3)<abs(sig95))),"*",""),
         sig1_04 = if_else(((abs(LD4)>=abs(sig90))&(abs(LD4)<abs(sig95))),"*",""),
         sig1_05 = if_else(((abs(LD5)>=abs(sig90))&(abs(LD5)<abs(sig95))),"*",""),
         sig1_06 = if_else(((abs(LD6)>=abs(sig90))&(abs(LD6)<abs(sig95))),"*",""),
         sig1_07 = if_else(((abs(LD7)>=abs(sig90))&(abs(LD7)<abs(sig95))),"*",""),
         sig1_08 = if_else(((abs(LD8)>=abs(sig90))&(abs(LD8)<abs(sig95))),"*",""),
         sig1_09 = if_else(((abs(LD9)>=abs(sig90))&(abs(LD9)<abs(sig95))),"*",""),
         sig1_10 = if_else(((abs(LD10)>=abs(sig90))&(abs(LD10)<abs(sig95))),"*",""),
         #sig1_11 = if_else(((abs(LD11)>=abs(sig90))&(abs(LD11)<abs(sig95))),"*",""),
         #sig1_12 = if_else(((abs(LD12)>=abs(sig90))&(abs(LD12)<abs(sig95))),"*",""),
         #sig1_13 = if_else(((abs(LD13)>=abs(sig90))&(abs(LD13)<abs(sig95))),"*",""),
         #sig1_14 = if_else(((abs(LD14)>=abs(sig90))&(abs(LD14)<abs(sig95))),"*",""),
         #sig1_15 = if_else(((abs(LD15)>=abs(sig90))&(abs(LD15)<abs(sig95))),"*",""),
         sig2_01 = if_else(((abs(LD1)>=abs(sig95))&(abs(LD1)<abs(sig99))),"*",""),
         sig2_02 = if_else(((abs(LD2)>=abs(sig95))&(abs(LD2)<abs(sig99))),"*",""),
         sig2_03 = if_else(((abs(LD3)>=abs(sig95))&(abs(LD3)<abs(sig99))),"*",""),
         sig2_04 = if_else(((abs(LD4)>=abs(sig95))&(abs(LD4)<abs(sig99))),"*",""),
         sig2_05 = if_else(((abs(LD5)>=abs(sig95))&(abs(LD5)<abs(sig99))),"*",""),
         sig2_06 = if_else(((abs(LD6)>=abs(sig95))&(abs(LD6)<abs(sig99))),"*",""),
         sig2_07 = if_else(((abs(LD7)>=abs(sig95))&(abs(LD7)<abs(sig99))),"*",""),
         sig2_08 = if_else(((abs(LD8)>=abs(sig95))&(abs(LD8)<abs(sig99))),"*",""),
         sig2_09 = if_else(((abs(LD9)>=abs(sig95))&(abs(LD9)<abs(sig99))),"*",""),
         sig2_10 = if_else(((abs(LD10)>=abs(sig95))&(abs(LD10)<abs(sig99))),"*",""),
         #sig2_11 = if_else(((abs(LD11)>=abs(sig95))&(abs(LD11)<abs(sig99))),"*",""),
         #sig2_12 = if_else(((abs(LD12)>=abs(sig95))&(abs(LD12)<abs(sig99))),"*",""),
         #sig2_13 = if_else(((abs(LD13)>=abs(sig95))&(abs(LD13)<abs(sig99))),"*",""),
         #sig2_14 = if_else(((abs(LD14)>=abs(sig95))&(abs(LD14)<abs(sig99))),"*",""),
         #sig2_15 = if_else(((abs(LD15)>=abs(sig95))&(abs(LD15)<abs(sig99))),"*",""),
         sig3_01 = if_else(((abs(LD1)>=abs(sig90))),"*",""),
         sig3_02 = if_else(((abs(LD2)>=abs(sig90))),"*",""),
         sig3_03 = if_else(((abs(LD3)>=abs(sig90))),"*",""),
         sig3_04 = if_else(((abs(LD4)>=abs(sig90))),"*",""),
         sig3_05 = if_else(((abs(LD5)>=abs(sig90))),"*",""),
         sig3_06 = if_else(((abs(LD6)>=abs(sig90))),"*",""),
         sig3_07 = if_else(((abs(LD7)>=abs(sig90))),"*",""),
         sig3_08 = if_else(((abs(LD8)>=abs(sig90))),"*",""),
         sig3_09 = if_else(((abs(LD9)>=abs(sig90))),"*",""),
         sig3_10 = if_else(((abs(LD10)>=abs(sig90))),"*",""),
         #sig3_11 = if_else(((abs(LD11)>=abs(sig90))),"*",""),
         #sig3_12 = if_else(((abs(LD12)>=abs(sig90))),"*",""),
         #sig3_13 = if_else(((abs(LD13)>=abs(sig90))),"*",""),
         #sig3_14 = if_else(((abs(LD14)>=abs(sig90))),"*",""),
         #sig3_15 = if_else(((abs(LD15)>=abs(sig90))),"*",""),
         bug1 = paste0(sig1_01,sig2_01,sig3_01),
         bug2 = paste0(sig1_02,sig2_02,sig3_02),
         bug3 = paste0(sig1_03,sig2_03,sig3_03),
         bug4 = paste0(sig1_04,sig2_04,sig3_04),
         bug5 = paste0(sig1_05,sig2_05,sig3_05),
         bug6 = paste0(sig1_06,sig2_06,sig3_06),
         bug7 = paste0(sig1_07,sig2_07,sig3_07),
         bug8 = paste0(sig1_08,sig2_08,sig3_08),
         bug9 = paste0(sig1_09,sig2_09,sig3_09),
         bug10 = paste0(sig1_10,sig2_10,sig3_10)
         #bug11 = paste0(sig1_11,sig2_11,sig3_11),
         #bug12 = paste0(sig1_12,sig2_12,sig3_12),
         #bug13 = paste0(sig1_13,sig2_13,sig3_13),
         #bug14 = paste0(sig1_14,sig2_14,sig3_14),
         #bug15 = paste0(sig1_15,sig2_15,sig3_15)
         ) %>% 
  select(bug1,
         bug2,
         bug3,
         bug4,
         bug5,
         bug6,
         bug7,
         bug8,
         bug9,
         bug10 
         #bug11,
         #bug12,
         #bug13,
         #bug14,
         #bug15
         ) 
centroids <- discrim.means(df_m16,discrim) %>% 
  rename(cluster = clustr_grp) %>% 
  mutate_at(vars(-cluster), list(~ round(., 3))) %>% 
  cbind(.,scores) %>% 
  mutate(centroid_01 = paste0(LD1,bug1),
         centroid_02 = paste0(LD2,bug2),
         centroid_03 = paste0(LD3,bug3),
         centroid_04 = paste0(LD4,bug4),
         centroid_05 = paste0(LD5,bug5),
         centroid_06 = paste0(LD6,bug6),
         centroid_07 = paste0(LD7,bug7),
         centroid_08 = paste0(LD8,bug8),
         centroid_09 = paste0(LD9,bug9),
         centroid_10 = paste0(LD10,bug10)
         #centroid_11 = paste0(LD11,bug11),
         #centroid_12 = paste0(LD12,bug12),
         #centroid_13 = paste0(LD13,bug13),
         #centroid_14 = paste0(LD14,bug14),
         #centroid_15 = paste0(LD15,bug15)
         ) %>% 
  select(cluster,
         centroid_01,
         centroid_02,
         centroid_03,
         centroid_04,
         centroid_05,
         centroid_06,
         centroid_07,
         centroid_08,
         centroid_09,
         centroid_10
         #centroid_11,
         #centroid_12,
         #centroid_13,
         #centroid_14,
         #centroid_15
         ) %>% 
  mutate_all(list(~str_replace(., "NANANA", " "))) %>% 
  write_csv("data/centroids.csv")

# Map Cluster Assignments -------------------------------------------------
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
clrs <- rand_ncolors(cbsa %>% group_by(cluster_11))
for (i in 01:11) {
  title <- sprintf("Cluster %s",i)
  ii <- str_pad(i,width=2, side="left", pad="0")
  plt <- ggplot() + 
    geom_sf(data = us, color = "gray60", fill = "gray90") +
    geom_point(data = cbsa48 %>% filter(cluster_11 == i), 
               aes(x,y), size = 2, color = "black") +#clrs[i]) +
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), 
          text = element_text(family = "IBM Plex Mono")) 
  nam <- paste0("clustr",ii)
  assign(nam, plt)
}
ggarrange(clustr01,clustr02,clustr03,clustr04,clustr05,clustr06,clustr07,clustr08,
          clustr09,clustr10,clustr11,#clustr12,clustr13,clustr14,clustr15,clustr16,
          ncol = 4, nrow = 3) +
  ggsave("plot/clustr_array_11.png", height = 10, width = 20)

rm(clustr01,clustr02,clustr03,clustr04,clustr05,clustr06,clustr07,clustr08,
   clustr09,clustr10,clustr11,#clustr12,clustr13,clustr14,#clustr15,clustr16,
   i,ii,nam,title,clrs,cbsa,us,cbsa48,plt)
