library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
library(writexl)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Read in data ------------------------------------------------------------
#df <- read_csv("data/master.csv") 
#id <- df %>% select(1:2)

# Cluster-Discriminant Function -------------------------------------------
clustr_assign <- dget("code/functions/clustr_assign.R")

# Base specification and others -------------------------------------------
#df_00 <- df %>% select(read_csv("data/analysis/specifications/vars_00.csv") %>% pull()) 
#df_01 <- df %>% select(read_csv("data/analysis/specifications/vars_01.csv") %>% pull())  
#write_xlsx(clustr_assign(df_00), "data/analysis/results/base_specify.xlsx")
#write_xlsx(clustr_assign(df_01), "data/analysis/results/specify_01.xlsx")

# Subset data to be only lower 48 -----------------------------------------
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)

#df_02 <- df %>% select(read_csv("data/analysis/specifications/vars_01.csv") %>% pull())  
df_03 <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  
#df_04 <- df %>% select(read_csv("data/analysis/specifications/vars_03.csv") %>% pull())  

#write_xlsx(clustr_assign(df_02), "data/analysis/results/specify_02.xlsx")
#write_xlsx(clustr_assign(df_03,clust_solution = 5), "data/analysis/results/specify_03.xlsx")
#write_xlsx(clustr_assign(df_04), "data/analysis/results/specify_04.xlsx") #superfund var. as raw value
spss <- clustr_assign(df_03,clust_solution = 5)
#write_csv(spss$combined,"/Users/andrew/SPSS/spss_input.csv")

# Significance of Centroid Scores -----------------------------------------
tstat <- function(x, na.rm = F) mean(x)/sd(x)#/(sqrt(n()))
cdf <- spss$combined
df_disc <- cdf %>% select(everything(),-(1:10), 7) %>% 
  select(tail(names(.), 1),everything()) %>% rename(cluster = 1)
discrim <- MASS::lda(cluster~., data = df_disc)
scores <- predict(discrim)$x %>% as_tibble() %>% 
  cbind(df_disc[1],.) %>% 
  group_by(cluster) %>%
  summarise_all(mean)

freq <- freqTab(df_disc,"cluster",Inf) %>% 
  arrange(CLUSTER) %>% 
  mutate(cluster = as.integer(CLUSTER)) %>% 
  select(cluster,N)

score_bugs <- predict(discrim)$x %>% as_tibble() %>% 
  cbind(df_disc[1],.) %>% 
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
         sig2_01 = if_else(((abs(LD1)>=abs(sig95))&(abs(LD1)<abs(sig99))),"*",""),
         sig2_02 = if_else(((abs(LD2)>=abs(sig95))&(abs(LD2)<abs(sig99))),"*",""),
         sig2_03 = if_else(((abs(LD3)>=abs(sig95))&(abs(LD3)<abs(sig99))),"*",""),
         sig2_04 = if_else(((abs(LD4)>=abs(sig95))&(abs(LD4)<abs(sig99))),"*",""),
         sig2_05 = if_else(((abs(LD5)>=abs(sig95))&(abs(LD5)<abs(sig99))),"*",""),
         sig2_06 = if_else(((abs(LD6)>=abs(sig95))&(abs(LD6)<abs(sig99))),"*",""),
         sig2_07 = if_else(((abs(LD7)>=abs(sig95))&(abs(LD7)<abs(sig99))),"*",""),
         sig2_08 = if_else(((abs(LD8)>=abs(sig95))&(abs(LD8)<abs(sig99))),"*",""),
         sig2_09 = if_else(((abs(LD9)>=abs(sig95))&(abs(LD9)<abs(sig99))),"*",""),
         sig3_01 = if_else(((abs(LD1)>=abs(sig90))),"*",""),
         sig3_02 = if_else(((abs(LD2)>=abs(sig90))),"*",""),
         sig3_03 = if_else(((abs(LD3)>=abs(sig90))),"*",""),
         sig3_04 = if_else(((abs(LD4)>=abs(sig90))),"*",""),
         sig3_05 = if_else(((abs(LD5)>=abs(sig90))),"*",""),
         sig3_06 = if_else(((abs(LD6)>=abs(sig90))),"*",""),
         sig3_07 = if_else(((abs(LD7)>=abs(sig90))),"*",""),
         sig3_08 = if_else(((abs(LD8)>=abs(sig90))),"*",""),
         sig3_09 = if_else(((abs(LD9)>=abs(sig90))),"*",""),
         bug1 = paste0(sig1_01,sig2_01,sig3_01),
         bug2 = paste0(sig1_02,sig2_02,sig3_02),
         bug3 = paste0(sig1_03,sig2_03,sig3_03),
         bug4 = paste0(sig1_04,sig2_04,sig3_04),
         bug5 = paste0(sig1_05,sig2_05,sig3_05),
         bug6 = paste0(sig1_06,sig2_06,sig3_06),
         bug7 = paste0(sig1_07,sig2_07,sig3_07),
         bug8 = paste0(sig1_08,sig2_08,sig3_08),
         bug9 = paste0(sig1_09,sig2_09,sig3_09)) %>% 
  select(bug1,
         bug2,
         bug3,
         bug4,
         bug5,
         bug6,
         bug7,
         bug8,
         bug9)

centroids <- scores %>% 
  mutate_at(vars(-cluster), list(~ round(., 3))) %>% 
  cbind(.,score_bugs) %>% 
  mutate(centroid_01 = paste0(LD1,bug1),
         centroid_02 = paste0(LD2,bug2),
         centroid_03 = paste0(LD3,bug3),
         centroid_04 = paste0(LD4,bug4),
         centroid_05 = paste0(LD5,bug5),
         centroid_06 = paste0(LD6,bug6),
         centroid_07 = paste0(LD7,bug7),
         centroid_08 = paste0(LD8,bug8),
         centroid_09 = paste0(LD9,bug9)) %>% 
  select(cluster,
         centroid_01,
         centroid_02,
         centroid_03,
         centroid_04,
         centroid_05,
         centroid_06,
         centroid_07,
         centroid_08,
         centroid_09) %>% 
  write_csv("data/analysis/results/centroids.csv")

# Centroids for K = 18 ----------------------------------------------------
df_disc18 <- cdf %>% select(everything(),-(1:10), 8) %>% 
  select(tail(names(.), 1),everything()) %>% rename(cluster = 1)
discrim18 <- MASS::lda(cluster~., data = df_disc18)
scores18 <- predict(discrim18)$x %>% as_tibble() %>% 
  cbind(df_disc18[1],.) %>% 
  group_by(cluster) %>%
  summarise_all(mean)

freq18 <- freqTab(df_disc18,"cluster",Inf) %>% 
  arrange(CLUSTER) %>% 
  mutate(cluster = as.integer(CLUSTER)) %>% 
  select(cluster,N)

score_bugs18 <- predict(discrim18)$x %>% as_tibble() %>% 
  cbind(df_disc18[1],.) %>% 
  group_by(cluster) %>%
  summarise_all(tstat) %>% 
  left_join(.,freq18) %>% 
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
       sig1_11 = if_else(((abs(LD11)>=abs(sig90))&(abs(LD11)<abs(sig95))),"*",""),
       sig1_12 = if_else(((abs(LD12)>=abs(sig90))&(abs(LD12)<abs(sig95))),"*",""),
       sig1_13 = if_else(((abs(LD13)>=abs(sig90))&(abs(LD13)<abs(sig95))),"*",""),
       sig1_14 = if_else(((abs(LD14)>=abs(sig90))&(abs(LD14)<abs(sig95))),"*",""),
       sig1_15 = if_else(((abs(LD15)>=abs(sig90))&(abs(LD15)<abs(sig95))),"*",""),
       sig1_16 = if_else(((abs(LD16)>=abs(sig90))&(abs(LD16)<abs(sig95))),"*",""),
       sig1_17 = if_else(((abs(LD17)>=abs(sig90))&(abs(LD17)<abs(sig95))),"*",""),
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
       sig2_11 = if_else(((abs(LD11)>=abs(sig95))&(abs(LD11)<abs(sig99))),"*",""),
       sig2_12 = if_else(((abs(LD12)>=abs(sig95))&(abs(LD12)<abs(sig99))),"*",""),
       sig2_13 = if_else(((abs(LD13)>=abs(sig95))&(abs(LD13)<abs(sig99))),"*",""),
       sig2_14 = if_else(((abs(LD14)>=abs(sig95))&(abs(LD14)<abs(sig99))),"*",""),
       sig2_15 = if_else(((abs(LD15)>=abs(sig95))&(abs(LD15)<abs(sig99))),"*",""),
       sig2_16 = if_else(((abs(LD16)>=abs(sig95))&(abs(LD16)<abs(sig99))),"*",""),
       sig2_17 = if_else(((abs(LD17)>=abs(sig95))&(abs(LD17)<abs(sig99))),"*",""),
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
       sig3_11 = if_else(((abs(LD11)>=abs(sig90))),"*",""),
       sig3_12 = if_else(((abs(LD12)>=abs(sig90))),"*",""),
       sig3_13 = if_else(((abs(LD13)>=abs(sig90))),"*",""),
       sig3_14 = if_else(((abs(LD14)>=abs(sig90))),"*",""),
       sig3_15 = if_else(((abs(LD15)>=abs(sig90))),"*",""),
       sig3_16 = if_else(((abs(LD16)>=abs(sig90))),"*",""),
       sig3_17 = if_else(((abs(LD17)>=abs(sig90))),"*",""),
       bug1 = paste0(sig1_01,sig2_01,sig3_01),
       bug2 = paste0(sig1_02,sig2_02,sig3_02),
       bug3 = paste0(sig1_03,sig2_03,sig3_03),
       bug4 = paste0(sig1_04,sig2_04,sig3_04),
       bug5 = paste0(sig1_05,sig2_05,sig3_05),
       bug6 = paste0(sig1_06,sig2_06,sig3_06),
       bug7 = paste0(sig1_07,sig2_07,sig3_07),
       bug8 = paste0(sig1_08,sig2_08,sig3_08),
       bug9 = paste0(sig1_09,sig2_09,sig3_09),
       bug10 = paste0(sig1_10,sig2_10,sig3_10),
       bug11 = paste0(sig1_11,sig2_11,sig3_11),
       bug12 = paste0(sig1_12,sig2_12,sig3_12),
       bug13 = paste0(sig1_13,sig2_13,sig3_13),
       bug14 = paste0(sig1_14,sig2_14,sig3_14),
       bug15 = paste0(sig1_15,sig2_15,sig3_15),
       bug16 = paste0(sig1_16,sig2_16,sig3_16),
       bug17 = paste0(sig1_17,sig2_17,sig3_17)
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
         bug10,
         bug11,
         bug12,
         bug13,
         bug14,
         bug15,
         bug16,
         bug17
  )

centroids18 <- scores18 %>% 
  mutate_at(vars(-cluster), list(~ round(., 3))) %>% 
  cbind(.,score_bugs18) %>% 
  mutate(centroid_01 = paste0(LD1,bug1),
         centroid_02 = paste0(LD2,bug2),
         centroid_03 = paste0(LD3,bug3),
         centroid_04 = paste0(LD4,bug4),
         centroid_05 = paste0(LD5,bug5),
         centroid_06 = paste0(LD6,bug6),
         centroid_07 = paste0(LD7,bug7),
         centroid_08 = paste0(LD8,bug8),
         centroid_09 = paste0(LD9,bug9),
         centroid_10 = paste0(LD10,bug10),
         centroid_11 = paste0(LD11,bug11),
         centroid_12 = paste0(LD12,bug12),
         centroid_13 = paste0(LD13,bug13),
         centroid_14 = paste0(LD14,bug14),
         centroid_15 = paste0(LD15,bug15),
         centroid_16 = paste0(LD16,bug16),
         centroid_17 = paste0(LD17,bug17)
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
         centroid_10,
         centroid_11,
         centroid_12,
         centroid_13,
         centroid_14,
         centroid_15,
         centroid_16,
         centroid_17
  ) %>% 
  mutate_all(list(~str_replace(., "NANANA", ""))) %>% 
  write_csv("data/analysis/results/centroids18.csv")
