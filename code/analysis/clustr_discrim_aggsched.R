library(tidyverse)
library(rleuven)

# Get Data ----------------------------------------------------------------
df_rnr <- read_csv("data/master_rnr.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df_rnr %>% select(1:2)
df <- df_rnr %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull()) 

# Functions ---------------------------------------------------------------
is.binary <- function(j) {
  x <- unique(j)
  length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)
}
clustr <- function(scaled_data) {
  d <- dist(scaled_data, method = "euclidean")
  hclust(d, method = "ward.D2")
}
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
    filter(clusters <= 25)
}
zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)
clustassign <- function(scaled_data,num_k,id_cols,method = NULL,needs_id = NULL){
  if(is.null(method)) {
    clust <- paste0("cluster_",num_k)
  } else {
    clust <- paste0("cluster",method,"_",num_k)
  }
  c <- clustr(scaled_data)
  cluster <- cutree(c, k = num_k)
  standalone <- cbind(id_cols,cluster) %>% rename(., !!clust := cluster)
  if(is.null(needs_id)) {
    assignment <- standalone %>% select(3)
  } else {
    assignment <- standalone
  }
  assignment
}
z <- df %>% mutate_if(Negate(is.binary),zscore)
m <- df %>% mutate_if(Negate(is.binary),mscore)

z_aggsched <- agg_sched(clustr(z)) %>% write_csv("data/z_aggsched.csv")
m_aggsched <- agg_sched(clustr(m)) %>% write_csv("data/m_aggsched.csv")

plot(clustr(m))
k3 <- cutree(clustr(m), k = 3) %>% as_tibble() %>% rename(cluster3 = value)
k7 <- cutree(clustr(m), k = 7) %>% as_tibble() %>% rename(cluster7 = value)
k14 <- cutree(clustr(m), k = 14) %>% as_tibble() %>% rename(cluster14 = value)
k_vals <- cbind(id,k3,k7,k14) %>% write_csv("data/assign_k3.csv")
