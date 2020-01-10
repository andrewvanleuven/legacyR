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
df <- read_csv("data/master.csv")
id <- df %>% select(1:2)

df_00 <- df %>% select(read_csv("data/analysis/specifications/vars_00.csv") %>% pull()) 
df_01 <- df %>% select(read_csv("data/analysis/specifications/vars_01.csv") %>% pull()) 

clustr_assign <- function(data){
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
      filter(clusters <= 20)
  }
  solutions <- function(agg_schedule){
    agg_schedule %>% 
      mutate(rank = rank(-(abs(accel))),
             solution = clusters + 1,
             accel = round(accel,2)) %>% 
      arrange(rank) %>% 
      filter(rank <= 5) %>% 
      select(rank,accel,solution)
  }
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
  zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
  mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)
  hit_ratios <- function(data,cluster_assignments){
    h_rat <- function(discriminant,data){
      pred <- predict(discriminant)
      data$lda <- pred$class
      mtx <- table(data$cluster,data$lda)
      n <- sum(rowSums(mtx))
      preds <- sum(diag(mtx))
      (preds/n)*100
    }
    cdf <- cbind(cluster_assignments,data)
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
    hit_ratios <- c(h_rat(disc_z1,df_z1),h_rat(disc_z2,df_z2),h_rat(disc_z3,df_z3),
                    h_rat(disc_z4,df_z4),h_rat(disc_m1,df_m1),h_rat(disc_m2,df_m2),
                    h_rat(disc_m3,df_m3),h_rat(disc_m4,df_m4))
    clustr_type <- c(
      "z1","z2","z3","z4","m1","m2","m3","m4"
    )
    k_solution <- c(tail(disc_z1$lev,n=1),tail(disc_z2$lev,n=1),tail(disc_z3$lev,n=1),tail(disc_z4$lev,n=1),
                    tail(disc_m1$lev,n=1),tail(disc_m2$lev,n=1),tail(disc_m3$lev,n=1),tail(disc_m4$lev,n=1))
    data.frame(clustr_type,as.numeric(k_solution),hit_ratios) %>% 
      rename(k = as.numeric.k_solution.) %>% 
      mutate(hit_ratios = round(hit_ratios,1))
  }
  pct_var <- function(data,cluster_assignments){
    cdf <- cbind(cluster_assignments,data)
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
    pct_o_var <- function(eigen,type){
      eigen %>% enframe(name = NULL) %>% 
        mutate(pov = 100*(value^2/sum(value^2)),
               func = row_number(),
               clustr_type = type,
               eigenvalue = round(value,1),
               pct_of_variance = round(pov,2)) %>% 
        select(clustr_type,func,eigenvalue,pct_of_variance)
    }
    eig_z1 <- disc_z1$svd %>% pct_o_var(.,type = "z1")
    eig_z2 <- disc_z2$svd %>% pct_o_var(.,type = "z2")
    eig_z3 <- disc_z3$svd %>% pct_o_var(.,type = "z3")
    eig_z4 <- disc_z4$svd %>% pct_o_var(.,type = "z4")
    eig_m1 <- disc_m1$svd %>% pct_o_var(.,type = "m1")
    eig_m2 <- disc_m2$svd %>% pct_o_var(.,type = "m2")
    eig_m3 <- disc_m3$svd %>% pct_o_var(.,type = "m3")
    eig_m4 <- disc_m4$svd %>% pct_o_var(.,type = "m4")
    rbind(eig_z1,eig_z2,eig_z3,eig_z4,eig_m1,eig_m2,eig_m3,eig_m4)
  }
  counts <- function(data,cluster_assignments){
    cdf <- cbind(cluster_assignments,data)
    rbind(cdf %>% group_by_at(03) %>% summarise(N=n()) %>% mutate(clustr_type = "z1") %>% rename(cluster = 1),
          cdf %>% group_by_at(04) %>% summarise(N=n()) %>% mutate(clustr_type = "z2") %>% rename(cluster = 1),
          cdf %>% group_by_at(05) %>% summarise(N=n()) %>% mutate(clustr_type = "z3") %>% rename(cluster = 1),
          cdf %>% group_by_at(06) %>% summarise(N=n()) %>% mutate(clustr_type = "z4") %>% rename(cluster = 1),
          cdf %>% group_by_at(07) %>% summarise(N=n()) %>% mutate(clustr_type = "m1") %>% rename(cluster = 1),
          cdf %>% group_by_at(08) %>% summarise(N=n()) %>% mutate(clustr_type = "m2") %>% rename(cluster = 1),
          cdf %>% group_by_at(09) %>% summarise(N=n()) %>% mutate(clustr_type = "m3") %>% rename(cluster = 1),
          cdf %>% group_by_at(10) %>% summarise(N=n()) %>% mutate(clustr_type = "m4") %>% rename(cluster = 1)) %>% 
      select(clustr_type,cluster,N)
  }
  corr <- as.data.frame(cor(data, use="pairwise.complete.obs")) %>% 
    mutate_if(is.numeric, round, 2) %>% 
    rownames_to_column(.,"variable")
  corr.tri <- cor(data)
  corr.tri[lower.tri(cor(data), diag=TRUE)]<-""
  corr_pairs <- as.data.frame(corr.tri) %>% 
    rownames_to_column(.,"variable") %>% 
    pivot_longer(-variable,names_to = "variable2",values_to = "correlation") %>% 
    filter(correlation != "") %>% 
    mutate(correlation = round(as.numeric(as.character(correlation)),3)) %>% 
    arrange(desc(abs(correlation)))
  z <- data %>% mutate_if(Negate(is.binary),zscore)
  m <- data %>% mutate_if(Negate(is.binary),mscore)
  zsols <- solutions(agg_sched(clustr(z))) %>% mutate(method = "z") 
  msols <- solutions(agg_sched(clustr(m))) %>% mutate(method = "m") 
  agg_sched <- rbind(zsols,msols)
  cluster_assignments <- cbind(clustassign(z,zsols[1,3],id_cols = id,needs_id = T,method = "z"),
                               clustassign(z,zsols[2,3],id_cols = id,method = "z"),
                               clustassign(z,zsols[3,3],id_cols = id,method = "z"),
                               clustassign(z,zsols[4,3],id_cols = id,method = "z"),
                               clustassign(m,msols[1,3],id_cols = id,method = "m"),
                               clustassign(m,msols[2,3],id_cols = id,method = "m"),
                               clustassign(m,msols[3,3],id_cols = id,method = "m"),
                               clustassign(m,msols[4,3],id_cols = id,method = "m"))
  hrs <- hit_ratios(data,cluster_assignments)
  p_var <- pct_var(data,cluster_assignments)
  counts <- counts(data,cluster_assignments)
  print(zsols)
  print(msols)
  print(hrs)
  list(vars = colnames(data) %>% enframe(name = NULL) %>% rename(specified_vars = value),
       agg_sched = agg_sched %>% select(-rank), 
       corr = corr, 
       corr_pairs = corr_pairs,
       cluster_assignments = cluster_assignments,
       hit_ratios = hrs,
       pct_of_var = p_var,
       counts = counts)
}

write_xlsx(clustr_assign(df_00), "data/analysis/base_specify.xlsx")
write_xlsx(clustr_assign(df_01), "data/analysis/specify_01.xlsx")
