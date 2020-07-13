library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

df <- read_csv("data/master.csv")

mscore_tables <- function(dataset, var, full=c('TRUE','FALSE')) {
  if(missing(full)) {
    full=F}
  v <- dataset %>% pull(all_of(var))
  m1 <- mean(v)
  m2 <- median(v)
  m3 <- (m1-m2)/m2
  m4 <- sd(v)
  m5 <- (IQR(v))/1.349
  m6 <- (m4-m5)/m5
  results <- data.frame(m1,m2,m3,m4,m5,m6) %>% 
    as_tibble() %>% 
    rename(mean = m1,median = m2,pct_diff_mean_median = m3,
           sd = m4,psd = m5,pct_diff_sd_psd = m6) %>% 
    mutate(variable = var) %>% 
    select(7,1,2,4,5,3,6)
  print(results %>% select(1:5))
  if (full==TRUE) return({results})
  if (full==FALSE) return({results %>% select(-pct_diff_sd_psd,-pct_diff_mean_median)})
}

pop <- mscore_tables(df,"population_2005",full = T)
irs <- mscore_tables(df,"chartbl_per_cap",full = T)

comparison <- bind_rows(pop,irs) %>% 
  write_csv("data/analysis/mscore_comparison.csv")
