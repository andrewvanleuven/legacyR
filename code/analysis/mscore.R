library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(scales)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)


# Read in Data ------------------------------------------------------------
clustr_assign <- dget("code/functions/clustr_assign.R")
zscore <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
mscore <- function(x, na.rm = F) (x-median(x, na.rm = na.rm))/(IQR(x, na.rm = na.rm)/1.349)
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)
df_spec <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  
clusters <- (clustr_assign(df_spec))$combined %>% 
  select(1,2,7)
rm(df_spec,id,clustr_assign) 
df_fake = data.frame(person = as.numeric(c(NA, NA, NA)),
                 variable = c("variable1", "variable2", "variable3"),
                 value = as.numeric(c(NA, NA, NA)))
df_fake2 = data.frame(person = as.numeric(c(NA, NA)),
                 variable = c("variable1", "variable2"),
                 value = as.numeric(c(NA, NA)))
clr_cons()

# Tables ------------------------------------------------------------------
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

comparison <- bind_rows(pop,irs)# %>% 
  #write_csv("data/analysis/mscore_comparison.csv")
rm(pop,irs,mscore_tables,comparison)
# M-Score Viz -------------------------------------------------------------

df1 <- read_csv("data/analysis/mscore/pkincome_2015.csv") %>% 
  filter(cbsa_fips > 1000) %>% rename(pki = percapita_income_2015)
mean(df1$pki)
sd(df1$pki)

ggplot(df1) +
  aes(x = pki) +
  geom_histogram(bins = 100L, fill = "gray90", color = "black", size = .2) +
  geom_vline(xintercept = mean(df1$pki), color = "red", size = 1) +
  geom_vline(xintercept = median(df1$pki), color = "blue", size = 1) +
  geom_point(data = df_fake2, aes(x = person, y = value, color = variable), 
             size=8) +
  labs(x = "Per-capita Personal Income", 
       y = "Number of Observations", 
       title = "Per-capita Personal Income in US Metropolitan Areas", 
       #subtitle = "",
       color = "",
       caption = "Source: BEA, 2015") +
  scale_x_continuous(labels = dollar) +
  scale_color_manual(values = c('red','blue'),
                     labels = c('Mean','Median')) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text=element_text(family="Futura Medium"),
        legend.key = element_rect(size = 6, fill = NA, color = NA),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm")) +
  ggsave("plot/mscore/msa_inc_dist.png", height = 8.5, width = 11)

#ggplot() +
#  geom_vline(xintercept = 0, color = "black", size = .4) +
#  geom_hline(yintercept = 0, color = "black", size = .4) +
#  geom_density(data = df, aes(x = charitable_assets),fill = "black", 
#               color = NA, alpha = .3) +
#  #geom_histogram(data = df, aes(x = population_2005),
#  #               bins = 200L, fill = "gray90", color = "black", size = .2) +
#  geom_vline(xintercept = mean(df$charitable_assets), color = "red", size = 1) +
#  geom_vline(xintercept = median(df$charitable_assets), color = "blue", size = 1) +
#  geom_point(data = df_fake2, aes(x = person, y = value, color = variable), 
#             size=8) +
#  labs(x = "Population", 
#       y = "", 
#       title = "Total Assets of Non-Profit Charitable Foundations in US Metropolitan Areas", 
#       subtitle = "Truncated to Exclude MSAs with Assets > $25 Million",
#       caption = "Source: NCCS, 2005",
#       color = "") +
#  scale_x_continuous(labels = dollar, limits = c(0, 25000000000)) + 
#  scale_color_manual(values = c('red','blue'),
#                     labels = c(sprintf("Mean\n%s",dollar(mean(df$charitable_assets))),
#                                sprintf("Median\n%s",dollar(median(df$charitable_assets))))) +
#  theme_minimal(base_size = 20) +
#  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
#        plot.subtitle = element_text(hjust = 0.5),
#        text=element_text(family="Futura Medium"),
#        axis.text.y=element_blank(),
#        legend.key = element_rect(size = 6, fill = NA, color = NA),
#        legend.key.height = unit(2, "cm"),
#        legend.key.width = unit(1, "cm")) +
#  ggsave("plot/mscore/msa_asset_dist.png", height = 10, width = 16)


# Density -----------------------------------------------------------------

df2 <- read_csv("data/analysis/mscore/pkinc_county.csv") %>% 
  select(-cty)
dfc <- cbsaxw %>% left_join(clusters %>% select(-cbsa), by = "cbsa_fips") %>% 
  filter(clusterm_10 %in% c(2,4,8)) %>% 
  select(11,12,1,2,16) %>% arrange(cbsa_fips) %>% 
  mutate(cty_fips = as.numeric(cty_fips)) %>% 
  left_join(df2, by = "cty_fips") %>% 
  mutate(cluster = as.factor(clusterm_10),
         income = as.numeric(pki15)) %>% 
  select(cty_fips,county,cluster,income) %>% arrange(cty_fips)
colvals <- rev(c('#bb0000','#386cb0','#33a02c'))
(base_dist <- ggplot() +
  geom_density(data = dfc, aes(x = income, fill = cluster), 
               alpha = .3, show.legend = FALSE) +
  geom_point(data = df_fake, aes(x = person, y = value, color = variable), 
             size=8, alpha = .3) +
  labs(x = "Per-capita Personal Income", 
       y = "", 
       title = "Per-capita Personal Income in Legacy Region Counties", 
       #subtitle = "",
       caption = "Source: BEA, 2015",
       color = "Cluster") +
  scale_x_continuous(labels = dollar) +
  scale_fill_manual(values = colvals, labels = colvals) +
  scale_color_manual(values = colvals,
                     labels = c('Strong Transit & Walkability,\nWeak Housing Market',
                                'Manufacturing Base \nwith Weak Assets',
                                'High Skill, High Density')) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text=element_text(family="Futura Medium"),
        axis.text.y=element_blank(),
        legend.key = element_rect(size = 6, fill = NA, color = NA),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"))) 

#base_dist + ggsave("plot/mscore/cty_inc_dist.png", height = 10, width = 15)
summ_stats <- dfc %>% group_by(cluster) %>% 
  summarise(mean = mean(income),
            median = median(income),
            sd = sd(income),
            psd = (IQR(income))/1.349) %>% 
  as.data.frame()

base_dist + 
  geom_vline(xintercept = summ_stats[3,2], color = "red", linetype = "longdash", size=1) +
  geom_vline(xintercept = summ_stats[3,3], color = "blue", size=1) +
  #geom_segment(aes(x = summ_stats[3,2],y = .00003,xend = summ_stats[3,3],yend = .00003)) +
  geom_segment(aes(x = summ_stats[3,2],y = .00003,
                   xend = (summ_stats[3,2]+summ_stats[3,4]),yend = .00003),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'red',linetype = "longdash") +
  geom_segment(aes(x = summ_stats[3,3],y = .000035,
                   xend = (summ_stats[3,3]+summ_stats[3,5]),yend = .000035),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'blue') +
  annotate(geom="text", x = (summ_stats[3,2]+(1.1*summ_stats[3,4])),y = .000028, 
           label="PSD = $15,243", color="red",
           family="Futura Medium", size = 6) +
  annotate(geom="text", x = (summ_stats[3,3]+(1.1*summ_stats[3,5])),y = .000037, 
           label="SD = $18,456", color="blue",
           family="Futura Medium", size = 6) +
  labs(subtitle = "Measures of Central Tendency for the 'High Skill, High Density' Cluster") +
  theme(text=element_text(size = 24)) +
  ggsave("plot/mscore/clust3.png", width = 20, height = 12)



base_dist + 
  geom_vline(xintercept = summ_stats[3,2], color = "red", linetype = "longdash", size=1) +
  geom_vline(xintercept = summ_stats[3,3], color = "blue", size=1) +
  #geom_point(aes(x=93666, y = .000042), size = 2) +
  geom_segment(aes(x = 93666,y = .0000425,xend = 93666,yend = 0),
               size = 1, linetype = "dotted") +
  geom_segment(aes(x = summ_stats[3,2],y = .000027,xend = 93666,yend = .000027),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'red',linetype = "longdash") +
  geom_segment(aes(x = summ_stats[3,3],y = .000035,xend = 93666,yend = .000035),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'blue') +
  geom_segment(aes(xend = summ_stats[3,2],yend = .000027,x = 93666,y = .000027),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'red',linetype = "longdash") +
  geom_segment(aes(xend = summ_stats[3,3],yend = .000035,x = 93666,y = .000035),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'blue') +
  annotate(geom="text", x = 93666, y = .000045, 
           label="Westchester County, New York\nPer-capita Income: $93,666", color="black",
           family="Futura Medium", size = 6) +
  annotate(geom="text", x = 102555, y = .000027, 
           label="Z-Score: 1.68", color="red",
           family="Futura Medium", size = 6) +
  annotate(geom="text", x = 102555, y = .000035, 
           label="M-Score: 2.31", color="blue",
           family="Futura Medium", size = 6) +
  theme(text=element_text(size = 24)) +
  ggsave("plot/mscore/clust3_scores.png", width = 20, height = 12)


(93666 - summ_stats[3,2])/summ_stats[3,4] #1.68 z-scores
(93666 - summ_stats[3,3])/summ_stats[3,5] #2.31 m-scores



# Z and M -----------------------------------------------------------------

pop_scores <- df %>% select(cbsa,population_2005) %>% 
  mutate(zpop = zscore((population_2005)),
         mpop = mscore((population_2005)),
         lnzpop = zscore(log(population_2005)),
         lnmpop = mscore(log(population_2005))) #%>% write_csv("pop_zm.csv")

ggplot() +
  geom_density(data = pop_scores, aes(x = zpop),adjust = 0.7, fill = "red", alpha = .3) +
  geom_density(data = pop_scores, aes(x = mpop), fill = "blue", alpha = .3) +
  geom_point(data = df_fake2, aes(x = person, y = value, color = variable), 
             size=8, alpha = .3) +
  labs(x = "Score", 
       y = "", 
       title = "Normalized Population Distribution of US Metropolitan Areas", 
       subtitle = "Comparison of Z and M Transformations",
       #caption = "Source: BEA, 2015",
       color = "Distribution") +
  scale_x_continuous(labels = comma, limits = c(-2.5,6.5)) +
  scale_color_manual(values = c('red','blue'),
                     labels = c('Z-Scores','M-Scores')) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text=element_text(family="Futura Medium"),
        axis.text.y=element_blank(),
        legend.key = element_rect(size = 6, fill = NA, color = NA),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm")) +
  ggsave("plot/mscore/pop_scores.png", width = 16, height = 10)

ggplot() +
  geom_density(data = pop_scores, aes(x = lnzpop), fill = "red", alpha = .3) +
  geom_density(data = pop_scores, aes(x = lnmpop), fill = "blue", alpha = .3) +
  geom_point(data = df_fake2, aes(x = person, y = value, color = variable), 
             size=8, alpha = .3) +
  labs(x = "Score", 
       y = "", 
       title = "Normalized Distribution of Logged Population in US Metropolitan Areas", 
       subtitle = "Comparison of Z and M Transformations",
       #caption = "Source: BEA, 2015",
       color = "Distribution") +
  scale_x_continuous(labels = comma, limits = c(-2.5, 6.5)) +
  scale_color_manual(values = c('red','blue'),
                     labels = c('Z-Scores','M-Scores')) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text=element_text(family="Futura Medium"),
        axis.text.y=element_blank(),
        legend.key = element_rect(size = 6, fill = NA, color = NA),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm")) +
  ggsave("plot/mscore/lnpop_scores.png", width = 16, height = 10)

# CBSA Pop M and Z --------------------------------------------------------
# philly: 5829139
ggplot() +
  geom_vline(xintercept = 0, color = "black", size = .4) +
  geom_hline(yintercept = 0, color = "black", size = .4) +
  geom_density(data = pop_scores, aes(x = population_2005),fill = "black", 
               alpha = .3, adjust = 1.5) +
  geom_vline(xintercept = mean(df$population_2005), color = "red", size = .7) +
  geom_vline(xintercept = median(df$population_2005), color = "blue", size = .7) +
  geom_segment(aes(x = 5829139,y = .00000115,xend = 5829139,yend = 0),
               size = .4, linetype = "dotted") +
  geom_segment(aes(x = mean(df$population_2005),y = .000001,xend = 5829139,yend = .000001),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'red',linetype = "longdash") +
  geom_segment(aes(x = median(df$population_2005),y = .0000007,xend = 5829139,yend = .0000007),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'blue') +
  geom_segment(aes(xend = mean(df$population_2005),yend = .000001,x = 5829139,y = .000001),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'red',linetype = "longdash") +
  geom_segment(aes(xend = median(df$population_2005),yend = .0000007,x = 5829139,y = .0000007),
               arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
               color = 'blue') +
  annotate(geom="text", x = 5829139, y = .00000125, 
           label="Philadelphia-Camden-Wilmington, PA-NJ-DE-MD\nPopulation: 5,829,139", 
           color="black", family="Futura Medium", size = 6) +
  annotate(geom="text", x = 6750000, y = .000001, 
           label= sprintf("Z-Score: %s",
                          pop_scores %>% 
                            filter(str_detect(cbsa,"Philadelphia")) %>% 
                            pull(zpop) %>% round(2)), 
           color="red",
           family="Futura Medium", size = 6) +
  annotate(geom="text", x = 6750000, y = .0000007, 
           label= sprintf("M-Score: %s",
                         pop_scores %>% 
                           filter(str_detect(cbsa,"Philadelphia")) %>% 
                           pull(mpop) %>% round(2)), 
           color="blue",
           family="Futura Medium", size = 6) +
  geom_point(data = df_fake2, aes(x = person, y = value, color = variable), 
             size=8) +
  labs(x = "Population", 
       y = "", 
       title = "Population Distribution of US Metropolitan Areas", 
       #subtitle = "",
       caption = "Note: MSAs with a population over 10 million are not displayed.",
       color = "") +
  scale_x_continuous(labels = comma, limits = c(0, 10000000)) + 
  scale_color_manual(values = c('red','blue'),
                     labels = c(sprintf("Mean\n%s",comma(mean(df$population_2005))),
                                sprintf("Median\n%s",comma(median(df$population_2005))))) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5, family="Futura Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 12),
        text=element_text(family="Futura Medium"),
        axis.text.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.key = element_rect(size = 6, fill = NA, color = NA),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.justification=c(1,0.525), legend.position=c(1,0.525)) +
  ggsave("plot/mscore/msa_pop_dist_zm.png", height = 10, width = 16)


