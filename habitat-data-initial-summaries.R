library(tidyverse)
library(odbc)
library(DBI)
library(docstring)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Fish/PhaseIII_Analysis")


#### Read Habitat data IN from habitat-data-prep.R ####



#### Write to File ####
df_habitat <- read_csv(site_metric_tibble, file = paste0(analysis_path, "/Habitat_Metrics_CREP_2013-2020_P3FR.csv"))

#### Set defaults and load libraries for visualization ####
library(ggplot2)
library(viridis)
theme_update(plot.title = element_text(hjust = 0.5))


#### Summary by Site Type per Phase ####
summary_sites_by_phase_type_1320 <- df_habitat %>%
  pivot_longer(cols = IBI:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Site_Type, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean.metric = mean(Value),
            Min.metric = min(Value),
            Max.metric = max(Value),
            N.metric = n(),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)


#### Summary Random Only by Phase ####
summary_random_sites_by_phase_1320 <- df_habitat %>%
  filter(Site_Type == "random") %>% 
  pivot_longer(cols = IBI:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean.metric = mean(Value),
            Min.metric = min(Value),
            Max.metric = max(Value),
            N.metric = n(),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)    

metric_list <- unique(summary_random_sites_by_phase_1320$Metric) 

for (met in metric_list) {
  summary_random_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", stringr::str_to_title(met)), 
         title = paste0("Mean ", stringr::str_to_title(met)," of Random Sites by Project Phase"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_random_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Random_ByPhase"), units = "in")
}

