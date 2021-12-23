library(tidyverse)
library(ggplot2)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/PhaseIII_Analysis")
plot_folder <- paste0(analysis_path,"/Plots")

### Open fish-data-prep and load in the data columns from the data base including
## pair_list and loc
## You will also need df from fish-data-initial-summaries.R either run that script or load it here:

# df <- read_csv(file = paste0(analysis_path,"/Fish_Metrics_summary_stats_2013-2020_allsites.csv"))


#### Paired Site Summary Analysis ####

paired <- df %>% 
  filter(Site_Type == 'paired') %>% 
  left_join(pair_list) %>% 
  select(1:6,118:119,7:116)


# scone <- paired %>% 
#   select(1:9) %>% 
#   select(-c(Site_ID, Reach_Name, Event_Date)) %>% 
#   pivot_wider(names_from = Year, values_from = INDIVIDUALS)

paired_differences <- paired %>% 
  pivot_longer(cols = INDIVIDUALS:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  select(-c(Site_ID, PU_Gap_Code, Reach_Name, Event_Date, Site_Type)) %>% 
  group_by(Metric, Pair_Number, Year) %>%
  pivot_wider(names_from = CRP_Class, values_from = Value) %>% 
  mutate(difference_hl = high - low)


### Plot Differences 

metric_list <- unique(paired_differences$Metric)

paired_differences %>%
  filter(Metric == 'RICHNESS') %>% 
  ggplot2::ggplot(aes(x= Year, y= difference_hl, color = factor(Pair_Number))) +
  geom_point(size = 2) +
  labs(y = "Difference (High CRP - Low CRP)", x = "Year", title = "Differences in Species Richness of Paired Sites by Year", color = "Pair Number")


ggsave("richness_1320_histogram_paired_sites.pdf", width = 8, height = 8, path = plot_folder, units = "in")

for (met in metric_list) {
  paired_differences %>%
    filter(Metric == met) %>% 
    ggplot2::ggplot(aes(x= Year, y= difference_hl, color = factor(Pair_Number))) +
    geom_point(size = 2) +
    labs(y = "Difference (High CRP - Low CRP)", x = "Year", title = paste0("Differences in ", stringr::str_to_title(met)," of Paired Sites by Year"), color = "Pair Number")
  
  
  ggsave(paste0(str_to_lower(met),"_1320_histogram_paired_sites.pdf"), width = 8, height = 8, path = paste0(plot_folder, "/Paired"), units = "in")
}

