library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/PhaseIII_Analysis")

## Analysis folder is the fold for saving _this_ particular run
plot_folder <- paste0(analysis_path,"/Plots")

metrics <- read_csv(file = paste0(analysis_path, "/Fish_Metrics_CREP_2013-2020.csv"))
id <- read_csv(file = paste0(analysis_path, "/id_table_CREP_2013-2020_P3FR.csv"))

### This uses all of the site data if you want to remove sites with limited numbers of species or individuals use the following:
# metrics_df <- metrics %>% 
#   filter(RICHNESS >5, INDIVIDUALS > 20)


#### Create summary of Fish Metric Data ####

library(psych)
metrics_summary <- describe(metrics)
write.csv(metrics_summary, paste0(analysis_path,"/Fish_Metrics_summary_stats_2013-2020_allsites.csv"))

#### Now you can compare the different types ####

df <- id %>% 
  left_join(metrics)

df$Year <- as.factor(lubridate::year(df$Event_Date))
df$Site_Type <- as.factor(df$Site_Type)

theme_update(plot.title = element_text(hjust = 0.5))

#### Fish Summaries ####
summary_all_sites_phase3 <- df %>%
  filter(Year == 2018|Year == 2019|Year == 2020) %>% 
  summarize(Total_Sites = n(),
            Total_Fish = sum(INDIVIDUALS),
            Individuals_Mean = mean(INDIVIDUALS),
            Individuals_Min = min(INDIVIDUALS),
            Individuals_Max = max(INDIVIDUALS),
            Richness_Mean = mean(RICHNESS),
            Richness_Min = min(RICHNESS),
            Richness_Max = max(RICHNESS),
            Diversity_Mean = mean(DIVERSITY),
            Diversity_Min = min(DIVERSITY),
            Diversity_Max = max(DIVERSITY),
            Eveness_Mean = mean(EVENNESS),
            Evenness_Min = min(EVENNESS),
            Evenness_Max = max(EVENNESS)
            
  )

summary_metrics_phase3 <- df %>%
  filter(Year == 2018|Year == 2019|Year == 2020) %>%
  pivot_longer(cols = INDIVIDUALS:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean = mean(Value),
            Min = min(Value),
            Max = max(Value)
            
  )

summary_metrics_phase3[summary_metrics_phase3$Metric=='RICHNESS', "Total_Fish"] <- unique_fish_1820[1,1]

summary_sites_by_year_phase3 <- df %>%
  filter(Year == 2018|Year == 2019|Year == 2020) %>%
  pivot_longer(cols = INDIVIDUALS:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean = mean(Value),
            Min = min(Value),
            Max = max(Value)
            
  )

summary_all_sites_phase3_byyear <- summary_sites_by_year_phase3 %>% 
  filter(Metric == "INDIVIDUALS"|Metric == "RICHNESS"|Metric == "DIVERSITY"|Metric == "EVENNESS")


#### Richness Unique number of identified species ####
unique_fish_1820_byyear <- fish_table_full %>% 
  filter(Hybrid == 0, 
         Unidentified_Species == 0,
         Year == 2018|Year == 2019|Year == 2020) %>%
  group_by(Year) %>% 
  summarise(richness = n_distinct(Fish_Species_Code))

unique_fish_1820 <- fish_table_full %>% 
  filter(Hybrid == 0, 
         Unidentified_Species == 0,
         Year == 2018|Year == 2019|Year == 2020) %>%
  summarise(richness = n_distinct(Fish_Species_Code))


##Write some things out for tables

write_csv(summary_all_sites_phase3_byyear, path = paste0(plot_folder, "/fish_IRDE_summary_phase3_byyear.csv"))
write_csv(summary_all_sites_phase3, path = paste0(plot_folder, "/fish_IRDE_summary_phase3.csv"))
write_csv(summary_sites_by_year_phase3, path = paste0(plot_folder, "/fish_metrics_summary_phase3_byyear.csv"))
write_csv(summary_metrics_phase3, path = paste0(plot_folder, "/fish_metrics_summary_phase3.csv"))
