library(tidyverse)
library(docstring)
library(viridis)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/PhaseIII_Analysis")
## Analysis folder is the fold for saving _this_ particular run
plot_folder <- paste0(analysis_path,"/Plots")

metrics <- read_csv(file = paste0(analysis_path, "/Fish_Metrics_CREP_2013-2020.csv"))
id <- read_csv(file = paste0(analysis_path, "/id_table_CREP_2013-2020_P3FR.csv"))
ibi <- read_csv(file = paste0(network_path, "/Data/Data_IN/IBI/IBI_Output_2013_2020.csv"))


#### Create summary of Fish Metric Data ####

library(psych)
metrics_summary <- describe(metrics)
write.csv(metrics_summary, paste0(analysis_path,"/Fish_Metrics_summary_stats_2013-2020_allsites.csv"))

#### Simplify IBI ####
ibi <- ibi %>% 
  select(PU_Gap_Code, Reach_Name, Event_Date = `Sample Date`, IBI)


#### Now you can compare the different types ####

df <- id %>% 
  left_join(metrics) %>% 
  left_join(ibi)

df$Year <- as.factor(lubridate::year(df$Event_Date))
df$Site_Type <- as.factor(df$Site_Type)

df <- df %>% 
  select(1:4,117,5,116,6:115)

df <- df %>% 
  mutate(Phase = if_else(Year == 2013|Year == 2014|Year == 2015, 'Phase I', 
                         if_else(Year == 2016|Year == 2017, 'Phase II', 'Phase III'))) %>% 
  select(1:5,118,6:117)

theme_update(plot.title = element_text(hjust = 0.5))


#### General Summaries ####

summary_all_sites_1320 <- df %>%
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

summary_sites_1320 <- df %>%
  pivot_longer(cols = IBI:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean = mean(Value),
            Min = min(Value),
            Max = max(Value)
            
  )


summary_sites_1320_IRDEonly <- summary_sites_1320 %>% 
  filter(Metric == "INDIVIDUALS"|Metric == "RICHNESS"|Metric == "DIVERSITY"|Metric == "EVENNESS")

summary_sites_by_year_1320 <- df %>%
  pivot_longer(cols = IBI:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean = mean(Value),
            Min = min(Value),
            Max = max(Value)
            
  )


summary_sites_by_year_1320_IRDEonly <- summary_sites_by_year_1320 %>% 
  filter(Metric == "INDIVIDUALS"|Metric == "RICHNESS"|Metric == "DIVERSITY"|Metric == "EVENNESS")

summary_sitetypes_1320 <- df %>%
  group_by(Site_Type, Year) %>% 
  summarize(Total_Sites = n())

summary_sites_by_type_1320 <- df %>%
  pivot_longer(cols = IBI:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Site_Type, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Total_Fish = sum(Value),
            Mean = mean(Value),
            Min = min(Value),
            Max = max(Value)
            
  )
            

##Write some things out for tables

write_csv(summary_sites_by_year_1320, path = paste0(plot_folder, "/fish_metric_summary_1320_byyear.csv"))
write_csv(summary_sites_by_year_1320_IRDEonly, path = paste0(plot_folder, "/fish_metric_summary_IRDE_1320_byyear.csv"))
write_csv(summary_sites_1320, path = paste0(plot_folder, "/fish_metric_summary_1320.csv"))
write_csv(summary_sites_1320_IRDEonly, path = paste0(plot_folder, "/fish_metric_summary_IRDE_1320.csv"))
write_csv(summary_sitetypes_1320, path = paste0(plot_folder, "/fish_sitetypes_byyear.csv"))


#### Histograms for general summary and understanding.

df %>% 
  ggplot2::ggplot(aes(x= RICHNESS)) +
  geom_histogram(binwidth = 1) +
  scale_fill_viridis()+
  scale_color_viridis()+
  labs(y = "Count", x = "Species Richness", title = "Species Richness All Sites by Year")+
  facet_wrap(~Year)

ggsave("richness_1320_histogram_all_sites_by_year.pdf", width = 8, height = 8, path = plot_folder, units = "in")

df %>% 
  filter(Site_Type == "random") %>% 
  ggplot2::ggplot(aes(x= RICHNESS)) +
  geom_histogram(binwidth = 1) +
  scale_fill_viridis()+
  scale_color_viridis()+
  labs(y = "Count", x = "Species Richness", title = "Species Richness All Sites by Year")+
  facet_wrap(~Year)

ggsave("richness_1320_histogram_random_sites.pdf", width = 8, height = 8, path = plot_folder, units = "in")



summary_site_type_year <- df %>% 
  group_by(Site_Type, Year = lubridate::year(Event_Date)) %>% 
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




#### Richness ####
## All Sites & Types ###
df %>% ggplot2::ggplot(aes(x= Year, y=RICHNESS, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Richness", title = "Richness of ALM Sites by Site Type",
       caption = "Phase III Summmary")

ggsave("richness_1320_allsites_bytype.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### Random Sites ###

df %>% 
  filter(Site_Type == "random") %>% 
  ggplot2::ggplot(aes(x= Year, y=RICHNESS, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Richness", title = "Richness of Random ALM Sites",
       caption = "Phase III Summmary")

# geom_text(data = ,aes(label = after_stat(count)), position = position_dodge(width = 0.75))
# stat_summary(fun.data = give.n, geom = "text", fun = median,
#                position = position_dodge(width = 0.75))

ggsave("richness_1320_random.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### ISWS Sites ###

df %>% 
  filter(Site_Type == "ISWS") %>% 
  ggplot2::ggplot(aes(x= Year, y=RICHNESS, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Richness", title = "Richness of ISWS ALM Sites",
       caption = "Phase III Summmary")

ggsave("richness_1320_ISWS.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### Copper Sites ###

df %>% 
  filter(Site_Type == "copper") %>% 
  ggplot2::ggplot(aes(x= Year, y=RICHNESS, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Richness", title = "Richness of Senstive Species ALM Sites",
       caption = "Phase III Summmary")

ggsave("richness_1320_copper.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### LD Sites ###

df %>% 
  filter(Site_Type == "less_disturbed") %>% 
  ggplot2::ggplot(aes(x= Year, y=RICHNESS, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Richness", title = "Richness of Less Disturbed ALM Sites",
       caption = "Phase III Summmary")

ggsave("richness_1320_LD.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### Random with LD Sites ###

df %>% 
  filter(Site_Type == "random"| Site_Type == "less_disturbed") %>% 
  ggplot2::ggplot(aes(x= Year, y=RICHNESS, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Richness", title = "Richness of Random & Less Disturbed ALM Sites",
       caption = "Phase III Summmary")


ggsave("richness_1320_random_LD.pdf", width = 8, height = 8, path = plot_folder, units = "in")

#### Shannon Diversity ####
## All Sites & Types ###
df %>% ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of ALM Sites by Site Type",
       caption = "Phase III Summmary")

ggsave("diversity_1320_allsites_bytype.pdf", width = 8, height = 8, path = plot_folder, units = "in")


df %>% 
  ggplot2::ggplot(aes(x= DIVERSITY)) +
  geom_histogram(binwidth = 0.1) +
  scale_fill_viridis()+
  scale_color_viridis()+
  labs(y = "Count", x = "Shannon Diversity", title = "Shannon Diversity All Sites by Year")+
  scale_x_continuous(breaks = seq(0,3,0.5))+
  facet_wrap(~Year)

ggsave("diversity_1320_histogram_all_sites_by_year.pdf", width = 8, height = 8, path = plot_folder, units = "in")

df %>%
  filter(Site_Type == "random") %>% 
  ggplot2::ggplot(aes(x= DIVERSITY)) +
  geom_histogram(binwidth = 0.1) +
  scale_fill_viridis()+
  scale_color_viridis()+
  labs(y = "Count", x = "Shannon Diversity", title = "Shannon Diversity Random Sites by Year")+
  scale_x_continuous(breaks = seq(0,3,0.5))+
  facet_wrap(~Year)

ggsave("diversity_1320_histogram_random_sites_by_year.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### Random Sites ###

df %>% 
  filter(Site_Type == "random") %>% 
  ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of Random ALM Sites",
       caption = "Phase III Summmary")

# geom_text(data = ,aes(label = after_stat(count)), position = position_dodge(width = 0.75))
# stat_summary(fun.data = give.n, geom = "text", fun = median,
#                position = position_dodge(width = 0.75))

ggsave("diversity_1320_random.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### ISWS Sites ###
## TODO can you look at the different sites through time? and how within a site they might be changing? Such as a repeated measures

df %>% 
  filter(Site_Type == "ISWS") %>% 
  ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of ISWS ALM Sites",
       caption = "Phase III Summmary")

ggsave("diversity_1320_ISWS.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### Copper Sites ###

df %>% 
  filter(Site_Type == "copper") %>% 
  ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of Sensitive Species ALM Sites",
       caption = "Phase III Summmary")

ggsave("diversity_1320_copper.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### LD Sites ###

df %>% 
  filter(Site_Type == "less_disturbed") %>% 
  ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of Less Disturbed ALM Sites",
       caption = "Phase III Summmary")

ggsave("diversity_1320_LD.pdf", width = 8, height = 8, path = plot_folder, units = "in")

### Random with LD Sites ###

df %>% 
  filter(Site_Type == "random"| Site_Type == "less_disturbed") %>% 
  ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of Random & Less-Disturbed ALM Sites",
       caption = "Phase III Summmary")


ggsave("diversity_1320_random_LD.pdf", width = 8, height = 8, path = plot_folder, units = "in")


#### ISWS Sites through time ####

df %>% 
  filter(Site_Type == "ISWS") %>% 
  ggplot2::ggplot(aes(x= Year, y= RICHNESS)) +
  geom_line(aes(group = PU_Gap_Code)) +
  geom_point(aes(x= Year, y= RICHNESS, shape = PU_Gap_Code), size = 3) +
  labs(x = "Year", y = "Species Richness", title = "Richness of ISWS Sites",
       caption = "Phase III Summmary")

ggsave("richness_1320_isws_repeated_measures.pdf", width = 8, height = 8, path = plot_folder, units = "in")


write_csv(df, path = paste0(analysis_path, "/fish_metrics_all_sites_withID_1320.csv")) 


#### Summary by Site Type per Phase ####
summary_sites_by_phase_type_1320 <- df %>%
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

#### Summary by Phase ####
summary_sites_by_phase_1320 <- df %>%
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

for (met in metric_list) {
summary_sites_by_phase_1320 %>%
  filter(Metric == met) %>% 
  ggplot() +
  geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
  geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
  geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
  scale_color_viridis()+
  labs(y = paste0("Mean ", stringr::str_to_title(met)), 
       title = paste0("Mean", stringr::str_to_title(met)," of All Sites by Project Phase"), 
       caption = "Metric mean with 95% Confidence Intervals")
 
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/ByPhase"), units = "in")
}


#### Summary Random Only by Phase ####
summary_random_sites_by_phase_1320 <- df %>%
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

for (met in metric_list) {
  summary_random_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", stringr::str_to_title(met)), 
         title = paste0("Mean", stringr::str_to_title(met)," of Random Sites by Project Phase"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_random_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Random_ByPhase"), units = "in")
}

##### Summary Copper Slough/Sensitive Species Sites 
summary_copper_sites_by_phase_1320 <- df %>%
filter(Site_Type == "copper") %>% 
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

for (met in metric_list) {
  summary_copper_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", stringr::str_to_title(met)), 
         title = paste0("Mean", stringr::str_to_title(met)," of Sensitive Species Sites by Project Phase"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_random_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Copper_ByPhase"), units = "in")
}

##### Summary Plots LD Sites by Year
summary_ld_sites_by_phase_1320 <- df %>%
  filter(Site_Type == "less_disturbed") %>% 
  pivot_longer(cols = IBI:COSUBPIND, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
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

for (met in metric_list) {
  summary_ld_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Year, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Year, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Year, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", stringr::str_to_title(met)), 
         title = paste0("Mean", stringr::str_to_title(met)," of Less Disturbed Sites by Year"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_ld_by_year.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/LD_ByYear"), units = "in")
}
