library(tidyverse)
# library(odbc)
# library(DBI)
library(docstring)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Habitat/PhaseIII_Analysis")
plot_folder<- paste0(analysis_path, "/Plots")

#### Read Habitat data IN from habitat-data-prep.R ####
# df_habitat <- read_csv(file = paste0(analysis_path, "/Data/Habitat_Metrics_CREP_2013-2020_P3FR.csv"))
df_habitat <- read_csv(file = paste0(analysis_path, "/Data/Habitat_Metrics_CREP_2013-2020_P3FR_basedOnIHI.csv"))
df_habitat$Year <- as.factor(df_habitat$Year)
df_habitat$Site_Type <- as.factor(df_habitat$Site_Type)

#### Set defaults and load libraries for visualization ####
library(ggplot2)
library(viridis)
theme_update(plot.title = element_text(hjust = 0.5))


#### Summary by Site Type per Phase ####
summary_sites_by_phase <- df_habitat %>%
  group_by(Phase, Site_Type) %>% 
  summarize(Total_Sites = n_distinct(Site_ID)) %>%
  ungroup() %>% 
  pivot_wider(names_from = Phase, values_from = Total_Sites)

# writexl::write_xlsx(summary_sites_by_phase, path = paste0(plot_folder,"/Habitat_Table1.xlsx"))

#### Summary by Site Type per Year####
summary_sites_by_yearphase <- df_habitat %>%
  group_by(Year,Site_Type) %>% 
  summarize(Total_Sites = n_distinct(Site_ID)) %>%
  ungroup() %>% 
  pivot_wider(names_from = Year, values_from = Total_Sites)

writexl::write_xlsx(summary_sites_by_yearphase, path = paste0(plot_folder,"/Habitat_Table_Year.xlsx"))


#### Summary of all 285 observation by Site Type & Metric not grouping by Phase of project. This just gives an overall
## idea of the mean for each parameter.

summary_sites_by_metric <- df_habitat %>%
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Site_Type, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)

writexl::write_xlsx(summary_sites_by_metric, path = paste0(plot_folder,"/Habitat_Table2.xlsx"))

#### Summary by Site Type & Metric per Phase ####
summary_sites_by_phase_type_1320 <- df_habitat %>%
  pivot_longer(cols = QHEI_Score:Visual_Water_Clarity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Site_Type, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Margin.error = qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)

sum_phase_type <- summary_sites_by_phase_type_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " \u00B1 ",formatC(Margin.error, digits = 2, format = "f"))) %>%
  select(Phase, Site_Type, Metric, summ) %>% 
  pivot_wider(names_from = Phase, values_from = summ)


writexl::write_xlsx(summary_sites_by_phase_type_1320, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_phase"))
writexl::write_xlsx(sum_phase_type, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_phase_formatted.xlsx"))

sum_phase_type_se <- summary_sites_by_phase_type_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " \u00B1 ",formatC(SE.metric, digits = 2, format = "f"))) %>%
  select(Phase, Site_Type, Metric, summ) %>% 
  pivot_wider(names_from = Phase, values_from = summ)

writexl::write_xlsx(sum_phase_type_se, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_phase_formatted_se.xlsx"))

sum_phase_type_sd <- summary_sites_by_phase_type_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " \u00B1 ",formatC(SD.metric, digits = 2, format = "f"))) %>%
  select(Phase, Site_Type, Metric, summ) %>% 
  pivot_wider(names_from = Phase, values_from = summ)

writexl::write_xlsx(sum_phase_type_sd, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_phase_formatted_sd.xlsx"))


#### Summary by Site Type & Metric per Year ####
summary_sites_by_year_type_1320 <- df_habitat %>%
  pivot_longer(cols = QHEI_Score:Visual_Water_Clarity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Site_Type, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Margin.error = qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)

sum_year_type <- summary_sites_by_year_type_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " \u00B1 ",formatC(Margin.error, digits = 2, format = "f"))) %>% 
  select(Year, Site_Type, Metric, summ) %>% 
  pivot_wider(names_from = Year, values_from = summ)

writexl::write_xlsx(summary_sites_by_year_type_1320, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_year.xlsx"))
writexl::write_xlsx(sum_year_type, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_year_formatted.xlsx"))

sum_year_type_se <- summary_sites_by_year_type_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " \u00B1 ",formatC(SE.metric, digits = 2, format = "f"))) %>% 
  select(Year, Site_Type, Metric, summ) %>% 
  pivot_wider(names_from = Year, values_from = summ)

writexl::write_xlsx(sum_year_type_se, path = paste0(plot_folder,"/Habitat_Table_all_sites_metric_year_formatted_se.xlsx"))

#### Drop Visual Water Clarity. There are too many NA's for phases I & II to reasonable to use for analysis

df_habitat <- df_habitat %>% 
  select(-c(Visual_Water_Clarity))

#### Summary Random Only by Phase ####
summary_random_sites_by_phase_1320 <- df_habitat %>%
  filter(Site_Type == "random") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)    

metric_list <- unique(summary_random_sites_by_phase_1320$Metric) 

### Random Sites - Barplots with 95% CI ###
for (met in metric_list) {
  summary_random_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Random Sites by Project Phase"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_random_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Random_ByPhase"), units = "in")
}


### Random Sites - Boxplots ###
for (met in metric_list) {
  df_habitat %>%
    # pivot_longer(cols = QHEI_Score:Visual_Water_Clarity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
    filter(Site_Type == "random") %>% 
    ggplot(aes_string(x= "Phase", y= met)) +
    geom_boxplot() +
    scale_color_viridis()+
    labs(x = "Phase",
         y = str_replace_all(met, "_", " "), 
         title = paste0("Boxplot of ", str_replace_all(met, "_", " ")," at Random Sites by Project Phase"))
  
  ggsave(paste0(str_to_lower(met),"_1320_boxplot_random_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Random_ByPhase"), units = "in")
}


#### Random Site By Year - Barplots with 95% CI ####
summary_random_sites_by_year_1320 <- df_habitat %>%
  filter(Site_Type == "random") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)    

metric_list <- unique(summary_random_sites_by_year_1320$Metric) 

for (met in metric_list) {
  summary_random_sites_by_year_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Year, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Year, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Year, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Random Sites by Year"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_random_by_year.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Random_ByYear"), units = "in")
}

#### Summary Random Only by Year ####

summary_random_sites_by_year_1320 <- df_habitat %>%
  filter(Site_Type == "random") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric)    

metric_list <- unique(summary_random_sites_by_year_1320$Metric) 


for (met in metric_list) {
  summary_random_sites_by_year_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Year, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Year, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Year, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Random Sites by Year"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0("RandomByYear",str_to_lower(met),"_barplot.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Random_ByYear"), units = "in")
}
####
#### Summary Sensitive Species Only by Phase ####
summary_sensitive_sites_by_phase_1320 <- df_habitat %>%
  filter(Site_Type == "copper") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

## Hach parameters (Ammonia, Nitrate, Orthophosphate and Turbidity) and Visual water Clarity not available for sensitive species sites

cut_list <- summary_sensitive_sites_by_phase_1320 %>% 
  filter(is.na(Mean.metric)) %>%
  select(Metric) %>% 
  unique()

cut_list <- cut_list$Metric

metric_list <- summary_sensitive_sites_by_phase_1320 %>% 
  select(Metric) %>% 
  filter(!Metric %in% cut_list) %>% 
  unique()

metric_list <- unique(metric_list$Metric)

### Sensitive Sites - Barplots with 95% CI ###
for (met in metric_list) {
  summary_sensitive_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Sensitive Species Sites by Project Phase"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_sensitive_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/SensitiveSpecies_ByPhase"), units = "in")
}


### Sensitive Sites - Boxplots ###
for (met in metric_list) {
  df_habitat %>%
    filter(Site_Type == "copper") %>% 
    ggplot(aes_string(x= "Phase", y= met)) +
    geom_boxplot() +
    scale_color_viridis()+
    labs(x = "Phase",
         y = str_replace_all(met, "_", " "), 
         title = paste0("Boxplot of ", str_replace_all(met, "_", " ")," at Sensitive Species Sites by Project Phase"))
  
  ggsave(paste0("boxplot_", str_to_lower(met),"_1320_sensitive_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/SensitiveSpecies_ByPhase"), units = "in")
}


####

#### Summary Sensitive Species Only by Year ####

summary_sensitive_sites_by_year_1320 <- df_habitat %>%
  filter(Site_Type == "copper") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup() %>% 
  drop_na()

metric_list <- summary_sensitive_sites_by_year_1320 %>% 
  select(Metric) %>% 
  arrange() %>% 
  unique()

metric_list <- unique(metric_list$Metric)

for (met in metric_list) {
  summary_sensitive_sites_by_year_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Year, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Year, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Year, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Sensitive Species Sites by Year"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0("SensitiveByYear_",str_to_lower(met),"_barplot.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/SensitiveSpecies_ByYear"), units = "in")
}

#### Summary Paired Sites Only by Phase ####
pair_list <- read_csv(file = paste0(analysis_path, "/Data/Paired_Site_Groupings.csv"))

summary_paired_sites_by_phase_1320 <- df_habitat %>%
  filter(Site_Type == "paired") %>%
  left_join(pair_list) %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Metric, CRP_Class) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

#### Check any parameters that there are not enough measurements for a non-finite mean. 
cut_list <- summary_paired_sites_by_phase_1320 %>% 
  filter(is.na(Mean.metric)) %>%
  select(Metric) %>% 
  unique()

cut_list <- cut_list$Metric

metric_list <- summary_paired_sites_by_phase_1320 %>% 
  select(Metric) %>% 
  filter(!Metric %in% cut_list) %>% 
  unique()

metric_list <- unique(metric_list$Metric)

### Paired Sites - Barplots with 95% CI ###
for (met in metric_list) {
  summary_paired_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot(aes(x=Phase, y= Mean.metric, fill = CRP_Class)) +
    geom_bar(position = "dodge",  stat="identity", alpha=0.7)+
    geom_errorbar(aes(ymin=Lower.ci, ymax=Upper.ci), position = position_dodge(0.9),  width=0.20, alpha=0.9, size=1.00) +
    geom_pointrange(aes(ymin=Lower.ci, ymax=Upper.ci), position = position_dodge(0.9), alpha=0.9, size=0.60) +
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         fill = "CRP Class",
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Paired Sites by Project Phase & CRP Level"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_paired_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Paired"), units = "in")
}

 ### Paired Sites - Boxplots ###
for (met in metric_list) {
  df_habitat %>%
    filter(Site_Type == "paired") %>%
    left_join(pair_list) %>% 
    ggplot(aes_string(x= "Phase", y= met, fill = "CRP_Class")) +
    geom_boxplot() +
    scale_color_viridis()+
    labs(x = "Phase",
         y = str_replace_all(met, "_", " "), 
         title = paste0("Boxplot of ", str_replace_all(met, "_", " ")," at Paired Sites by Project Phase"))
  
  ggsave(paste0("boxplot_", str_to_lower(met),"_1320_paired_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Paired"), units = "in")
}


sum_paired_phase_level <- summary_paired_sites_by_phase_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " (min ",Min.metric, ", max ",Max.metric, ")")) %>% 
  select(Phase, CRP_Class, Metric, summ) %>% 
  pivot_wider(names_from = Phase, values_from = summ)

writexl::write_xlsx(sum_paired_phase_level, path = paste0(plot_folder,"/Habitat_Table_Paired_Summary.xlsx"))
#### Summary Paired Sites Only by Year ####

summary_paired_sites_by_year_1319 <- df_habitat %>%
  filter(Site_Type == "paired",
         Year != 2020) %>%
  left_join(pair_list) %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric, CRP_Class) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

for (met in metric_list) {
  summary_paired_sites_by_year_1319 %>%
    filter(Metric == met) %>% 
    ggplot(aes(x=Year, y= Mean.metric, fill = CRP_Class)) +
    geom_bar(position = "dodge",  stat="identity", alpha=0.7)+
    geom_errorbar(aes(ymin=Lower.ci, ymax=Upper.ci), position = position_dodge(0.9),  width=0.20, alpha=0.9, size=1.00) +
    geom_pointrange(aes(ymin=Lower.ci, ymax=Upper.ci), position = position_dodge(0.9), alpha=0.9, size=0.60) +
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         fill = "CRP Class",
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Paired Sites by Year"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0("PairedByYear_",str_to_lower(met),"_barplot.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/Paired"), units = "in")
}

#### Paired Sites Summary Table by Year ####
summary_paired_sites_by_year_1320 <- df_habitat %>%
  filter(Site_Type == "paired")%>%
  left_join(pair_list) %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric, CRP_Class) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

sum_paired_year_level <- summary_paired_sites_by_year_1320 %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " \u00B1 ",formatC(SE.metric, digits = 2, format = "f"))) %>% 
  select(Metric, Year, CRP_Class, summ) %>% 
  pivot_wider(names_from = Year, values_from = summ)

writexl::write_xlsx(sum_paired_year_level, path = paste0(plot_folder,"/Habitat_Table_Paired_SummaryByYear.xlsx"))

#### Summary LD Sites Only by Year ####

summary_ld_sites_by_year_1320 <- df_habitat %>%
  filter(Site_Type == "less_disturbed") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Year, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

## Hach parameters (Ammonia, Nitrate, Orthophosphate and Turbidity) and Visual water Clarity not available for sensitive species sites

cut_list <- summary_ld_sites_by_year_1320 %>% 
  filter(is.na(Mean.metric)) %>%
  select(Metric) %>% 
  unique()

cut_list <- cut_list$Metric

metric_list <- summary_ld_sites_by_year_1320 %>% 
  select(Metric) %>% 
  filter(!Metric %in% cut_list) %>% 
  unique()

metric_list <- unique(metric_list$Metric)

### LD Sites - Barplots with 95% CI ###
for (met in metric_list) {
  summary_ld_sites_by_year_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Year, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Year, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Year, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Less-Disturbed Sites by Year"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_LD_by_year.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/LD"), units = "in")
}


### LD Sites - Boxplots ###
for (met in metric_list) {
  df_habitat %>%
    filter(Site_Type == "less_disturbed") %>% 
    ggplot(aes_string(x= "Year", y= met)) +
    geom_boxplot() +
    scale_color_viridis()+
    labs(x = "Year",
         y = str_replace_all(met, "_", " "), 
         title = paste0("Boxplot of ", str_replace_all(met, "_", " ")," at Less-Disturbed Sites by Year"))
  
  ggsave(paste0("x_boxplot_", str_to_lower(met),"_1320_ld_by_year.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/LD"), units = "in")
}
  
  
### LD by Year compared to Random Sites
summary_sites_by_year_type <- df_habitat %>%
    pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
    group_by(Year, Site_Type, Metric) %>% 
    summarize(Total_Sites = n_distinct(Site_ID),
              Mean.metric = mean(Value, na.rm = T),
              Min.metric = min(Value, na.rm = T),
              Max.metric = max(Value, na.rm = T),
              N.metric = sum(!is.na(Value)),
              N.NA = sum(is.na(Value)),
              SD.metric =sd(Value, na.rm = TRUE)
    ) %>% 
    mutate(SE.metric = SD.metric / sqrt(N.metric),
           Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
           Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

### LD Sites with Random Sites - Barplots with 95% CI ###
for (met in metric_list) {
  summary_sites_by_year_type %>%
    filter(Metric == met,
           Site_Type == "less_disturbed"|Site_Type == "random",
           Year == 2018|Year == 2019|Year == 2020 ) %>%
    ggplot(aes(x=Year, y= Mean.metric, fill= Site_Type)) +
    geom_bar(position = position_dodge(0.9), stat="identity", alpha=0.7)+
    geom_errorbar(aes(ymin=Lower.ci, ymax=Upper.ci), position = position_dodge(0.9), width=0.25, alpha=0.9, size=1.00) +
    geom_pointrange(aes(ymin=Lower.ci, ymax=Upper.ci), position = position_dodge(0.9), alpha=0.9, size=0.6) +
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of Less-Disturbed & Random Sites by Year"),
         fill= "Site Type", 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0("LD_Random_",str_to_lower(met),"_barplot_by_year.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/LD"), units = "in")
}


sum_ld_year <- summary_sites_by_year_type %>%
  filter(Site_Type == "less_disturbed",
         Year == 2018|Year == 2019|Year == 2020 ) %>% 
  mutate(summ = paste0(formatC(Mean.metric, digits = 2, format = "f"), " (min ",Min.metric, ", max ",Max.metric, ")")) %>% 
  select(Metric, Year, summ) %>% 
  pivot_wider(names_from = Year, values_from = summ)

writexl::write_xlsx(sum_ld_year, path = paste0(plot_folder,"/Habitat_Table_LD_Summary.xlsx"))

#### Summary ISWS Sites Only by Phase ####

#TODO What are the different levels of ISWS sites?

summary_isws_sites_by_phase_1320 <- df_habitat %>%
  filter(Site_Type == "ISWS") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Phase, Metric) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            Mean.metric = mean(Value, na.rm = T),
            Min.metric = min(Value, na.rm = T),
            Max.metric = max(Value, na.rm = T),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE)
  ) %>% 
  mutate(SE.metric = SD.metric / sqrt(N.metric),
         Lower.ci = Mean.metric - qt(1 - (0.05 / 2), N.metric - 1) * SE.metric,
         Upper.ci = Mean.metric + qt(1 - (0.05 / 2), N.metric - 1) * SE.metric) %>% 
  ungroup()

## Hach parameters (Ammonia, Nitrate, Orthophosphate and Turbidity) and Visual water Clarity not available for sensitive species sites

cut_list <- summary_isws_sites_by_phase_1320 %>% 
  filter(is.na(Mean.metric)) %>%
  select(Metric) %>% 
  unique()

cut_list <- cut_list$Metric

metric_list <- summary_isws_sites_by_phase_1320 %>% 
  select(Metric) %>% 
  filter(!Metric %in% cut_list) %>% 
  unique()

metric_list <- unique(metric_list$Metric)

### ISWS Sites - Barplots with 95% CI ###
for (met in metric_list) {
  summary_isws_sites_by_phase_1320 %>%
    filter(Metric == met) %>% 
    ggplot() +
    geom_bar( aes(x=Phase, y= Mean.metric), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=Phase, ymin=Lower.ci, ymax=Upper.ci), width=0.25, alpha=0.9, size=1.25) +
    geom_pointrange( aes(x=Phase, y=Mean.metric, ymin=Lower.ci, ymax=Upper.ci), alpha=0.9, size=1.25) +
    scale_color_viridis()+
    labs(y = paste0("Mean ", str_replace_all(met, "_", " ")), 
         title = paste0("Mean ", str_replace_all(met, "_", " ")," of ISWS Sites by Project Phase"), 
         caption = "Metric mean with 95% Confidence Intervals")
  
  ggsave(paste0(str_to_lower(met),"_1320_barplot_CI_isws_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/ISWS"), units = "in")
}


###  ISWS Scatter Plots by Year ###
isws_sites_restructure <- df_habitat %>%
  select(-c(Visual_Water_Clarity)) %>% 
  filter(Site_Type == "ISWS") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F)

for (met in metric_list) {
  isws_sites_restructure %>%
    filter(Metric == met) %>% 
    ggplot(aes(x = Year, y = Value)) +
    geom_point() +
    geom_jitter() +
    ylab(str_replace_all(met, "_", " ")) +
    labs(title = paste0(str_replace_all(met, "_", " ")," at ISWS Sites from 2014-2020")) 
  
  ggsave(paste0("scatter_by_year_",str_to_lower(met),".pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/ISWS"), units = "in")
}  
  
  
### ISWS Sites - Boxplots ###
for (met in metric_list) {
  df_habitat %>%
    filter(Site_Type == "ISWS") %>% 
    ggplot(aes_string(x= "Phase", y= met)) +
    geom_boxplot() +
    scale_color_viridis()+
    labs(x = "Phase",
         y = str_replace_all(met, "_", " "), 
         title = paste0("Boxplot of ", str_replace_all(met, "_", " ")," at ISWS Sites by Project Phase"))
  
  ggsave(paste0("boxplot_", str_to_lower(met),"_1320_isws_by_phase.pdf"), 
         width = 8, 
         height = 8, 
         path = paste0(plot_folder, "/ISWS"), units = "in")
}
