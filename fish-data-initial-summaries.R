library(tidyverse)
library(docstring)
library(viridis)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/PhaseIII_Analysis")
## Analysis folder is the fold for saving _this_ particular run
plot_folder <- paste0(analysis_path,"/Plots")

fish <- read_csv(file = paste0(analysis_path, "/Fish_Metrics_CREP_2013-2020.csv"))
id <- read_csv(file = paste0(analysis_path, "/id_table_CREP_2013-2020_P3FR.csv"))

#### Create summary of Fish Data ####
fish_df <- fish %>% 
  filter(RICHNESS >5, INDIVIDUALS > 20)

library(psych)
fish_summary <- describe(fish)
write.csv(fish_summary, paste0(analysis_path,"/Fish_Metrics_summary_stats_2013-2020_allsites.csv"))

#### Now you can compare the different types ####

df <- id %>% 
  left_join(fish)

df$Year <- as.factor(lubridate::year(df$Event_Date))
df$Site_Type <- as.factor(df$Site_Type)

theme_update(plot.title = element_text(hjust = 0.5))


#### General Summaries ####

summary_all_sites <- df %>%
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

df %>% 
  ggplot2::ggplot(aes(x= RICHNESS)) +
  geom_histogram(binwidth = 1) +
  scale_fill_viridis()+
  scale_color_viridis()+
  labs(y = "Count", x = "Species Richness", title = "Species Richness All Sites by Year")+
  facet_wrap(~Year)

ggsave("richness_1320_histogram_all_sites_by_year.pdf", width = 8, height = 8, path = plot_folder, units = "in")



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
  labs(x = "Year", y = "Species Richness", title = "Richness of ISWS ALM Sites",
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
  labs(x = "Year", y = "Species Richness", title = "Richness of Random ALM Sites",
       caption = "Phase III Summmary")


ggsave("richness_1320_random_LD.pdf", width = 8, height = 8, path = plot_folder, units = "in")

#### Shannon Diversity ####
## All Sites & Types ###
df %>% ggplot2::ggplot(aes(x= Year, y=DIVERSITY, fill=Site_Type)) +
  geom_boxplot()+
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of ALM Sites by Site Type",
       caption = "Phase III Summmary")

ggsave("diversity_1320_allsites_bytype.pdf", width = 8, height = 8, path = plot_folder, units = "in")

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
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of ISWS ALM Sites",
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
  labs(x = "Year", y = "Species Shannon Diversity", title = "Shannon Diversity of Random ALM Sites",
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

