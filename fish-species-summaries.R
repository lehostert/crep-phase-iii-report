library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Fish/PhaseIII_Analysis")

fish_data <- read_csv(file = paste0(analysis_path, "/Fish_Abundance_Data_CREP_2013-2020_P3FR.csv"))
  
  
# Add traits to fish count data
  
add_traits_to_data <- function(species_count_data) {
  
  #' Create dataframe in which species specific traits are joined with data collection data. 
  #'
  #' 
  #' @param species_count_data A dataframe with at least 3 collumns unique idenifying name for sample (Site_ID), 
  #' 3-letter short name for fish species (Fish_Species_Code), count of the number of that specific species sampled (Fish_Species_Count)
  #' 
  #' This adds species specific trait informaition compiled from the following sources:
  #' VT Fish Traits
  #' Emmanuel Frimpong, and Paul L. Angermeier, 200811, Fish Traits Database: USGS,  https://doi.org/10.5066/F7WD3ZH8.
  #' Accessed from USGS sciencebase through https://www.sciencebase.gov/catalog/item/5a7c6e8ce4b00f54eb2318c0 on 2019-02-11
  #' 
  #' USGS Tolerance Data
  #' M.R. Meador and D.M. Carlisle.  2007.  Quantifying tolerance indicator values for common fish species of the United States. Ecological Indicators, 7:329-338.
  #' Available from USGS Ecological National Synthesis (ENS) Project "Fish Traits & Tolerance Data"  https://water.usgs.gov/nawqa/ecology/data.html 
  #' Specifically <https://water.usgs.gov/nawqa/ecology/pubs/FishToleranceIndicatorValuesTables.xls> (accessed 2019-02-21)
  #' 
  #' Additional Tolerance Class Data was communicated directly from Dr. M.R. Meador and filled in missing values for the following fish:
  #' BGB, BUD, CAP, DUD, FRM, MUD, ORD, RDS, SES, SHD, SLD, SLM, SRD, SUM, SVS, WHS, YLB
  #' 
  #' Calculated binary Values from VT Traits
  #' HERBIVORE = "1" if ALGPHYTO or MACVASCU or DETRITUS = 1 ,  "0" if none of them = 1
  #' OMNIVORE = "1" if more than one of HERBIVORE INVLVFSH FSHCRCRB BLOOD EGGS OTHER = 1,"0" if only one =1
  #' LITHOPHILIC = "1" if GRAVEL or COBBLE or BOULDER = 1, "0" if none of them = 1
  #' BENTHIC_INSECTIVORE = "1" if BENTHIC and INVLVFSH = 1 , "0" if one or both = 0
  #' 
  #' MIN and MAXTEMP values were added for WHS and CAP as -8.9 and 28.9 for the 30 year ave min (Jan) and ave max (July) from NOAA records for Champaign, IL
  
  
  il_fish_traits <- read.csv(paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Fish/Data/Illinois_fish_traits_complete.csv"), na = "", stringsAsFactors = F)
  il_fish_traits$Native_Intolerant <- ifelse(il_fish_traits$Nonnative == '0' & il_fish_traits$Tolerance_Class == 'INTOLERANT', 1, 0)
  
  fish_table <- species_count_data %>% 
    select(c(Site_ID, Fish_Species_Code, Fish_Species_Count))%>%
    left_join(il_fish_traits, by = 'Fish_Species_Code')
}

fish_table <- add_traits_to_data(fish_data)
fish_table_full <- left_join(fish_table, id)
fish_table_full$Year <- lubridate::year(fish_table_full$Event_Date)


#### Unique number of identified species ####
unique_fish <- fish_table %>% 
  filter(Hybrid == 0, Unidentified_Species == 0) %>% 
  summarise(unique_species = n_distinct(Fish_Species_Code))


unique_fish_by_year<- fish_table_full %>% 
  filter(Hybrid == 0, 
         Unidentified_Species == 0) %>%
  group_by(Year) %>% 
  summarise(richness = n_distinct(Fish_Species_Code))

### Species count information on a total and per site basis ####
fish_species_frequency_count <- fish_table %>%
  filter(Hybrid == 0, Unidentified_Species == 0) %>% 
  group_by(Fish_Species_Code, Fish_Species_Common) %>% 
  summarise(Fish_Species_Total_Count = sum(Fish_Species_Count),
            site_count_per_species = n())
write_csv(fish_species_frequency_count, path = paste0(analysis_path,"/fish-species-frequency-summary-1320.csv"))
