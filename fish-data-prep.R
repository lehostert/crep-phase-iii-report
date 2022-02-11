library(tidyverse)
library(odbc)
library(DBI)
library(docstring)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/PhaseIII_Analysis")

#### Read all fish data in ####

#### Connect to Database ####
### with odbc
odbcListDrivers() # to get a list of the drivers your computer knows about, be sure there is one for MS Access
# con <- dbConnect(odbc::odbc(), "Testing_Database")
con <- dbConnect(odbc::odbc(), "2019_CREP_Database")
options(odbc.batch_rows = 1) # Must be defined as 1 for Access batch writing or appending to tables See below.
dbListTables(con) # To get the list of tables in the database

loc <- as_tibble(tbl(con, "Established_Locations"))
fmd <- as_tibble(tbl(con, "Fish_Metadata"))
fish <- as_tibble(tbl(con, "Fish_Abundance"))
pair_list <- as_tibble(tbl(con, "Paired_Site_Groupings"))

dbDisconnect(con)

fish$Reach_Name <- stringr::str_remove(fish$Reach_Name, "[:blank:]*$")
fish$PU_Gap_Code  <- stringr::str_remove(fish$PU_Gap_Code, "[:blank:]*$")

fish_data <- fish %>%
  drop_na(Fish_Species_Code) %>% 
  filter(lubridate::year(Event_Date) < 2021) %>% 
  group_by(PU_Gap_Code, Reach_Name, Event_Date, Fish_Species_Code) %>% 
  summarise(Fish_Species_Count = n()) %>% 
  ungroup()

fish_data$Site_ID <-paste(str_replace_all(fish_data$Reach_Name, "[:blank:]", ""), str_replace_all(fish_data$Event_Date,"-",""), sep = "_")

#### Add fish traits ####
# #Before moving on the fish count data must have the following 3 fields: "Site_ID" "Fish_Species_Code" and "Fish_Species_Count"
# #names(fish_data)

id_table <- fish_data %>%
  left_join(loc) %>% 
  select(Site_ID, PU_Gap_Code, Reach_Name, Event_Date, Site_Type) %>%
  unique()

### Manually updated kasky1038 because the site type was different for 2019 even though the sampled location was similar to 2016& 2017
# id_table$Site_Type[7] <- "less_disturbed"

write_csv(id_table, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/PhaseIII_Analysis/id_table_CREP_2013-2020_P3FR.csv"))
write_csv(fish_data, path = paste0(analysis_path,"/Fish_Abundance_Data_CREP_2013-2020_P3FR.csv"))
write_csv(fmd, path = paste0(analysis_path,"/Fish_Collection_Metadata_P3FR.csv"))

## RUN Per_Site_Fish_Metric_Tibble.R ###

write.csv(site_metric_tibble, file = paste0(analysis_path, "/Fish_Metrics_CREP_2013-2020_P3FR.csv"), na = "0", row.names = F)
### Something is wrong with the Site_ID names for this files but the values are the same as earlier. 


##### Clean it up ##### 
network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Fish/PhaseIII_Analysis")


metrics <- read_csv(file = paste0(analysis_path, "/Fish_Metrics_CREP_2013-2020.csv"))
id <- read_csv(file = paste0(analysis_path, "/id_table_CREP_2013-2020_P3FR.csv"))
ibi <- read_csv(file = paste0(network_path, "/Data/Data_IN/IBI/IBI_Output_2013_2020.csv"))

### Simplify IBI ###
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

write_csv(df, path = paste0(analysis_path, "/Data/Habitat_Metrics_CREP_2013-2020_P3FR_All.csv"))
