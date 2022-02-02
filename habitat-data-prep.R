library(tidyverse)
library(odbc)
library(DBI)
library(docstring)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Fish/PhaseIII_Analysis")


#### Read Habitat data IN from Access Db Query ####

### Connect to Database with odbc ###
odbcListDrivers() # to get a list of the drivers your computer knows about, be sure there is one for MS Access
# con <- dbConnect(odbc::odbc(), "Testing_Database")
con <- dbConnect(odbc::odbc(), "2019_CREP_Database")
options(odbc.batch_rows = 1) # Must be defined as 1 for Access batch writing or appending to tables.
dbListTables(con) # To get the list of tables in the database

hab <- as_tibble(tbl(con, "Habitat_Metrics_Matrix"))

dbDisconnect(con)

hab$Reach_Name <- stringr::str_remove(hab$Reach_Name, "[:blank:]*$")
hab$PU_Gap_Code  <- stringr::str_remove(hab$PU_Gap_Code, "[:blank:]*$")

hab_data <- hab %>%
  filter(lubridate::year(Event_Date) < 2021)

#### Write Site_ID to match Fish data to Habitat data ####
hab_data$Site_ID <-paste(str_replace_all(hab_data$Reach_Name, "[:blank:]", ""), str_replace_all(hab_data$Event_Date,"-",""), sep = "_")

#### Write to File ####
# write_csv(hab_data, file = paste0(analysis_path, "/Habitat_Metrics_CREP_2013-2020_P3FR.csv"))

id <- read_csv(file = paste0(analysis_path, "/id_table_CREP_2013-2020_P3FR.csv"))


df_hab <- id %>% 
  left_join(hab_data)

df_hab$Year <- as.factor(lubridate::year(df_hab$Event_Date))
df_hab$Site_Type <- as.factor(df_hab$Site_Type)

df_hab <- df_hab %>% 
  select(1:4,117,5,116,6:115)

df_hab <- df_hab %>% 
  mutate(Phase = if_else(Year == 2013|Year == 2014|Year == 2015, 'Phase I', 
                         if_else(Year == 2016|Year == 2017, 'Phase II', 'Phase III'))) %>% 
  select(1:5,118,6:117)

