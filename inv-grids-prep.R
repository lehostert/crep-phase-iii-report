library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")


inv18 <- readxl::read_xlsx(path = paste0(network_path, "/EcoAnalysts Invert Tables Copy/2018/Invert_Sample_Labels_2018.xlsx"), sheet = 1)
inv19 <- readxl::read_xlsx(path = paste0(network_path, "/EcoAnalysts Invert Tables Copy/2019/CREP_Invert_Sample_List_2019.xlsx"), sheet = 2)
inv20 <- readxl::read_xlsx(path = paste0(network_path, "/EcoAnalysts Invert Tables Copy/2020/macroinvertebrate_labprocessing_2020.xlsx"), sheet = 1)

inv18 <- inv18 %>% 
  select(PU_Gap_Code, Reach_Name, Event_Date, Grids, Notes) %>% 
  unique()

inv19 <- inv19 %>% 
  select(PU_Gap_Code, Reach_Name, Event_Date, Grids) %>% 
  unique()

inv20 <- inv20 %>% 
  rename(PU_Gap_Code = PUGap_Code,
         Reach_Name = Site_Name) %>% 
  select(PU_Gap_Code, Reach_Name, Event_Date, Grids) %>% 
  unique()

inv_phase_iii <- inv18 %>% 
  full_join(inv19) %>% 
  full_join(inv20)

inv_phase_iii$Reach_Name <- str_replace(inv_phase_iii$Reach_Name, "copper", "Copper ")
inv_phase_iii$Reach_Name <- str_replace(inv_phase_iii$Reach_Name, "isws", "ISWS")
inv_phase_iii$Reach_Name <- str_replace(inv_phase_iii$Reach_Name, "x", "X")


write_csv(inv_phase_iii, path = paste0(network_path, "/EcoAnalysts Invert Tables Copy/2020/macroinvertebrate_grids_2018-2020.csv"))
          
          