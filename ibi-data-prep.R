library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")


#### IBI REconciling

ibi_20 <- readxl::read_xlsx(path = paste0(network_path,"/Data/Data_OUT/IBI/IBI_Output_2020_LEHrecalculate.xlsx"))
ibi_19 <- readxl::read_xlsx(path = paste0(network_path,"/Data/Data_OUT/IBI/IBI_Output_2019.xlsx"))
ibi_18 <- readxl::read_xlsx(path = paste0(network_path,"/Data/Data_OUT/IBI/IBI_Output_2018.xlsx"))
ibi_1317 <- readxl::read_xlsx(path = paste0(network_path,"/Data/Data_OUT/IBI/IBI_Attributes_Metrics_Score_2013-2017.xlsx"))

# Combine then pull just IBI Score, Reach Name, Event Date, PUGap_Code

names(ibi_1317) <- str_to_title(names(ibi_1317))

ibi_1317 <- ibi_1317 %>% 
  rename(
    Reach_Name = Reach_name,
    'Stream Width (ft)' = 'Stream Width (Ft)',
    IBI = Ibi,
    'Sample Date' = Date
  ) %>% 
  mutate(PU_Gap_Code = paste0("kasky", Pu_gap_code)) %>% 
  select(-c(Surveyyear, Surveyevent, Reachname, Pu_gap_code))


ibi_all <- ibi_18 %>% 
  full_join(ibi_19) %>% 
  full_join(ibi_20) %>% 
  select(-c(`IBI Region`, `IEPA Station`)) %>% 
  full_join(ibi_1317)

ibi_all$PU_Gap_Code <- str_replace(ibi_all$PU_Gap_Code, "KASKY", "kasky")
ibi_all$Reach_Name <- str_replace(ibi_all$Reach_Name, "KASKY", "kasky")

write_csv(ibi_all, paste0(network_path, "/Data/Data_IN/IBI/IBI_Output_2013_2020.csv"))


