library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")


#### Fix Missing IBIs for Phase III Report
# As of 12/23/2021 there are 3 sites missing from the compiles IBI list. This is a script to prepare the data for computing those IBIs

### Missing site information
# WO28_20140820
# kasky175
# WO28
# 2014-08-20
# 
# 
# Copper13_20160823
# kasky272
# Copper 13
# 2016-08-23
# 
# Copper13_20170720
# kasky272
# Copper 13
# 2017-07-20

# Bring in Fish_table_full from fish species summaries 

missing_ibi <- fish_table_full %>% 
  filter(Site_ID == "WO28_20140820"|Site_ID == "Copper13_20160823"|Site_ID == "Copper13_20170720")

write_csv(missing_ibi, path = paste0(network_path, "/Data/Data_IN/IBI/Fish_2014_2017_missing_IBI.csv"))
