library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")

otu_df <- read_csv(file = "~/OTU_Example.csv" )  


otu <- otu_df %>% 
  group_by(OTU) %>% 
  pivot_wider(names_from = Lab_Sample_ID, values_from=Abundance)
