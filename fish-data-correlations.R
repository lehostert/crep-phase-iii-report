library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Fish/PhaseIII_Analysis")
plot_folder<- paste0(analysis_path, "/Plots_Reduced")

set.seed(2022)


## Get df_fish from fish-data-draft-summaries.R
df_fish <- read_csv(file = paste0(analysis_path, "/Data/Fish_Metrics_CREP_2013-2020_P3FR_Selected.csv"))

#### Random ####
df_fish_random <- df_fish %>% 
  filter(Site_Type == "random")

res_random <- cor.test(df_fish_random$IBI, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "IBI")
res_random[2,] <- cor.test(df_fish_random$Diversity, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_random[3,] <- cor.test(df_fish_random$Individuals, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_random[4,] <- cor.test(df_fish_random$Percent_Catostomidae, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_random[5,] <- cor.test(df_fish_random$Percent_Intolerant_Taxa, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_random[6,] <- cor.test(df_fish_random$Percent_Moderate_Tolerance_Taxa, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_random[7,] <- cor.test(df_fish_random$Percent_Tolerant_Taxa, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")
res_random[8,] <- cor.test(df_fish_random$Channel_Order, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Channel_Order")
res_random[9,] <- cor.test(df_fish_random$Link, df_fish_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Link")

res_random <- res_random %>% 
  mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 3)))

writexl::write_xlsx(res_random, path = paste0(plot_folder,"/fish_cor_random.xlsx"))

#### Paired ####
pair_list <- read_csv(file = paste0(analysis_path, "/Data/Paired_Site_Groupings.csv"))

df_fish_paired <- df_fish %>%
  ungroup() %>% 
  filter(Site_Type == "paired") %>%
  left_join(pair_list) %>% 
  pivot_longer(cols = IBI:Percent_Tolerant_Taxa, names_to = "Metric", values_to = "Value", values_drop_na = F) %>%
  select(-c(Reach_Name, Site_ID, Phase, Site_Type, Event_Date, Channel_Order, Link)) %>% 
  group_by(PU_Gap_Code) %>% 
  pivot_wider(names_from = Year, values_from= Value)

df_fish_paired_16 <- df_fish %>%
  ungroup() %>% 
  filter(Site_Type == "paired",
         Year == 2016) %>%
  left_join(pair_list) %>% 
  filter(Pair_Number != 2)

res_pair16 <- t.test(IBI ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IBI")
res_pair16[2,] <- t.test(Diversity ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_pair16[3,] <- t.test(Individuals ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_pair16[4,] <- t.test(Percent_Catostomidae ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_pair16[5,] <- t.test(Percent_Intolerant_Taxa ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_pair16[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_pair16[7,] <- t.test(Percent_Tolerant_Taxa ~ CRP_Class, data = df_fish_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_pair16 <- res_pair16 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))
writexl::write_xlsx(res_pair16, path = paste0(plot_folder,"/fish_cor_paired16.xlsx"))

df_fish_paired_17 <- df_fish %>%
  ungroup() %>% 
  filter(Site_Type == "paired",
         Year == 2017) %>%
  left_join(pair_list)

res_pair17 <- t.test(IBI ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IBI")
res_pair17[2,] <- t.test(Diversity ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_pair17[3,] <- t.test(Individuals ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_pair17[4,] <- t.test(Percent_Catostomidae ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_pair17[5,] <- t.test(Percent_Intolerant_Taxa ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_pair17[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_pair17[7,] <- t.test(Percent_Tolerant_Taxa ~ CRP_Class, data = df_fish_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_pair17 <- res_pair17 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))
writexl::write_xlsx(res_pair17, path = paste0(plot_folder,"/fish_cor_paired17.xlsx"))


df_fish_paired_18 <- df_fish %>%
  ungroup() %>% 
  filter(Site_Type == "paired",
         Year == 2018) %>%
  left_join(pair_list) %>% 
  filter(Pair_Number != 4)

res_pair18 <- t.test(IBI ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IBI")
res_pair18[2,] <- t.test(Diversity ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_pair18[3,] <- t.test(Individuals ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_pair18[4,] <- t.test(Percent_Catostomidae ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_pair18[5,] <- t.test(Percent_Intolerant_Taxa ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_pair18[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_pair18[7,] <- t.test(Percent_Tolerant_Taxa ~ CRP_Class, data = df_fish_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_pair18 <- res_pair18 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))
writexl::write_xlsx(res_pair18, path = paste0(plot_folder,"/fish_cor_paired18.xlsx"))

# Only 1 pair was sampled in 2020 therefore there is not enough to be able to test. 

#### Sensitive Species ####

df_fish_sensitive <- df_fish %>% 
  filter(Site_Type == "copper")

## Pearson's R with correlation over years nothing significant and not what was asked.
# res_sensitive <- cor.test(df_fish_sensitive$IBI, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "IBI")
# res_sensitive[2,] <- cor.test(df_fish_sensitive$Diversity, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Diversity")
# res_sensitive[3,] <- cor.test(df_fish_sensitive$Individuals, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Individuals")
# res_sensitive[4,] <- cor.test(df_fish_sensitive$Percent_Catostomidae, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
# res_sensitive[5,] <- cor.test(df_fish_sensitive$Percent_Intolerant_Taxa, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
# res_sensitive[6,] <- cor.test(df_fish_sensitive$Percent_Moderate_Tolerance_Taxa, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
# res_sensitive[7,] <- cor.test(df_fish_sensitive$Percent_Tolerant_Taxa, df_fish_sensitive$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

## T-test comparing earliest sampling (2016) with latest sampling (2019)
# res_sensitive <- t.test(IBI ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IBI")
# res_sensitive[2,] <- t.test(Diversity ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
# res_sensitive[3,] <- t.test(Individuals ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
# res_sensitive[4,] <- t.test(Percent_Catostomidae ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
# res_sensitive[5,] <- t.test(Percent_Intolerant_Taxa ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
# res_sensitive[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
# res_sensitive[7,] <- t.test(Percent_Tolerant_Taxa ~ Phase, data = df_fish_sensitive, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")
# 
# 
# res_sensitive <- res_sensitive %>% 
#   mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

# writexl::write_xlsx(res_sensitive, path = paste0(plot_folder,"/fish_cor_sensitive.xlsx"))

## Paired t-tests within sites. Comparing earliest sampling (2016) with latest sampling (2019)

df_fish_ss <- df_fish %>%
  ungroup() %>% 
  filter(Site_Type == "copper",
         Year == 2016|Year == 2019) %>%
  pivot_longer(cols = IBI:Percent_Tolerant_Taxa, names_to = "Metric", values_to = "Value", values_drop_na = F) %>%
  select(-c(PU_Gap_Code, Site_ID, Phase, Site_Type, Event_Date, Channel_Order, Link)) %>% 
  group_by(Reach_Name) %>% 
  pivot_wider(names_from = Year, values_from= Value) %>% 
  rename(Year1 ="2016", Year3 = "2019")

df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "IBI")
res_ss <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IBI")
df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "Diversity")
res_ss[2,] <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "Individuals")
res_ss[3,] <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "Percent_Catostomidae")
res_ss[4,] <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "Percent_Intolerant_Taxa")
res_ss[5,] <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "Percent_Moderate_Tolerance_Taxa")
res_ss[6,] <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
df_fish_ss_metric <- df_fish_ss %>% ungroup() %>% filter(Metric == "Percent_Tolerant_Taxa")
res_ss[7,] <- t.test(df_fish_ss_metric$Year1, df_fish_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ss <- res_ss %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

remove(df_fish_ss_metric, df_fish_ss_ibi)
writexl::write_xlsx(res_ss, path = paste0(plot_folder,"/fish_cor_sensitive.xlsx"))

## Kruskal-Wallis Test whether or not there were differences between years
df_fish_ss_KW <- df_fish %>%
  ungroup() %>% 
  filter(Site_Type == "copper",
         Year != 2020,
         Reach_Name != "Copper 17") 

df_fish_ss_KW$Year <- ordered(df_fish_ss_KW$Year ,
                         levels = c("2016", "2017", "2019"))

res_ss_KW <- kruskal.test(IBI ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "IBI")
res_ss_KW[2,] <- kruskal.test(Diversity ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_ss_KW[3,] <- kruskal.test(Individuals ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_ss_KW[4,] <- kruskal.test(Percent_Catostomidae ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_ss_KW[5,] <- kruskal.test(Percent_Intolerant_Taxa ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_ss_KW[6,] <- kruskal.test(Percent_Moderate_Tolerance_Taxa ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_ss_KW[7,] <- kruskal.test(Percent_Tolerant_Taxa ~ Year, data = df_fish_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ss_WX <- pairwise.wilcox.test(df_fish_ss_KW$IBI, df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "IBI")
res_ss_WX2 <- pairwise.wilcox.test(df_fish_ss_KW$Diversity , df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_ss_WX3 <- pairwise.wilcox.test(df_fish_ss_KW$Individuals, df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_ss_WX4 <- pairwise.wilcox.test(df_fish_ss_KW$Percent_Catostomidae, df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_ss_WX5 <- pairwise.wilcox.test(df_fish_ss_KW$Percent_Intolerant_Taxa, df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Tax")
res_ss_WX6 <- pairwise.wilcox.test(df_fish_ss_KW$Percent_Moderate_Tolerance_Taxa, df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_ss_WX7 <- pairwise.wilcox.test(df_fish_ss_KW$Percent_Tolerant_Taxa, df_fish_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ss_WX <- rbind(res_ss_WX, res_ss_WX2, res_ss_WX3, res_ss_WX4, res_ss_WX5, res_ss_WX6, res_ss_WX7)
remove(list = c("res_ss_WX2", "res_ss_WX3", "res_ss_WX4","res_ss_WX5","res_ss_WX6","res_ss_WX7"))

writexl::write_xlsx(res_ss_KW, path = paste0(plot_folder,"/fish_kruskalwallis_sensitive.xlsx"))
writexl::write_xlsx(res_ss_WX, path = paste0(plot_folder,"/fish_wilcox_sensitive.xlsx"))

#### LD ####

df_fish_ld <- df_fish %>% 
  filter(Site_Type == "less_disturbed")

res_ld <- cor.test(df_fish_ld$IBI, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "IBI")
res_ld[2,] <- cor.test(df_fish_ld$Diversity, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_ld[3,] <- cor.test(df_fish_ld$Individuals, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_ld[4,] <- cor.test(df_fish_ld$Percent_Catostomidae, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_ld[5,] <- cor.test(df_fish_ld$Percent_Intolerant_Taxa, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_ld[6,] <- cor.test(df_fish_ld$Percent_Moderate_Tolerance_Taxa, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_ld[7,] <- cor.test(df_fish_ld$Percent_Tolerant_Taxa, df_fish_ld$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ld <- res_ld %>% 
  mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_ld, path = paste0(plot_folder,"/fish_cor_ld.xlsx"))

## Test whether there is a difference between ld and random

df_fish_ldr <- df_fish %>% 
  filter(Site_Type == "less_disturbed"|Site_Type == "random")

with(df_fish_ldr, shapiro.test(Individuals[Site_Type == "less_disturbed"]))# p = 0.02596
with(df_fish_ldr, shapiro.test(Individuals[Site_Type == "random"]))# p = <2.2e-16
###  non normal so will continue with a non-parametric test
res_ldr <- wilcox.test(Individuals ~ Site_Type, data = df_fish_ldr)
res_ldr

#### ISWS ####

df_fish_isws <- df_fish %>% 
  filter(Site_Type == "ISWS")

res_isws <- cor.test(df_fish_isws$IBI, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "IBI")
res_isws[2,] <- cor.test(df_fish_isws$Diversity, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_isws[3,] <- cor.test(df_fish_isws$Individuals, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_isws[4,] <- cor.test(df_fish_isws$Percent_Catostomidae, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_isws[5,] <- cor.test(df_fish_isws$Percent_Intolerant_Taxa, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_isws[6,] <- cor.test(df_fish_isws$Percent_Moderate_Tolerance_Taxa, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_isws[7,] <- cor.test(df_fish_isws$Percent_Tolerant_Taxa, df_fish_isws$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_isws <- res_isws %>% 
  mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_isws, path = paste0(plot_folder,"/fish_cor_isws.xlsx"))

# new_list <- vector(metric_list)
