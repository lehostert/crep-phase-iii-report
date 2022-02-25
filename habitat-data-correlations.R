library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison.ad.uillinois.edu", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/StreamEcologyLab/CREP")
analysis_path <- paste0(network_prefix,"/ResearchData/Groups/StreamEcologyLab/CREP/Analysis/Habitat/PhaseIII_Analysis")
plot_folder<- paste0(analysis_path, "/Plots_Reduced")

set.seed(2022)

df_habitat <- read_csv(file = paste0(analysis_path, "/Data/Habitat_Metrics_CREP_2013-2020_P3FR_basedOnIHI.csv"))
df_habitat$Site_Type <- as.factor(df_habitat$Site_Type)

#### Create Functions ####

l.correlation <- function(met, fact, type){
  x <-df_habitat %>%  filter(Site_Type == type) %>% pull(met)
  y <-df_habitat %>%  filter(Site_Type == type) %>% pull(fact) 
  cor.test(x,y, method = "pearson") %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
}


# l.wilcox2 <- function(dat, met, type1, type2){
#   x <- dat %>%  filter(Site_Type == type1)%>% pull(met)
#   y <- dat %>%  filter(Site_Type == type2)%>% pull(met)
#   res_wil <- wilcox.test(x, y) %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
#   return(res_wil)
# }

l.wilcox <- function(dat, met, fact){
  x <- dat %>% pull(met)
  y <- dat %>% pull(fact)
  res_wil <- wilcox.test(x~ y, data = dat, conf.int = TRUE) %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
  return(res_wil)
}


l.pttest <- function(df, met, group1, group2){
  x <- df %>%  filter(Metric == met) %>% pull(group1)
  y <- df %>%  filter(Metric == met) %>% pull(group2) 
  t.test(x,y, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
}

#### Get Metric List ####
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
  
#### Random ####
res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.correlation(met, "Year", "random")
  print(i)
  print(met)
}

res_random <- do.call(rbind, res)

res_random <- res_random %>% 
  mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 3)))

writexl::write_xlsx(res_random, path = paste0(plot_folder,"/habitat_cor_random.xlsx"))

#### Paired ####
pair_list <- read_csv(file = paste0(analysis_path, "/Data/Paired_Site_Groupings.csv"))

df_habitat_paired <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "paired") %>%
  left_join(pair_list) %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>%
  select(-c(PU_Gap_Code, Reach_Name, Site_ID, Phase, Site_Type, Event_Date, Visual_Water_Clarity)) %>% 
  group_by(Pair_Number) %>% 
  pivot_wider(names_from = CRP_Class, values_from= Value)

## Paired t-test for 2016
df_habitat_paired_16 <- df_habitat_paired %>%
  ungroup() %>% 
  filter(Year == 2016)

res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.ttest(df_habitat_paired_16, met, "low", "high")
  print(i)
  print(met)
}

res_pair16 <- do.call(rbind, res)

res_pair16 <- res_pair16 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_pair16, path = paste0(plot_folder,"/habitat_pttest_paired16.xlsx"))

## Paired t-test for 2017
df_habitat_paired_17 <- df_habitat_paired %>%
  ungroup() %>% 
  filter(Year == 2017)

res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.ttest(df_habitat_paired_17, met, "low", "high")
  print(i)
  print(met)
}

res_pair17 <- do.call(rbind, res)

res_pair17 <- res_pair17 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_pair17, path = paste0(plot_folder,"/habitat_pttest_paired17.xlsx"))

## Paired t-test for 2018
df_habitat_paired_18 <- df_habitat_paired %>%
  ungroup() %>% 
  filter(Year == 2018)

res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.ttest(df_habitat_paired_18, met, "low", "high")
  print(i)
  print(met)
}

res_pair18 <- do.call(rbind, res)

res_pair18 <- res_pair18 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_pair18, path = paste0(plot_folder,"/habitat_pttest_paired18.xlsx"))

# Only 1 pair was sampled in 2020 therefore there is not enough to be able to test. 

#### Sensitive Species ####

df_habitat_sensitive <- df_habitat %>% 
  filter(Site_Type == "copper")

## Paired t-tests within sites. Comparing earliest sampling (2016) with latest sampling (2019)
df_habitat_ss <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "copper",
         Year == 2016|Year == 2019|Year == 2017) %>%
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>%
  select(-c(PU_Gap_Code, Site_ID, Phase, Site_Type, Event_Date, Visual_Water_Clarity)) %>% 
  group_by(Reach_Name) %>% 
  pivot_wider(names_from = Year, values_from= Value) %>% 
  rename(Year1 ="2016", Year2  = "2017", Year3 = "2019") %>% 
  ungroup() %>% 
  select(-c(Year1)) %>% 
  drop_na()

metric_list_ss <- unique(df_habitat_ss$Metric) 

res <- NULL
i <- 0
for (met in metric_list_ss) {
  i <- i+1
  res[[i]] <- l.ttest(df_habitat_ss, met, "Year2", "Year3")
  print(i)
  print(met)
}

res_ss <- do.call(rbind, res)

res_ss <- res_ss %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 3)))

writexl::write_xlsx(res_ss, path = paste0(plot_folder,"/habitat_ttest_sensitive.xlsx"))

## Kruskal-Wallis Test whether or not there were differences between years
# df_habitat_ss_KW <- df_habitat %>%
#   ungroup() %>% 
#   filter(Site_Type == "copper",
#          Year != 2020,
#          Reach_Name != "Copper 17") 
# 
# df_habitat_ss_KW$Year <- ordered(df_habitat_ss_KW$Year ,
#                               levels = c("2016", "2017", "2019"))
# 
# res_ss_KW <- kruskal.test(IHI_Score ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "IHI Score")
# res_ss_KW[2,] <- kruskal.test(Diversity ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Diversity")
# res_ss_KW[3,] <- kruskal.test(Individuals ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Individuals")
# res_ss_KW[4,] <- kruskal.test(Percent_Catostomidae ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
# res_ss_KW[5,] <- kruskal.test(Percent_Intolerant_Taxa ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
# res_ss_KW[6,] <- kruskal.test(Percent_Moderate_Tolerance_Taxa ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
# res_ss_KW[7,] <- kruskal.test(Percent_Tolerant_Taxa ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")
# 
# res_ss_WX <- pairwise.wilcox.test(df_habitat_ss_KW$IHI_Score, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "IHI Score")
# res_ss_WX2 <- pairwise.wilcox.test(df_habitat_ss_KW$Diversity , df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Diversity")
# res_ss_WX3 <- pairwise.wilcox.test(df_habitat_ss_KW$Individuals, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Individuals")
# res_ss_WX4 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Catostomidae, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
# res_ss_WX5 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Intolerant_Taxa, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Tax")
# res_ss_WX6 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Moderate_Tolerance_Taxa, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
# res_ss_WX7 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Tolerant_Taxa, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")
# 
# res_ss_WX <- rbind(res_ss_WX, res_ss_WX2, res_ss_WX3, res_ss_WX4, res_ss_WX5, res_ss_WX6, res_ss_WX7)
# remove(list = c("res_ss_WX2", "res_ss_WX3", "res_ss_WX4","res_ss_WX5","res_ss_WX6","res_ss_WX7"))
# 
# writexl::write_xlsx(res_ss_KW, path = paste0(plot_folder,"/fish_kruskalwallis_sensitive.xlsx"))
# writexl::write_xlsx(res_ss_WX, path = paste0(plot_folder,"/fish_wilcox_sensitive.xlsx"))

#### LD ####

res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.correlation(met, "Year", "less_disturbed")
  print(i)
  print(met)
}

res_ld <- do.call(rbind, res)

res_ld <- res_ld %>% 
  mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_ld, path = paste0(plot_folder,"/habitat_cor_ld.xlsx"))

### Test whether there is a difference between ld and random

l.shapiro <- function(dat, met, sitetype){
  d <- dat %>%  filter(Site_Type == sitetype) %>% pull(met)
  shapiro.test(d) %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
}

## Test for normality in LD Sites
res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.shapiro(df_habitat, met, "less_disturbed")
  print(i)
  print(met)
}

res_ld_shapiro <- do.call(rbind, res)
res_ld_shapiro <- res_ld_shapiro %>% mutate(p_value = formatC(p.value, digits = 4, format = "f"))


## Test for normality in Random Sites
res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.shapiro(df_habitat, met, "random")
  print(i)
  print(met)
}

res_random_shapiro <- do.call(rbind, res)
res_random_shapiro <- res_rand_shapiro  %>% mutate(p_value = formatC(p.value, digits = 4, format = "f"))

## Only pH is normal so continue with non-parametric tests

# Create dataframe with only LD and random
df_habitat_ldr <- df_habitat %>% 
  filter(Site_Type == "less_disturbed"|Site_Type == "random")


df_habitat_ldr_summary <- df_habitat %>% 
  filter(Site_Type == "less_disturbed"|Site_Type == "random") %>% 
  pivot_longer(cols = QHEI_Score:Turbidity, names_to = "Metric", values_to = "Value", values_drop_na = F) %>% 
  group_by(Metric) %>% 
  mutate(Metric = str_replace_all(Metric, "_", " ")) %>% 
  summarize(Total_Sites = n_distinct(Site_ID),
            N.metric = sum(!is.na(Value)),
            N.NA = sum(is.na(Value)),
            SD.metric =sd(Value, na.rm = TRUE))   


## Test all metrics with Mann-Whitney U/ Wilcoxon Rank Sum Test
res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.wilcox(df_habitat_ldr, met, "Site_Type")
  print(i)
  print(met)
}

res_ldr_wilcox <- do.call(rbind, res)
res_ldr_wilcox <- res_ldr_wilcox %>% 
  left_join(df_habitat_ldr_summary) %>% 
  mutate(p_value = formatC(p.value, digits = 4, format = "f"),
         Z.stat = qnorm(p.value/2),
         effect.size = abs(Z.stat)/sqrt(N.metric),
         report = paste0("(Z= ",formatC(Z.stat, digits = 3, format = "f"),", p=",p_value, ")"))

writexl::write_xlsx(res_ldr_wilcox, path = paste0(plot_folder,"/habitat_wilcox_ldr.xlsx"))
#### ISWS ####
res <- NULL
i <- 0
for (met in metric_list) {
  i <- i+1
  res[[i]] <- l.correlation(met, "Year", "less_disturbed")
  print(i)
  print(met)
}

res_isws <- do.call(rbind, res)

res_isws <- res_isws %>% 
  mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

writexl::write_xlsx(res_isws, path = paste0(plot_folder,"/habitat_cor_isws.xlsx"))
