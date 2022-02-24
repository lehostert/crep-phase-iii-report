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
  x <- dat %>%  filter(Site_Type == type1)%>% pull(met)
  y <- dat %>%  filter(Site_Type == type2)%>% pull(met)
  res_wil <- wilcox.test(x, y) %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
  return(res_wil)
}

l.wilcox <- function(dat, met, fact){
  x <- dat %>% pull(met)
  y <- dat %>% pull(fact)
  res_wil <- wilcox.test(x~ y, data = dat, conf.int = TRUE) %>% broom::tidy() %>% mutate(Metric = str_replace_all(met, "_", " "))
  return(res_wil)
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

# df_habitat_random <- df_habitat %>% 
#   filter(Site_Type == "random")
# 
# res_random <- cor.test(df_habitat_random$Ammonia, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Ammonia")
# res_random[2,] <- cor.test(df_habitat_random$Conductivity, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Conductivity")
# res_random[3,] <- cor.test(df_habitat_random$DO, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "DO")
# res_random[4,] <- cor.test(df_habitat_random$DO_Saturation, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "DO Saturation")
# res_random[5,] <- cor.test(df_habitat_random$IHI_Score, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "IHI Score")
# res_random[6,] <- cor.test(df_habitat_random$Mean_Depth, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Mean Depth")
# res_random[7,] <- cor.test(df_habitat_random$Mean_Width, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Mean Width")
# res_random[8,] <- cor.test(df_habitat_random$Nitrate, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Nitrate")
# res_random[9,] <- cor.test(df_habitat_random$Orthophosphate, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Orthophosphate")
# res_random[10,] <- cor.test(df_habitat_random$pH, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "pH")
# res_random[11,] <- cor.test(df_habitat_random$QHEI_Score, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "QHEI Score")
# res_random[12,] <- cor.test(df_habitat_random$Reach_Length, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Reach Length")
# res_random[13,] <- cor.test(df_habitat_random$Turbidity, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Turbidity")
# res_random[14,] <- cor.test(df_habitat_random$Water_Temperature, df_habitat_random$Year, method = "pearson") %>% broom::tidy() %>% mutate(Metric = "Water Temperature")
# 
# res_random <- res_random %>% 
#   mutate(summary = paste0("r(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 3)))

#### Paired ####
pair_list <- read_csv(file = paste0(analysis_path, "/Data/Paired_Site_Groupings.csv"))

df_habitat_paired <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "paired") %>%
  left_join(pair_list) %>% 
  pivot_longer(cols = IHI_Score:Percent_Tolerant_Taxa, names_to = "Metric", values_to = "Value", values_drop_na = F) %>%
  select(-c(Reach_Name, Site_ID, Phase, Site_Type, Event_Date, Channel_Order, Link)) %>% 
  group_by(PU_Gap_Code) %>% 
  pivot_wider(names_from = Year, values_from= Value)

df_habitat_paired_16 <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "paired",
         Year == 2016) %>%
  left_join(pair_list) %>% 
  filter(Pair_Number != 2)

res_pair16 <- t.test(IHI_Score ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IHI Score")
res_pair16[2,] <- t.test(Diversity ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_pair16[3,] <- t.test(Individuals ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_pair16[4,] <- t.test(Percent_Catostomidae ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_pair16[5,] <- t.test(Percent_Intolerant_Taxa ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_pair16[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_pair16[7,] <- t.test(Percent_Tolerant_Taxa ~ CRP_Class, data = df_habitat_paired_16, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_pair16 <- res_pair16 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))
writexl::write_xlsx(res_pair16, path = paste0(plot_folder,"/habitat_cor_paired16.xlsx"))

df_habitat_paired_17 <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "paired",
         Year == 2017) %>%
  left_join(pair_list)

res_pair17 <- t.test(IHI_Score ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IHI Score")
res_pair17[2,] <- t.test(Diversity ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_pair17[3,] <- t.test(Individuals ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_pair17[4,] <- t.test(Percent_Catostomidae ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_pair17[5,] <- t.test(Percent_Intolerant_Taxa ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_pair17[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_pair17[7,] <- t.test(Percent_Tolerant_Taxa ~ CRP_Class, data = df_habitat_paired_17, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_pair17 <- res_pair17 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))
writexl::write_xlsx(res_pair17, path = paste0(plot_folder,"/habitat_cor_paired17.xlsx"))


df_habitat_paired_18 <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "paired",
         Year == 2018) %>%
  left_join(pair_list) %>% 
  filter(Pair_Number != 4)

res_pair18 <- t.test(IHI_Score ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IHI Score")
res_pair18[2,] <- t.test(Diversity ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_pair18[3,] <- t.test(Individuals ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_pair18[4,] <- t.test(Percent_Catostomidae ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_pair18[5,] <- t.test(Percent_Intolerant_Taxa ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_pair18[6,] <- t.test(Percent_Moderate_Tolerance_Taxa ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_pair18[7,] <- t.test(Percent_Tolerant_Taxa ~ CRP_Class, data = df_habitat_paired_18, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_pair18 <- res_pair18 %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))
writexl::write_xlsx(res_pair18, path = paste0(plot_folder,"/habitat_cor_paired18.xlsx"))

# Only 1 pair was sampled in 2020 therefore there is not enough to be able to test. 

#### Sensitive Species ####

df_habitat_sensitive <- df_habitat %>% 
  filter(Site_Type == "copper")

## Paired t-tests within sites. Comparing earliest sampling (2016) with latest sampling (2019)

df_habitat_ss <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "copper",
         Year == 2016|Year == 2019) %>%
  pivot_longer(cols = IHI_Score:Percent_Tolerant_Taxa, names_to = "Metric", values_to = "Value", values_drop_na = F) %>%
  select(-c(PU_Gap_Code, Site_ID, Phase, Site_Type, Event_Date, Channel_Order, Link)) %>% 
  group_by(Reach_Name) %>% 
  pivot_wider(names_from = Year, values_from= Value) %>% 
  rename(Year1 ="2016", Year3 = "2019")

df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "IHI Score")
res_ss <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "IHI Score")
df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "Diversity")
res_ss[2,] <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Diversity")
df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "Individuals")
res_ss[3,] <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Individuals")
df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "Percent_Catostomidae")
res_ss[4,] <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "Percent_Intolerant_Taxa")
res_ss[5,] <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "Percent_Moderate_Tolerance_Taxa")
res_ss[6,] <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
df_habitat_ss_metric <- df_habitat_ss %>% ungroup() %>% filter(Metric == "Percent_Tolerant_Taxa")
res_ss[7,] <- t.test(df_habitat_ss_metric$Year1, df_habitat_ss_metric$Year3, paired = TRUE) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ss <- res_ss %>% 
  mutate(summary = paste0("t(",parameter, ")= ",round(statistic,digits = 2),", p=", round(p.value,digits = 2)))

remove(df_habitat_ss_metric)
writexl::write_xlsx(res_ss, path = paste0(plot_folder,"/habitat_cor_sensitive.xlsx"))

## Kruskal-Wallis Test whether or not there were differences between years
df_habitat_ss_KW <- df_habitat %>%
  ungroup() %>% 
  filter(Site_Type == "copper",
         Year != 2020,
         Reach_Name != "Copper 17") 

df_habitat_ss_KW$Year <- ordered(df_habitat_ss_KW$Year ,
                              levels = c("2016", "2017", "2019"))

res_ss_KW <- kruskal.test(IHI_Score ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "IHI Score")
res_ss_KW[2,] <- kruskal.test(Diversity ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_ss_KW[3,] <- kruskal.test(Individuals ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_ss_KW[4,] <- kruskal.test(Percent_Catostomidae ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_ss_KW[5,] <- kruskal.test(Percent_Intolerant_Taxa ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Taxa")
res_ss_KW[6,] <- kruskal.test(Percent_Moderate_Tolerance_Taxa ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_ss_KW[7,] <- kruskal.test(Percent_Tolerant_Taxa ~ Year, data = df_habitat_ss_KW) %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ss_WX <- pairwise.wilcox.test(df_habitat_ss_KW$IHI_Score, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "IHI Score")
res_ss_WX2 <- pairwise.wilcox.test(df_habitat_ss_KW$Diversity , df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Diversity")
res_ss_WX3 <- pairwise.wilcox.test(df_habitat_ss_KW$Individuals, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Individuals")
res_ss_WX4 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Catostomidae, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Catostomidae")
res_ss_WX5 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Intolerant_Taxa, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Intolerant_Tax")
res_ss_WX6 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Moderate_Tolerance_Taxa, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Moderate_Tolerance_Taxa")
res_ss_WX7 <- pairwise.wilcox.test(df_habitat_ss_KW$Percent_Tolerant_Taxa, df_habitat_ss_KW$Year, p.adjust.method = "BH") %>% broom::tidy() %>% mutate(Metric = "Percent_Tolerant_Taxa")

res_ss_WX <- rbind(res_ss_WX, res_ss_WX2, res_ss_WX3, res_ss_WX4, res_ss_WX5, res_ss_WX6, res_ss_WX7)
remove(list = c("res_ss_WX2", "res_ss_WX3", "res_ss_WX4","res_ss_WX5","res_ss_WX6","res_ss_WX7"))

writexl::write_xlsx(res_ss_KW, path = paste0(plot_folder,"/fish_kruskalwallis_sensitive.xlsx"))
writexl::write_xlsx(res_ss_WX, path = paste0(plot_folder,"/fish_wilcox_sensitive.xlsx"))

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
