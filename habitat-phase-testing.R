## Habitat Notes


# Kruskal–Wallis one-way analysis of variance
## Tutorial from ://www.r-bloggers.com/2021/06/kruskal-wallis-test-in-r-one-way-anova-alternative/
library(ggpubr)
library(rstatix)

habitat_df <- habitat_df  %>%
  reorder_levels(phase, order= c("Phase I", "Phase II", "Phase III"))

#summary
habitat_df %>% 
  group_by(phase) %>% 
  get_summary_stats(nitrate, type = "common") #example for nitrate, met 

#kruskel test
res.kruskel <- habitat_df %>%  kruskal_test(met ~ phase)

#Wilcox
res1<- habitat_df %>%
  dunn_test(met ~ phase, p.adjust.method = "bonferroni")



### Alternative testing
kruskal.test(met ~ phase, data = habitat_df)
pairwise.wilcox.test(x= met,g= phase, p.adjust.method = "BH") # alternative "bonferroni"