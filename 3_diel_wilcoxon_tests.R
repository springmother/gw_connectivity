###############################################################
## Purpose: Analysis for Diurnal temp metrics
## By: Grace Peven, gpeven@gmail.com
## Date Created: 03/21/2025
###############################################################

library(pgirmess)
library(dplyr)
library(dunn.test)
library(emmeans)
library(tibble)
library(ggplot2)
library(tidyr)
library(gridExtra)



diurnal_dynamic_metrics = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/CSVs/diurnal_dynamic_metrics.csv")

diurnal_ts_metrics = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/CSVs/diurnal_ts_metrics.csv")



######################################
## Statistical Tests
######################################

### 1-sample wilcoxson rank test between 2021 and all other years

diurnal_summary_2021 = diurnal_dynamic_metrics %>% group_by(site_id) %>%  filter(year ==2021) %>%
  summarise(AmpRatio_2021 = diurnal_AmpRatio, PL_2021 = diurnal_phase_lag)


diurnal_summary_other= diurnal_dynamic_metrics %>% group_by(site_id) %>%  filter(year != 2021) %>%
  summarise(AmpRatio_other = median(diurnal_AmpRatio), PL_other =  median(diurnal_phase_lag))

# join tables

diurnal_summary = left_join(diurnal_summary_2021, diurnal_summary_other, by = "site_id")


## Visually compare 2021 vs. AOY
ggplot(diurnal_summary, aes(factor(site_id)))+
  geom_boxplot(aes(y = AmpRatio_2021))+
  geom_point(aes(y = AmpRatio_other), size = 3, color = "blue")+
  ylim(0,1)
 

# testing all streams together
wilcox.test(diurnal_summary$AmpRatio_other, diurnal_summary$AmpRatio_2021, paired = TRUE)


############################################################################
############################################################################

# test for differences per stream

sites = unique(diurnal_dynamic_metrics$site_id)

diurnal_dynamic_metrics$year <- as.factor(diurnal_dynamic_metrics$year)
diurnal_dynamic_metrics$year <- relevel(diurnal_dynamic_metrics$year, ref = "2021") 

stat_tests = c()

for(i in 1:length(sites)) {
  
  extract = which(diurnal_dynamic_metrics$site_id == sites[i])
  sub1 = diurnal_dynamic_metrics[extract,]
 
# specific differences between years relative to 2021

 model = lm(diurnal_AmpRatio ~ year, data = sub1)
 model_sum = summary(model)$coefficients

 model_sum<- model_sum %>%
  as.data.frame() %>%
  rownames_to_column(var = "term")
 
 
 ## 2021 compared to average of all other years
sub1_2021 = sub1 %>%  filter(year ==2021) %>%
   summarise(AmpRatio_2021 = diurnal_AmpRatio)


sub1_other= sub1 %>%  filter(year != 2021) %>%
  summarise(AmpRatio_other = median(diurnal_AmpRatio))

sub_summary = cbind(sub1_other, sub1_2021)


wx = wilcox.test(sub_summary$AmpRatio_other, sub_summary$AmpRatio_2021, paired = TRUE)

 
 years = model_sum[1]
 p_values = model_sum[5]
 wx_2021_p = wx$p.value
 
test_results =  data.frame(site_id = sites[i],
            years,
            p_values ,
            wx_2021_p) 
 
stat_tests = rbind(stat_tests, test_results)

}


View(stat_tests)

stat_tests = stat_tests %>% rename(years = term, p_values = Pr...t..)

## Results included in Table 2


##############################################################################
## Summarize annual trends for Table 2
#############################################################################

diurnal_sum_2021 = diurnal_ts_metrics %>% group_by(site_id)%>%
  filter(year == 2021)%>%
  summarise(med_amp_ratio = median(diurnal_AmpRatio), avg_ampratio = mean(diurnal_AmpRatio), avg_TS_slope = mean(TS_Slope), avg_R = mean(AdjRsqr))

diurnal_sum_aoy = diurnal_ts_metrics %>% group_by(site_id)%>%
  filter(year != 2021)%>%
  summarise(med_amp_ratio = median(diurnal_AmpRatio), avg_ampratio = mean(diurnal_AmpRatio), avg_TS_slope = mean(TS_Slope), avg_R = mean(AdjRsqr))


coef_sum_2021 = seasonal_diurnal_trends%>% group_by(site_id)%>%
  filter(year == 2021)%>%
  summarise(avg_coef = concavity_amp_ratio)

coef_sum_aoy = seasonal_diurnal_trends %>% group_by(site_id)%>%
  filter(year != 2021)%>%
  summarise(avg_coef = mean(concavity_amp_ratio))



