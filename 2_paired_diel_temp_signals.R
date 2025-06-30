#######################################################################
## Purpose: Calculate thermal sensitivity and AR/phase lag statistics on a 
##          diel basis
## By: Grace Peven, gpeven@gmail.com
## Date created: 03/24/2025
#######################################################################

library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)


######################################################
## Import data 
######################################################

all_temp_sub = read.csv("Processed_temp_data/ALL_SITES_AST_sub.csv") ## daily max and min data



#############################################################################
## Calculate radian days/hours
############################################################################


# Annual cycle: Normalize DOY to a full-year sine wave
all_temp_sub$rday_annual <- (2 * pi * all_temp_sub$DOY) / 365  

# Diurnal cycle: Normalize TimeOfDay to a full-day sine wave
all_temp_sub$rday_diurnal <- (2 * pi * all_temp_sub$TimeofDay) / 24  


# make unique site list for loop
sites = unique(all_temp_sub$site_ID)

# unique year list
years = unique(all_temp_sub$Year)

# note: unique DOY calculated in-loop

###########################################################################
## Calculate seasonal temp max, min, and average to later join with seasonal table

temp_seasonal = all_temp_sub %>% group_by(site_ID, Year)%>%
  summarise(MaxTempC = max(StreamTempC), MeanTempC = mean(StreamTempC), MinTempC = min(StreamTempC))

temp_seasonal = temp_seasonal %>% rename(site_id = site_ID, year = Year)

###############################################################################
#### Fit Diel Sinusoidal Model
##############################################################################


diurnal_dynamic_metrics  = c()


for(i in 1:length(sites)) {
  
  extract = which(all_temp_sub$site_ID == sites[i])
  sub1 = all_temp_sub[extract,]
  
  for(ii in 1:length(years)) {
    
    extract = which(sub1$Year == years[ii])
    sub2 = sub1[extract,]
    
    
    # Unique DOY values
    days = unique(sub2$DOY)
    
    for(j in 1:length(days)) {
      
      extract = which(sub2$DOY == days[j])
      sub3 = sub2[extract,]
      
      # Skip if not enough data in a day
      if(nrow(sub3) < 6) next 
    
    
    
    # Water temperature linear fit
    Tfit.lm_wt <- lm(StreamTempC ~ sin(rday_annual) + cos(rday_annual) + 
                       sin(rday_diurnal) + cos(rday_diurnal), 
                     data = sub3)
    
    # Air temperature linear fit
    Tfit.lm_at <- lm(AirTempC ~ sin(rday_annual) + cos(rday_annual) + 
                       sin(rday_diurnal) + cos(rday_diurnal), 
                     data = sub3)
    
    # Get model summaries
    sum_wt <- summary(Tfit.lm_wt)
    sum_at <- summary(Tfit.lm_at)
    
    # Extract R² and residual SE
    r_squared_wt <- sum_wt$r.squared
    resid_se_wt <- sum_wt$sigma
    
    r_squared_at <- sum_at$r.squared
    resid_se_at <- sum_at$sigma
    
    
    # calculate water temp phase
    
    Phase_wt = atan2(coef(Tfit.lm_wt)['sin(rday_diurnal)'], coef(Tfit.lm_wt)['cos(rday_diurnal)'])
    
    
    #Calculate Amplitude of the signal 
    Amp_WT <- sqrt((coef(Tfit.lm_wt)['sin(rday_diurnal)']^2) + (coef(Tfit.lm_wt)['cos(rday_diurnal)']^2))
    
    
    # calculate air  temp phase
    Phase_at = atan2(coef(Tfit.lm_at)['sin(rday_diurnal)'], coef(Tfit.lm_at)['cos(rday_diurnal)'])
    
    #Calculate Amplitude of the signal 
    Amp_AT <- sqrt((coef(Tfit.lm_at)['sin(rday_diurnal)']^2) + (coef(Tfit.lm_at)['cos(rday_diurnal)']^2))
    
    
    
    #create dataframe output summary data
    lmStats <- data.frame(site_id = sites[i], 
                          year = years[ii],
                          DOY = days[j],
                          Amp_WT = Amp_WT,
                          Amp_AT = Amp_AT,
                          diurnal_phase_lag = Phase_wt - Phase_at,
                          diurnal_AmpRatio = round((Amp_WT)/(Amp_AT),2),
                          diurnal_Mean_Ratio = round(coef(Tfit.lm_wt)['(Intercept)'] /(coef(Tfit.lm_at)['(Intercept)']),2),
                          count = nobs(Tfit.lm_wt), 
                          r_squared_wt = r_squared_wt,
                          resid_se_wt = resid_se_wt,
                          r_squared_at = r_squared_at,
                          resid_se_at = resid_se_at)
    
    # convert back to hours
    lmStats$diurnal_phase_lag= lmStats$diurnal_phase_lag*(24/(2*pi))
    
    
    diurnal_dynamic_metrics = rbind(diurnal_dynamic_metrics , lmStats)
    
  }
  }
}


View(diurnal_dynamic_metrics )

# plot example AT amplitude

diurnal_dynamic_metrics %>% filter(site_id == "grouse_E062") %>%
  ggplot(aes(DOY, Amp_WT))+
  geom_point()+
  geom_smooth(method = "gam")+
  facet_wrap(~year)



############################################################################
## Filter out days with poor fits
############################################################################
r2_threshold <- 0.5

diurnal_dynamic_metrics = diurnal_dynamic_metrics %>% 
  filter(r_squared_wt >r2_threshold, 
         r_squared_at >r2_threshold)


write.csv(diurnal_dynamic_metrics, "CSVs/diurnal_dynamic_metrics.csv")



################################################################
## Calculate thermal sensitivity statistics

diurnal_thermal_sensitivity_stats  = c()


for(i in 1:length(sites)) {
  
  extract = which(all_temp_sub$site_ID == sites[i])
  sub1 = all_temp_sub[extract,]
  
  for(ii in 1:length(years)) {
    
    extract = which(sub1$Year == years[ii])
    sub2 = sub1[extract,]
    # Unique DOY values
    days = unique(sub2$DOY)
    
    for(j in 1:length(days)) {
      
      extract = which(sub2$DOY == days[j])
      sub3 = sub2[extract,]
      
      # Skip if not enough data in a day
      if(nrow(sub3) < 6) next 
    # Seasonal/Annual fit 
    TS_fit.lm <- lm(StreamTempC ~ AirTempC, data = sub3)
    
    
    
  
    TS_output <- data.frame(site_id = sites[i], 
                            year = years[ii],
                            DOY = days[j],
                            count = nobs(TS_fit.lm),
                            TS_Slope =round(coef(TS_fit.lm)[2],2), 
                            AdjRsqr=round(summary(TS_fit.lm)$adj.r.squared,2),
                            RMSE=sqrt(mean(resid(TS_fit.lm)^2)),
                            YInt=round(coef(TS_fit.lm)['(Intercept)'],2)
    )
    
    
    diurnal_thermal_sensitivity_stats = rbind(diurnal_thermal_sensitivity_stats, TS_output)
    
  }
}

}

View(diurnal_thermal_sensitivity_stats)


write.csv(diurnal_thermal_sensitivity_stats, "CSVs/diurnal_thermal_sensitivity_stats.csv")



###############################################################################
## Merge TS and AR dataframes and save

diurnal_metrics = merge(diurnal_thermal_sensitivity_stats, diurnal_dynamic_metrics, by = c("site_id", "year", "count", "DOY"))

write.csv(diurnal_metrics, "CSVs/diurnal_ts_metrics.csv")


View(diurnal_metrics)





#########################################################################
## Summarize seasonal trends
#########################################################################

seasonal_metrics =diurnal_metrics %>% group_by(site_id, year)%>%
  summarise(avg_TS_Slope = mean(TS_Slope), avg_AmpRatio = mean(diurnal_AmpRatio),
            median_AmpRatio = median(diurnal_AmpRatio), iqr_AmpRatio = IQR(diurnal_AmpRatio),
            avg_PL = mean(diurnal_phase_lag), avg_Mean_Ratio = mean(diurnal_Mean_Ratio), 
            change_AR = (quantile(diurnal_AmpRatio, 0.9))- quantile(diurnal_AmpRatio, 0.1))

seasonal_metrics$year = as.factor(seasonal_metrics$year)

seasonal_diurnal_trends$year = as.factor(seasonal_diurnal_trends$year)

seasonal_diurnal_trends = seasonal_diurnal_trends %>% left_join(seasonal_metrics, by = c("year", "site_id"))

seasonal_diurnal_trends = seasonal_diurnal_trends %>% left_join(temp_seasonal, by = c("site_id", "year"))


write.csv(seasonal_diurnal_trends, "CSVs/seasonal_diurnal_trends.csv")




###############################################################################
## Data Visualization
###############################################################################


diurnal_metrics %>% filter(site_id %in% c("grouse_E062"))%>%
ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("Grouse Creek E062")

diurnal_metrics %>% filter(site_id %in% c("warren_E162"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1, method = "loess")+
  geom_point()+
  ggtitle("Warren E162")+
  ylim(0, 1)


diurnal_metrics %>% filter(site_id %in% c("lake_E035"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("Lake Creek E035")

diurnal_metrics %>% filter(site_id %in% c("LB_lower_E126"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("LB Lower E126")

diurnal_metrics %>% filter(site_id %in% c("LB_upper_E017"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("LB Upper E017")



diurnal_metrics %>% filter(site_id %in% c("buckhorn_E016"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("Buckhorn E016")




diurnal_metrics %>% filter(site_id %in% c("chamberlain_E032"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("Chamberlain E032")


diurnal_metrics %>% filter(site_id %in% c("logan_E123"))%>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year)))+
  geom_smooth(se = FALSE, lwd = 1.5, span = 1)+
  ggtitle("Logan E123")+
  ylim(0, 0.5)


################################################################################
## Old code to explore fitting polynomial models to data
################################################################################

################################################################################
# fit polynomial to Ar and PL metrics for each year and stream
# use 7-day running average
###############################################################################
# 
# library(mgcv)
# library(zoo)
# 
# 
# 
# # make unique list of years and sites for loop
# years = unique(diurnal_metrics$year)
# sites = unique(diurnal_metrics$site_id)
# 
# 
# seasonal_diurnal_trends = c()
# poly_time_series = c()
# 
# for(i in 1:length(sites)) {
#   
#   extract = which(diurnal_metrics$site_id == sites[i])
#   sub1 = diurnal_metrics[extract,]
#   
#   for(ii in 1:length(years)) {
#     
#     extract = which(sub1$year == years[ii])
#     sub2 = sub1[extract,]
#     
# # Skip if no data for a given year/site combo
#     if(nrow(sub2) == 0) next 
#     
# # Calculate 7-day rolling average (initially I fit GAMS, but this over-smoothed the data)
#     
#   sub2$AmpRatio_7day <- rollmean(sub2$diurnal_AmpRatio, k = 7, fill = NA, align = "center")
#   sub2$PL_7day <- rollmean(sub2$diurnal_phase_lag, k = 7, fill = NA, align = "center")
#     
# # Fit second order Polynomial model - amp ratio
#     
#   poly_model_ar <- lm(AmpRatio_7day ~ poly(DOY, 2), data = sub2)
#     
#   coefficients_ar <- coef(poly_model_ar)
# 
#   intercept_ar <- coefficients_ar[1]
#   linear_term_ar <- coefficients_ar[2]
#   quadratic_term_ar <- coefficients_ar[3]
# 
# poly_term_p_value = summary(poly_model_ar)$coefficients[, "Pr(>|t|)"][2]  
# summary_ar = summary(poly_model_ar)  
# r_squared_ar <- summary_ar$r.squared
# resid_se_ar <- summary_ar$sigma
# 
#   
# sub2$predicted_AmpRatio =  predict(poly_model_ar, newdata =  sub2, type = "response")
# 
#     
#   
# # Fit second order Polynomial model - phase_lag
#   
#   poly_model_pl <- lm(PL_7day ~ poly(DOY, 2), data = sub2)
#   
#   coefficients_pl <- coef(poly_model_pl)
#   
#   intercept_pl <- coefficients_pl[1]
#   linear_term_pl <- coefficients_pl[2]
#   quadratic_term_pl <- coefficients_pl[3]  
#   
#   
#   summary_pl <- summary(poly_model_pl)
#   r_squared_pl <- summary_pl$r.squared
#   resid_se_pl <- summary_pl$sigma
#   
# sub2$predicted_PL <- predict(poly_model_pl, newdata =  sub2, type = "response")
# 
# poly_results = data.frame(site_id = sites[i], 
#                           year = years[ii], 
#                           mean_amp_ratio = intercept_ar, 
#                           poly_term_p_value = round(poly_term_p_value, 5),
#                           linear_term_amp_ratio = linear_term_ar, 
#                           concavity_amp_ratio = quadratic_term_ar, 
#                           r2_amp_ratio = r_squared_ar,
#                           resid_se_amp_ratio = resid_se_ar,
#                           mean_phase_lag = intercept_pl, 
#                           linear_term_phase_lag = linear_term_pl, 
#                           concavity_phase_lag = quadratic_term_pl, 
#                           r2_phase_lag = r_squared_pl,
#                           resid_se_phase_lag = resid_se_pl)    
# 
# 
# poly_time = data.frame(site_id = sites[i], 
#                        year = years[ii], 
#                        DOY = sub2["DOY"], 
#                        predicted_AmpRatio = sub2["predicted_AmpRatio"], 
#                        predicted_PL = sub2["predicted_PL"], 
#                        diurnal_AmpRatio = sub2["diurnal_AmpRatio"], 
#                        diurnal_phase_lag = sub2["diurnal_phase_lag"], 
#                        AmpRatio_7day = sub2["AmpRatio_7day"], 
#                        PL_7day = sub2["PL_7day"])
# 
# 
# poly_time_series = rbind(poly_time_series, poly_time)
# 
# 
# seasonal_diurnal_trends = rbind(seasonal_diurnal_trends, poly_results)
# 
# }}
# 
# 
# View(seasonal_diurnal_trends)
# 
# View(poly_time_series)
# 
# 
# write.csv(poly_time_series, "CSVs/poly_time_series.csv")
# 
# poly_time_series= read.csv("CSVs/poly_time_series.csv")
# 
# poly_time_series %>% filter(year == 2022)%>%
# ggplot( aes(DOY, predicted_AmpRatio))+
#   geom_point()+
#   facet_wrap(~site_id)
# 
# 
# 
# 
# ################################################################################
# ## Multi-year fit
# #################################################################################
# library(mgcv)
# library(zoo)
# 
# diurnal_metrics= read.csv("CSVs/diurnal_ts_metrics.csv")
# 
# 
# # make unique list of years and sites for loop
# years = unique(diurnal_metrics$year)
# sites = unique(diurnal_metrics$site_id)
# 
# 
# seasonal_diurnal_trends_all = c()
# poly_time_series_all  = c()
# 
# for(i in 1:length(sites)) {
#   
#   extract = which(diurnal_metrics$site_id == sites[i])
#   sub1 = diurnal_metrics[extract,]
#   
# 
#     
#     # Skip if no data for a given year/site combo
#     if(nrow(sub1) == 0) next 
#     
#     # Calculate 7-day rolling average (initially I fit GAMS, but this over-smoothed the data)
#     
#     sub1$AmpRatio_7day <- rollmean(sub1$diurnal_AmpRatio, k = 7, fill = NA, align = "center")
#     sub1$PL_7day <- rollmean(sub1$diurnal_phase_lag, k = 7, fill = NA, align = "center")
#     
#     # Fit second order Polynomial model - amp ratio
#     
#     poly_model_ar <- lm(AmpRatio_7day ~ poly(DOY, 2), data = sub1)
#     
#     coefficients_ar <- coef(poly_model_ar)
#     
#     intercept_ar <- coefficients_ar[1]
#     linear_term_ar <- coefficients_ar[2]
#     quadratic_term_ar <- coefficients_ar[3]
#     
#     
#     summary_ar = summary(poly_model_ar)  
#     r_squared_ar <- summary_ar$r.squared
#     resid_se_ar <- summary_ar$sigma
#     
#     
#     sub1$predicted_AmpRatio =  predict(poly_model_ar, newdata =  sub1, type = "response")
#     
#     
#     
#     # Fit second order Polynomial model - phase_lag
#     
#     poly_model_pl <- lm(PL_7day ~ poly(DOY, 2), data = sub1)
#     
#     coefficients_pl <- coef(poly_model_pl)
#     
#     intercept_pl <- coefficients_pl[1]
#     linear_term_pl <- coefficients_pl[2]
#     quadratic_term_pl <- coefficients_pl[3]  
#     
#     summary_pl <- summary(poly_model_pl)
#     r_squared_pl <- summary_pl$r.squared
#     resid_se_pl <- summary_pl$sigma
#     
#     sub1$predicted_PL <- predict(poly_model_pl, newdata =  sub1, type = "response")
#     
#     poly_results = data.frame(site_id = sites[i], 
#                             
#                               mean_amp_ratio = intercept_ar, 
#                               linear_term_amp_ratio = linear_term_ar, 
#                               concavity_amp_ratio = quadratic_term_ar, 
#                               r2_amp_ratio = r_squared_ar,
#                               resid_se_amp_ratio = resid_se_ar,
#                               mean_phase_lag = intercept_pl, 
#                               linear_term_phase_lag = linear_term_pl, 
#                               concavity_phase_lag = quadratic_term_pl, 
#                               r2_phase_lag = r_squared_pl,
#                               resid_se_phase_lag = resid_se_pl)    
#     
#     
# 
#     
# 
#     seasonal_diurnal_trends_all = rbind(seasonal_diurnal_trends_all, poly_results)
#     
#   }
# 
# 
# View(seasonal_diurnal_trends_all)
# 
# View(poly_time_series_all)
# 
# 
# write.csv(poly_time_series_all, "CSVs/poly_time_series_.csv")
# 

#########################################################################
## Summarize seasonal trends
#########################################################################
# 
# seasonal_metrics_all =diurnal_metrics %>% group_by(site_id)%>%
#   summarise(avg_TS_Slope = mean(TS_Slope), avg_AmpRatio = mean(diurnal_AmpRatio), avg_PL = mean(diurnal_phase_lag), avg_Mean_Ratio = mean(diurnal_Mean_Ratio))
# 
# 
# 
# seasonal_diurnal_trends_all = seasonal_diurnal_trends_all %>% left_join(seasonal_metrics_all, by = c( "site_id"))
# 
# seasonal_diurnal_trends_all = seasonal_diurnal_trends_all %>% select(-mean_amp_ratio)
# 
# write.csv(seasonal_diurnal_trends_all, "CSVs/general_diurnal_trends_all_years.csv")


  
  
  
