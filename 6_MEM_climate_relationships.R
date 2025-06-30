
################################################################################
## Purpose: Mixed Effects Climate analysis
## By: Grace Peven, gpeven@gmail.com
## Date Created: 04/15/2025
################################################################################

library(lme4)
library(grid)
library(gridExtra)
library(effectsize)


# load file if not already loaded
seasonal_diurnal_trends = read.csv( "CSVs/seasonal_diurnal_trends.csv")
 

seasonal_diurnal_trends$year = as.character(seasonal_diurnal_trends$year)
annual_climate$year = as.character(annual_climate$year)


## Merge seasonal trends with climate data
 seasonal_diurnal_trends = seasonal_diurnal_trends %>% left_join(annual_climate, by = c("site_id", "site_id_display","year"))

 
## Edit table after merge if needed
 
# #  
#  seasonal_diurnal_trends = seasonal_diurnal_trends %>% 
#    select(site_id, year, linear_term_amp_ratio, concavity_amp_ratio, concavity_phase_lag, r2_amp_ratio, r2_phase_lag, 
#           avg_TS_Slope, avg_AmpRatio, avg_PL, avg_Mean_Ratio, median_AmpRatio, iqr_AmpRatio, Apr_SPEI_3month.x, May_SPEI_3month.x, June_SPEI_3month.x, 
#           July_SPEI_3month.x, Aug_SPEI_3month.x, Sep_SPEI_3month.x, change_AR, hydro_category, site_id_display, MaxTempC, MeanTempC, 
#           MinTempC, Water_Year, melt_rate_avg.x, melt_rate_max.x, total_runoff_max.x, total_runoff_avg.x, melt_days.x, sd_snowmelt.x, 
#           DOY_50_pct_runoff.x, DOY_75_pct_runoff.x, DOY_95_pct_runoff.x, DOY_peak_runoff.x, DOY_90_pct_runoff, DOY_85_pct_runoff, DOY_80_pct_runoff)%>%
#    
#    rename(Apr_SPEI_3month = Apr_SPEI_3month.x, May_SPEI_3month = May_SPEI_3month.x, June_SPEI_3month = June_SPEI_3month.x, July_SPEI_3month = July_SPEI_3month.x, 
#           Aug_SPEI_3month = Aug_SPEI_3month.x, Sep_SPEI_3month = Sep_SPEI_3month.x, melt_rate_max = melt_rate_max.x, melt_rate_avg = melt_rate_avg.x, 
#           total_runoff_avg = total_runoff_avg.x, total_runoff_max = total_runoff_max.x, melt_days = melt_days.x, sd_snowmelt = sd_snowmelt.x, 
#           DOY_75_pct_runoff = DOY_75_pct_runoff.x, DOY_peak_runoff = DOY_peak_runoff.x, DOY_95_pct_runoff = DOY_95_pct_runoff.x)
#  
#  
#  seasonal_diurnal_trends = seasonal_diurnal_trends %>% mutate(site_id_display = recode(site_id,
#                                      "government_E323" = "Government",
#                                      "LB_upper_E017" = "LB Upper",
#                                      "LB_lower_E126" = "LB Lower",
#                                      "mckorkle_E118" = "McKorkle",
#                                      "chamberlain_E032" = "Chamberlain",
#                                      "buckhorn_E016" = "Buckhorn", 
#                                      "BC_E109" = "Big Creek", 
#                                      "grouse_E062" = "Grouse", 
#                                      "lake_E035" = "Lake", 
#                                      "logan_E123" = "Logan", 
#                                      "smith_E208" = "Smith", 
#                                      "warren_E162" = "Warren"))
# 
 
 
## write to drive 
 write.csv(seasonal_diurnal_trends, "CSVs/seasonal_diurnal_trends.csv")



################################################################
## Convert snowmelt metrics from m to mm

seasonal_diurnal_trends$total_runoff_avg = seasonal_diurnal_trends$total_runoff_avg*1000

seasonal_diurnal_trends$total_runoff_max = seasonal_diurnal_trends$total_runoff_max*1000

seasonal_diurnal_trends$melt_rate_avg = seasonal_diurnal_trends$melt_rate_avg*1000

seasonal_diurnal_trends$melt_rate_max = seasonal_diurnal_trends$melt_rate_max*1000

seasonal_diurnal_trends$melt_rate_to_90 = seasonal_diurnal_trends$melt_rate_to_90*1000
seasonal_diurnal_trends$total_runoff_to_90 = seasonal_diurnal_trends$total_runoff_to_90*1000



##############################################################################
## Assign categories
##############################################################################


seasonal_diurnal_trends<- seasonal_diurnal_trends%>%
mutate(hydro_category = case_when(
site_id %in% c("grouse_E062", "lake_E035", "warren_E162", "LB_upper_E017") ~ "Variable",
site_id %in% c("BC_E109", "buckhorn_E016", "chamberlain_E032", "mckorkle_E118") ~ "Atmospheric",
site_id %in% c("government_E323", "LB_lower_E126", "smith_E208",  "logan_E123") ~ "Groundwater"))


############################################################################
# TOTAL RUNOFF MODEL
############################################################################

# amp ratio
m1 = lmer(log(avg_AmpRatio)~ total_runoff_avg*hydro_category + (1|site_id), data = seasonal_diurnal_trends)

summary(m1)


## emtrends gives me coefficient estimates, SE, df, and CI
library(eemeans)
emtrends(m1, ~ hydro_category, var = "total_runoff_avg")

## Check model assumptions

plot(fitted(m1), resid(m1), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(resid(m1))
qqline(resid(m1), col = "red")

## Check model assumptions with DHARMa
library(DHARMa)


#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = m1)

#2. Plot scaled residuals
plot(simulationOutput)

# no overdispersion, distribution is correct, res vs. predicted looks good 

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, seasonal_diurnal_trends$total_runoff_avg)

#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # no problems



# add fitted data to table for plots 
seasonal_diurnal_trends$fit_m1 = predict(m1)
# reverse the log transformation
seasonal_diurnal_trends$fit_m1 = exp(seasonal_diurnal_trends$fit_m1)

#######################################################################
# fit model for melt rate

m2 = lmer(log(avg_AmpRatio)  ~ melt_rate_to_90*hydro_category +(1|site_id), data = seasonal_diurnal_trends)

summary(m2)


emtrends(m2, ~ hydro_category, var = "melt_rate_to_90")

## Check model assumptions

plot(fitted(m2), resid(m2), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(resid(m2))
qqline(resid(m2), col = "red")

## Check model assumptions with DHARMa

#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = m2)

#2. Plot scaled residuals
plot(simulationOutput)

# no overdispersion, distribution is correct, res vs. predicted looks good 

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, seasonal_diurnal_trends$melt_rate_to_90)

#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # no problems

# add fitted data
seasonal_diurnal_trends$fit_m2 = predict(m2)
# reverse the log transformation
seasonal_diurnal_trends$fit_m2 = exp(seasonal_diurnal_trends$fit_m2)

#######################################################################
# fit model for SPEI

m3 = lmer(log(avg_AmpRatio)  ~ June_SPEI_3month*hydro_category+(1|site_id), data = seasonal_diurnal_trends)

summary(m3)

emtrends(m3, ~ hydro_category, var = "June_SPEI_3month")

## Check model assumptions

plot(fitted(m3), resid(m3), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(resid(m3))
qqline(resid(m3), col = "red")

## Check model assumptions with DHARMa

#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = m3)

#2. Plot scaled residuals
plot(simulationOutput)

# no overdispersion, distribution is correct, res vs. predicted looks good 

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, seasonal_diurnal_trends$June_SPEI_3month)

#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # no problems


# add fitted data
seasonal_diurnal_trends$fit_m3 = predict(m3)
# reverse the log transformation
seasonal_diurnal_trends$fit_m3 = exp(seasonal_diurnal_trends$fit_m3)



#######################################################################
# fit model for snowmelt timing

m4 = lmer(log(avg_AmpRatio)  ~ DOY_90_pct_runoff*hydro_category +(1|site_id), data = seasonal_diurnal_trends)

summary(m4)

emtrends(m4, ~ hydro_category, var = "DOY_90_pct_runoff")

## Check model assumptions

plot(fitted(m4), resid(m4), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(resid(m4))
qqline(resid(m4), col = "red")

## Check model assumptions with DHARMa

#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = m4)

#2. Plot scaled residuals
plot(simulationOutput)

# no overdispersion, distribution is correct, res vs. predicted looks good 

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, seasonal_diurnal_trends$DOY_90_pct_runoff)

#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # no problems



# add fitted data
seasonal_diurnal_trends$fit_m4 = predict(m4)
# reverse the log transformation
seasonal_diurnal_trends$fit_m4 = exp(seasonal_diurnal_trends$fit_m4)


#####################################################################
## Use summary() and emtrends() to extract metrics for Table 3
#####################################################################

