########################################################################
## Purpose: Calculate CWB and SPEI for each site 
## By: Grace Peven
## Date Created: 03/28/2025
########################################################################

library(stringr)
library(SPEI)
########################################################################
# Air Temp and Precip Data - PRISM monthly

precip_airtemp = read.csv("Data/Climate/PRISM/PNF_ST_2017_2024_all_sites_ATEMP_PRECIP.csv")

head(precip_airtemp)
str(precip_airtemp)

precip_airtemp$Year = str_sub(precip_airtemp$Date, 1, 4)
precip_airtemp$Month = str_sub(precip_airtemp$Date, -2, -1)
precip_airtemp$Month = as.numeric(precip_airtemp$Month)
precip_airtemp = precip_airtemp %>% drop_na(airtemp_C)

# some names were cut off in PRISM
precip_airtemp <- precip_airtemp %>%
  mutate(site_id = recode(site_id,
                          "government_E" = "government_E323",
                          "LB_upper_E01" = "LB_upper_E017",
                          "LB_lower_E12" = "LB_lower_E126",
                          "mckorkle_E11" = "mckorkle_E118",
                          "chamberlain_" = "chamberlain_E032",
                          "buckhorn_E01" = "buckhorn_E016"))

# Calculate PET
precip_airtemp$PET <- thornthwaite(precip_airtemp$airtemp_C, lat = 45)

precip_airtemp$Year = as.character(precip_airtemp$Year)


SPEI_monthly = precip_airtemp
SPEI_monthly = as.data.frame(SPEI_monthly)

# Calculate CWB
SPEI_monthly$CWB_mm = SPEI_monthly$ppt_mm - SPEI_monthly$PET

###############################################################################
###############################################################################

# SPEI Calculation

#1 month spei
spei1 = spei(SPEI_monthly$CWB_mm, 1)
# annual spei
spei12 = spei(SPEI_monthly$CWB_mm, 12)
# 3 month spei
spei3 = spei(SPEI_monthly$CWB_mm, 3)

# re-join to SPEI table
SPEI_monthly <- SPEI_monthly %>%
mutate(SPEI_1month = spei1$fitted)

# re-join to SPEI table (3-month)
SPEI_monthly <- SPEI_monthly %>%
  mutate(SPEI_3month = spei3$fitted)

# visualize
ggplot(SPEI_monthly, aes(factor(Month), SPEI_3month))+
  geom_boxplot()+
  facet_wrap(~Year)+
  geom_hline(yintercept = 0)

######################################################################
## Calculate monthly SPEI

sites = unique(SPEI_monthly$site_id)
years = unique(SPEI_monthly$Year)

SPEI_month  = c()


for(i in 1:length(sites)) {
  
  extract = which(SPEI_monthly$site_id == sites[i])
  sub1 = SPEI_monthly[extract,]
  
  for(ii in 1:length(years)) {
    
    extract = which(sub1$Year == years[ii])
    sub2 = sub1[extract,]
   
    Apr_SPEI_3month = ifelse(any(sub2$Month == 4), sub2$SPEI_3month[sub2$Month == 4], NA)
    May_SPEI_3month  = ifelse(any(sub2$Month == 5), sub2$SPEI_3month[sub2$Month == 5], NA)
    June_SPEI_3month  = ifelse(any(sub2$Month == 6), sub2$SPEI_3month[sub2$Month == 6], NA)
    July_SPEI_3month  = ifelse(any(sub2$Month == 7), sub2$SPEI_3month[sub2$Month == 7], NA)
    Aug_SPEI_3month  = ifelse(any(sub2$Month == 8), sub2$SPEI_3month[sub2$Month == 8], NA)
    Sep_SPEI_3month  = ifelse(any(sub2$Month == 9), sub2$SPEI_3month[sub2$Month == 9], NA)
    
    spei_out = data.frame(site_id = sites[i], 
                          Year = years[ii],
                          Apr_SPEI_3month  = Apr_SPEI_3month ,
                          May_SPEI_3month  = May_SPEI_3month ,
                          June_SPEI_3month = June_SPEI_3month , 
                          July_SPEI_3month  = July_SPEI_3month , 
                          Aug_SPEI_3month  = Aug_SPEI_3month , 
                          Sep_SPEI_3month  = Sep_SPEI_3month )
    
    SPEI_month = rbind(SPEI_month, spei_out)
    
  }}

View(SPEI_month)

################################################################################

SPEI_month = SPEI_month %>% rename(year = Year)

SPEI_month = SPEI_month %>% mutate(site_id_display = recode(site_id,
                                "government_E323" = "Government",
                                "LB_upper_E017" = "LB Upper",
                                "LB_lower_E126" = "LB Lower",
                                "mckorkle_E118" = "McKorkle",
                                "chamberlain_E032" = "Chamberlain",
                                "buckhorn_E016" = "Buckhorn", 
                                "BC_E109" = "Big Creek", 
                                "grouse_E062" = "Grouse", 
                                "lake_E035" = "Lake", 
                                "logan_E123" = "Logan", 
                                "smith_E208" = "Smith", 
                                "warren_E162" = "Warren"))




###############################################################################
## bring in snowmelt data from SnowModel
#############################################################################

head(snowmelt_annual) # read in if not already loaded

snowmelt_annual$year = snowmelt_annual$Water_Year
snowmelt_annual$year = as.factor(snowmelt_annual$year)
SPEI_month$year = as.factor(SPEI_month$year)

annual_climate = left_join(SPEI_month, snowmelt_annual, by = c("year", "site_id_display"))

View(annual_climate)

write.csv(annual_climate,"CSVs/annual_climate.csv")




# Visualize data
ggplot(annual_climate,aes(year, melt_rate_to_90))+
  geom_boxplot(aes(y = melt_rate_to_90*100, color = "Snowmelt Rate"), size = 1)+
  geom_boxplot(aes(y = total_runoff_avg, color = "Snowmelt Runoff"), size = 1)+
  scale_y_continuous(
    name = "Max Snowmelt Runoff (m)",
    sec.axis = sec_axis(~ . /100, name = "Max Snowmelt Rate (m/day)"))

