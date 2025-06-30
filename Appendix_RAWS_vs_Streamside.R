###############################################################################
## Purpose: Compare air temp variability between RAWS and Streamside Loggers
## By: Grace Peven, gpeven@gmail.com
## Date Created: 06/20/2025
###############################################################################

library(dplyr)
library(ggplot2)




#############################################################################
## COMPARE AT VARIABILITY BETWEEN RAWS AND STREAMSIDE LOGGERS
#############################################################################


### read in diel file

diurnal_dynamic_metrics = read.csv("/diurnal_dynamic_metrics.csv")

### Assign station types
diurnal_dynamic_metrics_compare = diurnal_dynamic_metrics%>% filter(site_id %in%c("chamberlain_E032", "LB_lower_E126", "LB_upper_E017", "buckhorn_E016")) %>%
  mutate(station_type = "RAWS")


diurnal_dynamic_metrics_streamside= diurnal_dynamic_metrics%>% filter(!site_id %in%c("chamberlain_E032", "LB_lower_E126", "LB_upper_E017", "buckhorn_E016")) %>%
  mutate(station_type = "Streamside")

### Bind datasets together
diurnal_dynamic_metrics = rbind(diurnal_dynamic_metrics_compare, diurnal_dynamic_metrics_streamside)

### Read in LB lower air temp logger for 2024
LB_air = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/clipped2024/Little_Buckhorn_Air_E378.csv")
head(LB_air)


LB_air$Date = mdy(LB_air$Date)
LB_air$DOY = yday(LB_air$Date)
LB_air$Year = year(LB_air$Date)
LB_air$Year = as.factor(LB_air$Year)


LB_air$Time <- as.POSIXct(LB_air$Time, format="%H:%M", tz="UTC") 

LB_air <- LB_air%>%
  mutate(Time = floor_date(Time, unit = "hour")) # Round down to the nearest hour


LB_air$TimeofDay<- as.numeric(format(LB_air$Time, "%H")) + as.numeric(format(LB_air$Time, "%M")) / 60

LB_air <- LB_air%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(AirTempC = mean(AirTempC, na.rm = TRUE))

LB_air = LB_air %>% rename(AirTempC_logger = AirTempC)


### Filter LB Lower RAWS data
LB_air_RAWS = all_temp_sub %>% filter(site_ID == "LB_lower_E126")
LB_air_RAWS$Year = as.factor(LB_air_RAWS$Year)

### Combine 2024 LB Lower logger and RAWS site
LB_air_compare = left_join(LB_air, LB_air_RAWS, by = c("DOY", "Year", "TimeofDay"))

LB_air_compare = LB_air_compare %>% rename(AirTempC_RAWS = AirTempC)


### !! see paper_figures.R for plots included in Appendix

#############################################################################
## Data Visualization
#############################################################################

## Compare individual sites (TEAPOT vs. Warren)

diurnal_dynamic_metrics %>% filter(site_id %in% c("LB_lower_E126", "warren_E162"))%>%
  ggplot(aes(factor(year), Amp_AT, color = station_type))+
  geom_boxplot()



diurnal_dynamic_metrics %>% filter(site_id %in% c("LB_lower_E126", "warren_E162"))%>%
  ggplot(aes(DOY, Amp_AT, color = site_id))+
  geom_smooth(se = FALSE)+
  facet_wrap(~year)


## Compare individual sites (TEAPOT vs. Lake)

diurnal_dynamic_metrics %>% filter(site_id %in% c("LB_lower_E126", "lake_E035"))%>%
  ggplot(aes(factor(year), Amp_AT, color = station_type))+
  geom_boxplot()


diurnal_dynamic_metrics %>% filter(site_id %in% c("LB_lower_E126", "lake_E035"))%>%
  ggplot(aes(DOY, Amp_AT, color = site_id))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

## Compare individual sites (TEAPOT vs. Big Creek)

diurnal_dynamic_metrics %>% filter(site_id %in% c("LB_lower_E126", "BC_E109"))%>%
  ggplot(aes(factor(year), Amp_AT, color = station_type))+
  geom_boxplot()

diurnal_dynamic_metrics %>% filter(site_id %in% c("LB_lower_E126", "BC_E109"))%>%
  ggplot(aes(DOY, Amp_AT, color = site_id))+
  geom_smooth(se = FALSE)+
  facet_wrap(~year)



## Compare individual sites (LODGEPOLE vs. Warren)

diurnal_dynamic_metrics %>% filter(site_id %in% c("chamberlain_E032", "warren_E162"))%>%
  ggplot(aes(factor(year), Amp_AT, color = station_type))+
  geom_boxplot()

## Compare individual sites (LODGEPOLEvs. Lake)

diurnal_dynamic_metrics %>% filter(site_id %in% c("chamberlain_E032", "lake_E035"))%>%
  ggplot(aes(factor(year), Amp_AT, color = station_type))+
  geom_boxplot()


## Compare individual sites (LODGEPOLE vs. Big Creek)

diurnal_dynamic_metrics %>% filter(site_id %in% c("chamberlain_E032", "BC_E109"))%>%
  ggplot(aes(factor(year), Amp_AT, color = station_type))+
  geom_boxplot()


## Compare streamside loggers 

diurnal_dynamic_metrics %>% filter(site_id %in% c("warren_E162", "BC_E109"))%>%
  ggplot(aes(factor(year), Amp_AT, color = site_id))+
  geom_boxplot()

diurnal_dynamic_metrics %>% filter(site_id %in% c("warren_E162", "BC_E109"))%>%
  ggplot(aes(DOY, Amp_AT, color = site_id))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)


diurnal_dynamic_metrics %>% filter(site_id %in% c("lake_E035", "BC_E109"))%>%
  ggplot(aes(factor(year), Amp_AT, color = site_id))+
  geom_boxplot()



diurnal_dynamic_metrics %>% filter(site_id %in% c("lake_E035", "BC_E109"))%>%
  ggplot(aes(DOY, Amp_AT, color = site_id))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

diurnal_dynamic_metrics %>% filter(site_id %in% c("lake_E035", "warren_E162"))%>%
  ggplot(aes(factor(year), Amp_AT, color = site_id))+
  geom_boxplot()


diurnal_dynamic_metrics %>% filter(site_id %in% c("lake_E035", "warren_E162"))%>%
  ggplot(aes(DOY, Amp_AT, color = site_id))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)


   

