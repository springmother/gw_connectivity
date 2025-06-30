###############################################################
## Purpose: Processing Stream temperature data on diurnal basis
## By: Grace Peven, gpeven@gmail.com 
## Date Created: 03/11/2025
###############################################################

library(sf)
library(lubridate)
library(MuMIn)
library(ggplot2)
library(dplyr)
library(zoo)


###################################################################
## List of sites to use: water ID and air temp ID in parentheses
#### 1.  Warren E162 (E343)
#### 2.  Lake Cr E035 (E342)
#### 3.  McKorkle E118 (E341)
#### 4.  Government E323 (E341)
#### 5.  Smith Cr E208 (E341)
#### 6.  Logan Cr E109 (E341)
#### 7.  Pioneer Cr (TWRS) # no subdaily
#### 8.  Little Buckhorn E017 (Teapot RAWS)        
#### 9.  Buckhorn main E016 (TEAPOT RAWS)           
#### 10. Little Buckhorn Outlet E126 (TEAPOT RAWS)  
#### 11. Chamberlain E032 (LODGEPOLE RAWS)          
#### 12. Grouse Cr E062 (E342)
#################################################################

#######################
## LODGEPOLE RAWS

lodgepole_at = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Climate/Air_Temp/WRCC_Lodgepole_RAWS_Data_prepped.csv")

lodgepole_at <- lodgepole_at %>%
  mutate(AirTempC = ifelse(AirTempC == "M", "", AirTempC))

lodgepole_at$Date = mdy(lodgepole_at$Date)
lodgepole_at$Year = year(lodgepole_at$Date)
lodgepole_at$DOY = yday(lodgepole_at$Date)
lodgepole_at$Year = as.factor(lodgepole_at$Year)

lodgepole_at$AirTempC = as.numeric(lodgepole_at$AirTempC)


lodgepole_at$Time <- as.POSIXct(lodgepole_at$Time, format="%H:%M", tz="UTC") 

lodgepole_at$TimeofDay<- as.numeric(format(lodgepole_at$Time, "%H")) + as.numeric(format(lodgepole_at$Time, "%M")) / 60


lodgepole_at = lodgepole_at %>% drop_na(AirTempC)

lodgepole_at <- lodgepole_at[, c("TimeofDay", "DOY", "Year", "AirTempC")]



############################################################################


########################
## TEAPOT RAWS

teapot_at = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Climate/Air_Temp/WRCC_TeaPot_RAWS_Data_prepped.csv")

teapot_at <- teapot_at %>%
  mutate(AirTempC = ifelse(AirTempC == "M", "", AirTempC))

teapot_at$Date = mdy(teapot_at$Date)
teapot_at$Year = year(teapot_at$Date)
teapot_at$DOY = yday(teapot_at$Date)
teapot_at$Year = as.factor(teapot_at$Year)

teapot_at$AirTempC = as.numeric(teapot_at$AirTempC)


teapot_at$Time <- as.POSIXct(teapot_at$Time, format="%H:%M", tz="UTC") 

teapot_at$TimeofDay<- as.numeric(format(teapot_at$Time, "%H")) + as.numeric(format(teapot_at$Time, "%M")) / 60


teapot_at = teapot_at %>% drop_na(AirTempC)

teapot_at <- teapot_at[, c("TimeofDay", "DOY", "Year", "AirTempC")]



############################################################################

##########################
## Warren Air temp (E343)

warren_at_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2017.csv", fileEncoding = "Latin1")
warren_at_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2019.csv")
warren_at_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2020.csv")
warren_at_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2021.csv")
warren_at_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2022.csv")
warren_at_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2023.csv")
warren_at_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/air_temp/2024.csv")

warren_at_sub = rbind(warren_at_2017, warren_at_2019, warren_at_2020, warren_at_2021, warren_at_2022, warren_at_2023, warren_at_2024)


warren_at_sub = warren_at_sub %>% rename(Time = Time..GMT.06.00, AirTempC = Temp...C)

warren_at_sub$Date = mdy(warren_at_sub$Date)
warren_at_sub$DOY = yday(warren_at_sub$Date)
warren_at_sub$Year = year(warren_at_sub$Date)
warren_at_sub$Year = as.factor(warren_at_sub$Year)


warren_at_sub$Time <- as.POSIXct(warren_at_sub$Time, format="%H:%M", tz="UTC") 

warren_at_sub <- warren_at_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # Round down to the nearest hour
 

warren_at_sub$TimeofDay<- as.numeric(format(warren_at_sub$Time, "%H")) + as.numeric(format(warren_at_sub$Time, "%M")) / 60

warren_at_sub <- warren_at_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(AirTempC = mean(AirTempC, na.rm = TRUE))

warren_at_sub = warren_at_sub %>% drop_na(Year)


############################################################################


############################
## McKorkle Air temp (E341)


mckorkle_at_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2018.csv", fileEncoding = "Latin1")
mckorkle_at_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2019.csv")
mckorkle_at_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2020.csv")
mckorkle_at_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2021.csv")
mckorkle_at_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2022.csv")
mckorkle_at_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2023.csv")
mckorkle_at_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/air_temp/2024.csv")

mckorkle_at_sub = rbind(mckorkle_at_2018, mckorkle_at_2019, mckorkle_at_2020, mckorkle_at_2021, mckorkle_at_2022, mckorkle_at_2023, mckorkle_at_2024)


mckorkle_at_sub = mckorkle_at_sub %>% rename(Time = Time..GMT.06.00, AirTempC = Temp...C)

mckorkle_at_sub$Date = mdy(mckorkle_at_sub$Date)
mckorkle_at_sub$DOY = yday(mckorkle_at_sub$Date)
mckorkle_at_sub$Year = year(mckorkle_at_sub$Date)
mckorkle_at_sub$Year = as.factor(mckorkle_at_sub$Year)

mckorkle_at_sub$Time <- as.POSIXct(mckorkle_at_sub$Time, format="%H:%M", tz="UTC") 

mckorkle_at_sub <- mckorkle_at_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


mckorkle_at_sub$TimeofDay<- as.numeric(format(mckorkle_at_sub$Time, "%H")) + as.numeric(format(mckorkle_at_sub$Time, "%M")) / 60

mckorkle_at_sub <- mckorkle_at_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(AirTempC = mean(AirTempC, na.rm = TRUE))

mckorkle_at_sub = mckorkle_at_sub %>% drop_na(Year)

############################################################################


############################
## Lake Cr Air temp (E342)

lake_at_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/air_temp/2017.csv", fileEncoding = "Latin1")
lake_at_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/air_temp/2018.csv", fileEncoding = "Latin1")
lake_at_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/air_temp/2021.csv")
lake_at_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/air_temp/2022.csv")
lake_at_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/air_temp/2023.csv", fileEncoding = "Latin1")
lake_at_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/air_temp/2024.csv", fileEncoding = "Latin1")

lake_at_2024 = lake_at_2024 %>% rename(Date =  ï..Date, Temp...C = Temp..Â.C)

lake_at_sub = rbind(lake_at_2018, lake_at_2017, lake_at_2021, lake_at_2022, lake_at_2023, lake_at_2024)


lake_at_sub = lake_at_sub %>% rename(Time = Time..GMT.06.00, AirTempC = Temp...C)

lake_at_sub$Date = mdy(lake_at_sub$Date)
lake_at_sub$DOY = yday(lake_at_sub$Date)
lake_at_sub$Year = year(lake_at_sub$Date)
lake_at_sub$Year = as.factor(lake_at_sub$Year)


lake_at_sub$Time <- as.POSIXct(lake_at_sub$Time, format="%H:%M", tz="UTC") 

lake_at_sub <- lake_at_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


lake_at_sub$TimeofDay<- as.numeric(format(lake_at_sub$Time, "%H")) + as.numeric(format(lake_at_sub$Time, "%M")) / 60

lake_at_sub <- lake_at_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(AirTempC = mean(AirTempC, na.rm = TRUE))

lake_at_sub = lake_at_sub %>% drop_na(Year)

############################################################################


################################################################################
## Stream temperature data 
################################################################################



#########################################################################
## Warren Creek (E162)

warren_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2017.csv", fileEncoding = "Latin1")
warren_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2019.csv")
warren_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2020.csv")
warren_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2021.csv")
warren_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2022.csv")
warren_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2023.csv", fileEncoding = "Latin1")
warren_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Warren_E162/2024.csv")

warren_2023 = warren_2023 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
warren_sub = rbind(warren_2017, warren_2019, warren_2020, warren_2021, warren_2022, warren_2023, warren_2024)


warren_sub = warren_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

warren_sub$Date = mdy(warren_sub$Date)
warren_sub$DOY = yday(warren_sub$Date)
warren_sub$Year = year(warren_sub$Date)
warren_sub$Year = as.factor(warren_sub$Year)


# Convert 30-minute intervals into a continuous time of day
warren_sub$Time <- as.POSIXct(warren_sub$Time, format="%H:%M", tz="UTC") 

warren_sub <- warren_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


warren_sub$TimeofDay<- as.numeric(format(warren_sub$Time, "%H")) + as.numeric(format(warren_sub$Time, "%M")) / 60

warren_sub <- warren_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

warren_sub = merge(warren_sub, warren_at_sub, by = c("DOY", "Year", "TimeofDay")) 

warren_sub = warren_sub %>% rename(StreamTempC = Temp_C)


warren_sub$site_ID = "warren_E162"
warren_sub$Year = as.numeric(as.character(warren_sub$Year))

warren_sub = warren_sub %>% drop_na(Year)


##############################################################################

############################################

# ## write to drive
# 
write.csv(warren_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/warren_E162_ast_sub.csv")





#######################################################
## Grouse Creek (E062)

grouse_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2017.csv", fileEncoding = "Latin1")
grouse_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2018.csv", fileEncoding = "Latin1")
grouse_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2019.csv")
grouse_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2020.csv")
grouse_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2021.csv")
grouse_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2022.csv")
grouse_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2023.csv", fileEncoding = "Latin1")
grouse_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Grouse_E062/2024.csv")


grouse_sub= rbind(grouse_2017, grouse_2018, grouse_2019, grouse_2020, grouse_2021, grouse_2022, grouse_2023, grouse_2024)


grouse_sub= grouse_sub%>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

grouse_sub$Date = mdy(grouse_sub$Date)
grouse_sub$DOY = yday(grouse_sub$Date)
grouse_sub$Year = year(grouse_sub$Date)
grouse_sub$Year = as.factor(grouse_sub$Year)


# Convert 30-minute intervals into a continuous time of day
grouse_sub$Time <- as.POSIXct(grouse_sub$Time, format="%H:%M", tz="UTC") 

grouse_sub <- grouse_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


grouse_sub$TimeofDay<- as.numeric(format(grouse_sub$Time, "%H")) + as.numeric(format(grouse_sub$Time, "%M")) / 60

grouse_sub <- grouse_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

grouse_sub = merge(grouse_sub, lake_at_sub, by = c("DOY", "Year", "TimeofDay")) 

grouse_sub = grouse_sub %>% rename(StreamTempC = Temp_C)


grouse_sub$site_ID = "grouse_E062"
grouse_sub$Year = as.numeric(as.character(grouse_sub$Year))

grouse_sub = grouse_sub %>% drop_na(Year)

# grouse <- grouse  %>%
#   mutate(
#     Date = as.Date(DOY, origin = paste0(Year - 1, "-12-31")),
#     Date = format(Date, "%m/%d/%Y"))  # Format as d/m/yyyy

############################################
# ## write to drive

write.csv(grouse_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/grouse_E062_ast_sub.csv")







#######################################################
## McKorkle (Upper Big Creek) E118


mckorkle_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2018.csv", fileEncoding = "Latin1")
mckorkle_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2019.csv")
mckorkle_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2020.csv", fileEncoding = "Latin1")
mckorkle_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2021.csv")
mckorkle_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2022.csv")
mckorkle_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2023.csv", fileEncoding = "Latin1")
mckorkle_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/McCorkle_E118/2024.csv")


mckorkle_2020= mckorkle_2020%>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
mckorkle_2023= mckorkle_2023%>% rename(Date = ï..Date , Temp...C = Temp..Â.C)

mckorkle_sub= rbind(mckorkle_2018, mckorkle_2019, mckorkle_2020, mckorkle_2021, mckorkle_2022, mckorkle_2023, mckorkle_2024)


mckorkle_sub= mckorkle_sub%>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

mckorkle_sub$Date = mdy(mckorkle_sub$Date)
mckorkle_sub$DOY = yday(mckorkle_sub$Date)
mckorkle_sub$Year = year(mckorkle_sub$Date)
mckorkle_sub$Year = as.factor(mckorkle_sub$Year)


# Convert 30-minute intervals into a continuous time of day
mckorkle_sub$Time <- as.POSIXct(mckorkle_sub$Time, format="%H:%M", tz="UTC") 

mckorkle_sub <- mckorkle_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


mckorkle_sub$TimeofDay<- as.numeric(format(mckorkle_sub$Time, "%H")) + as.numeric(format(mckorkle_sub$Time, "%M")) / 60

mckorkle_sub <- mckorkle_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

mckorkle_sub = merge(mckorkle_sub, mckorkle_at_sub, by = c("DOY", "Year", "TimeofDay")) 

mckorkle_sub = mckorkle_sub %>% rename(StreamTempC = Temp_C)


mckorkle_sub$site_ID = "mckorkle_E118"
mckorkle_sub$Year = as.numeric(as.character(mckorkle_sub$Year))

mckorkle_sub = mckorkle_sub%>% drop_na(Year)


############################################
# 
# ## write to drive
# 
write.csv(mckorkle_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/mckorkle_E118_ast_sub.csv")




################################################################################
#### Lake Creek E035

lake_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2017.csv", fileEncoding = "Latin1")
lake_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2018.csv", fileEncoding = "Latin1")
lake_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2019.csv")
lake_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2020.csv")
lake_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2021.csv")
lake_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2022.csv")
lake_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2023.csv", fileEncoding = "Latin1")
lake_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Lake_E035/2024.csv")

lake_2023= lake_2023%>% rename(Date = ï..Date , Temp...C = Temp..Â.C)

lake_sub = rbind(lake_2017, lake_2018, lake_2019, lake_2020, lake_2021, lake_2022, lake_2023, lake_2024)


lake_sub = lake_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

lake_sub$Date = mdy(lake_sub$Date)
lake_sub$DOY = yday(lake_sub$Date)
lake_sub$Year = year(lake_sub$Date)
lake_sub$Year = as.factor(lake_sub$Year)


# Convert 30-minute intervals into a continuous time of day
lake_sub$Time <- as.POSIXct(lake_sub$Time, format="%H:%M", tz="UTC") 

lake_sub <- lake_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


lake_sub$TimeofDay<- as.numeric(format(lake_sub$Time, "%H")) + as.numeric(format(lake_sub$Time, "%M")) / 60

lake_sub <- lake_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

lake_sub = merge(lake_sub, lake_at_sub, by = c("DOY", "Year", "TimeofDay")) 

lake_sub = lake_sub %>% rename(StreamTempC = Temp_C)



lake_sub$site_ID = "lake_E035"
lake_sub$Year = as.numeric(as.character(lake_sub$Year))

lake_sub = lake_sub%>% drop_na(Year)


############################################

## write to drive

write.csv(lake_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/lake_E035_ast_sub.csv")




######################################################################

#######  Government E323 (air - E341)

government_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Government_E323/2019.csv")
government_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Government_E323/2020.csv")
government_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Government_E323/2021.csv")
government_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Government_E323/2022.csv")
government_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Government_E323/2023.csv", fileEncoding = "Latin1")
government_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Government_E323/2024.csv")

government_2023= government_2023%>% rename(Date = ï..Date , Temp...C = Temp..Â.C)

government_sub = rbind(government_2019, government_2020, government_2021, government_2022, government_2023, government_2024)


government_sub = government_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

government_sub$Date = mdy(government_sub$Date)
government_sub$DOY = yday(government_sub$Date)
government_sub$Year = year(government_sub$Date)
government_sub$Year = as.factor(government_sub$Year)



# Convert 30-minute intervals into a continuous time of day
government_sub$Time <- as.POSIXct(government_sub$Time, format="%H:%M", tz="UTC") 

government_sub <- government_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


government_sub$TimeofDay<- as.numeric(format(government_sub$Time, "%H")) + as.numeric(format(government_sub$Time, "%M")) / 60

government_sub <- government_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

government_sub = merge(government_sub, mckorkle_at_sub, by = c("DOY", "Year", "TimeofDay")) 

government_sub = government_sub %>% rename(StreamTempC = Temp_C)



government_sub$site_ID = "government_E323"
government_sub$Year = as.numeric(as.character(government_sub$Year))

government_sub = government_sub %>% drop_na(Year)



############################################

# ## write to drive

write.csv(government_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/government_E323_ast_sub.csv")




#######################################################################
#######################################################################
### Smith Cr E208 (E341)
#######################################################################
#######################################################################

smith_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2017.csv", fileEncoding = "Latin1")
smith_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2018.csv", fileEncoding = "Latin1")
smith_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2019.csv", fileEncoding = "Latin1")
smith_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2021.csv")
smith_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2022.csv")
smith_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2023.csv", fileEncoding = "Latin1")
smith_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Smith_E208/2024.csv")

smith_2023 = smith_2023 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
smith_2019 = smith_2019 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
smith_sub = rbind(smith_2017, smith_2018, smith_2019, smith_2021, smith_2022, smith_2023, smith_2024)


smith_sub = smith_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

smith_sub$Date = mdy(smith_sub$Date)
smith_sub$DOY = yday(smith_sub$Date)
smith_sub$Year = year(smith_sub$Date)
smith_sub$Year = as.factor(smith_sub$Year)



# Convert 30-minute intervals into a continuous time of day
smith_sub$Time <- as.POSIXct(smith_sub$Time, format="%H:%M", tz="UTC") 

smith_sub <- smith_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


smith_sub$TimeofDay<- as.numeric(format(smith_sub$Time, "%H")) + as.numeric(format(smith_sub$Time, "%M")) / 60

smith_sub <- smith_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

smith_sub = merge(smith_sub, mckorkle_at_sub, by = c("DOY", "Year", "TimeofDay")) 

smith_sub = smith_sub %>% rename(StreamTempC = Temp_C)



smith_sub$site_ID = "smith_E208"
smith_sub$Year = as.numeric(as.character(smith_sub$Year))

smith_sub = smith_sub%>% drop_na(Year)


############################################

## write to drive

write.csv(smith_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/smith_E208_ast_sub.csv")



#######################################################################
#######################################################################
### Big Cr E109 (E341)
#######################################################################
#######################################################################

BC_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2017.csv", fileEncoding = "Latin1")
BC_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2018.csv", fileEncoding = "Latin1")
BC_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2019.csv", fileEncoding = "Latin1")
BC_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2020.csv", fileEncoding = "Latin1")
BC_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2021.csv")
BC_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2022.csv")
BC_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2023.csv", fileEncoding = "Latin1")
BC_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Logan_E109/2024.csv")


BC_2020 = BC_2020 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
BC_2023 = BC_2023 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
BC_2019 = BC_2019 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)

BC_sub = rbind(BC_2017, BC_2018, BC_2019, BC_2020,BC_2021, BC_2022, BC_2023, BC_2024)


BC_sub = BC_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

BC_sub$Date = mdy(BC_sub$Date)
BC_sub$DOY = yday(BC_sub$Date)
BC_sub$Year = year(BC_sub$Date)
BC_sub$Year = as.factor(BC_sub$Year)


# Convert 30-minute intervals into a continuous time of day

BC_sub$Time <- as.POSIXct(BC_sub$Time, format="%H:%M", tz="UTC") 

BC_sub <- BC_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


BC_sub$TimeofDay<- as.numeric(format(BC_sub$Time, "%H")) + as.numeric(format(BC_sub$Time, "%M")) / 60

BC_sub <- BC_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

BC_sub = merge(BC_sub, mckorkle_at_sub, by = c("DOY", "Year", "TimeofDay")) 

BC_sub = BC_sub %>% rename(StreamTempC = Temp_C)





BC_sub$Year = as.numeric(as.character(BC_sub$Year))

BC_sub = BC_sub%>% drop_na(Year)

BC_sub$site_ID = "BC_E109"
############################################
# 
# ## write to drive

write.csv(BC_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/BC_E109_ast_sub.csv")

# #########################################################################################



#######################################################################
#######################################################################
#### Chamberlain Creek
#######################################################################
#######################################################################

cham_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2018.csv", fileEncoding = "Latin1")
cham_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2019.csv")
cham_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2020.csv")
cham_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2021.csv")
cham_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2022.csv")
cham_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2023.csv", fileEncoding = "Latin1")
cham_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Chamberlain/2024.csv")


chamberlain_sub = rbind(cham_2018, cham_2019, cham_2020, cham_2021, cham_2022, cham_2023, cham_2024)


chamberlain_sub = chamberlain_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

chamberlain_sub$Date = mdy(chamberlain_sub$Date)
chamberlain_sub$DOY = yday(chamberlain_sub$Date)
chamberlain_sub$Year = year(chamberlain_sub$Date)
chamberlain_sub$Year = as.factor(chamberlain_sub$Year)


# Convert 30-minute intervals into a continuous time of day

chamberlain_sub$Time <- as.POSIXct(chamberlain_sub$Time, format="%H:%M", tz="UTC") 

chamberlain_sub <- chamberlain_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


chamberlain_sub$TimeofDay<- as.numeric(format(chamberlain_sub$Time, "%H")) + as.numeric(format(chamberlain_sub$Time, "%M")) / 60

chamberlain_sub <- chamberlain_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

chamberlain_sub = merge(chamberlain_sub, lodgepole_at, by = c("DOY", "Year", "TimeofDay")) 

chamberlain_sub = chamberlain_sub %>% rename(StreamTempC = Temp_C)


chamberlain_sub$Year = as.numeric(as.character(chamberlain_sub$Year))

chamberlain_sub = chamberlain_sub%>% drop_na(Year)

chamberlain_sub$site_ID = "chamberlain_E032"
############################################
# 
# ## write to drive

write.csv(chamberlain_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/chamberlain_E032_ast_sub.csv")



#######################################################################
#######################################################################
###  Little Buckhorn E017 upper (Teapot RAWS)
#######################################################################
#######################################################################

LB_upper_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2017.csv", fileEncoding = "Latin1")
LB_upper_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2018.csv", fileEncoding = "Latin1")
LB_upper_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2020.csv", fileEncoding = "Latin1")
LB_upper_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2021.csv")
LB_upper_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2022.csv")
LB_upper_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2023.csv", fileEncoding = "Latin1")
LB_upper_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_upper_E017/2024.csv")


LB_upper_2020 = LB_upper_2020 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
LB_upper_2023 = LB_upper_2023 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)


LB_upper_sub = rbind(LB_upper_2017, LB_upper_2018, LB_upper_2020,LB_upper_2021, LB_upper_2022, LB_upper_2023, LB_upper_2024)


LB_upper_sub = LB_upper_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

LB_upper_sub$Date = mdy(LB_upper_sub$Date)
LB_upper_sub$DOY = yday(LB_upper_sub$Date)
LB_upper_sub$Year = year(LB_upper_sub$Date)
LB_upper_sub$Year = as.factor(LB_upper_sub$Year)


# Convert 30-minute intervals into a continuous time of day

LB_upper_sub$Time <- as.POSIXct(LB_upper_sub$Time, format="%H:%M", tz="UTC") 

LB_upper_sub <- LB_upper_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


LB_upper_sub$TimeofDay<- as.numeric(format(LB_upper_sub$Time, "%H")) + as.numeric(format(LB_upper_sub$Time, "%M")) / 60

LB_upper_sub <- LB_upper_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

LB_upper_sub = merge(LB_upper_sub, teapot_at, by = c("DOY", "Year", "TimeofDay")) 

LB_upper_sub = LB_upper_sub %>% rename(StreamTempC = Temp_C)


LB_upper_sub$Year = as.numeric(as.character(LB_upper_sub$Year))

LB_upper_sub = LB_upper_sub%>% drop_na(Year)

LB_upper_sub$site_ID = "LB_upper_E017"
############################################
# 
# ## write to drive

write.csv(LB_upper_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/LB_upper_E017_ast_sub.csv")




#######################################################################
#######################################################################
###  Little Buckhorn Outlet E126 (TEAPOT RAWS)
#######################################################################
#######################################################################

LB_lower_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2017.csv", fileEncoding = "Latin1")
LB_lower_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2018.csv", fileEncoding = "Latin1")
LB_lower_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2019.csv", fileEncoding = "Latin1")
LB_lower_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2020.csv", fileEncoding = "Latin1")
LB_lower_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2021.csv")
LB_lower_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2022.csv")
LB_lower_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2023.csv", fileEncoding = "Latin1")
LB_lower_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Little_Buckhorn_lower_E126/2024.csv")


LB_lower_2017 = LB_lower_2017 %>% rename(Temp...C = Temp..øC)
LB_lower_2020 = LB_lower_2020 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
LB_lower_2023 = LB_lower_2023 %>% rename(Time..GMT.06.00 = Time)
LB_lower_2019 = LB_lower_2019 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)

LB_lower_sub = rbind(LB_lower_2017, LB_lower_2018, LB_lower_2019, LB_lower_2020,LB_lower_2021, LB_lower_2022, LB_lower_2023, LB_lower_2024)

LB_lower_sub = LB_lower_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

LB_lower_sub$Date = mdy(LB_lower_sub$Date)
LB_lower_sub$DOY = yday(LB_lower_sub$Date)
LB_lower_sub$Year = year(LB_lower_sub$Date)
LB_lower_sub$Year = as.factor(LB_lower_sub$Year)


# Convert 30-minute intervals into a continuous time of day

LB_lower_sub$Time <- as.POSIXct(LB_lower_sub$Time, format="%H:%M", tz="UTC") 

LB_lower_sub <- LB_lower_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


LB_lower_sub$TimeofDay<- as.numeric(format(LB_lower_sub$Time, "%H")) + as.numeric(format(LB_lower_sub$Time, "%M")) / 60

LB_lower_sub <- LB_lower_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

LB_lower_sub = merge(LB_lower_sub, teapot_at, by = c("DOY", "Year", "TimeofDay")) 

LB_lower_sub = LB_lower_sub %>% rename(StreamTempC = Temp_C)


LB_lower_sub$Year = as.numeric(as.character(LB_lower_sub$Year))

LB_lower_sub = LB_lower_sub%>% drop_na(Year)

LB_lower_sub$site_ID = "LB_lower_E126"
############################################
# 
# ## write to drive

write.csv(LB_lower_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/LB_lower_E126_ast_sub.csv")

ggplot(LB_lower_sub, aes(DOY, StreamTempC, color = factor(Year)))+geom_smooth()


#######################################################################
#######################################################################
###  Buckhorn main E016 (TEAPOT RAWS)
#######################################################################
#######################################################################


buckhorn_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Buckhorn_E016/2018.csv", fileEncoding = "Latin1")
buckhorn_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Buckhorn_E016/2020.csv", fileEncoding = "Latin1")
buckhorn_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Buckhorn_E016/2021.csv")
buckhorn_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Buckhorn_E016/2022.csv")
buckhorn_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Buckhorn_E016/2023.csv", fileEncoding = "Latin1")
buckhorn_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Buckhorn_E016/2024.csv")



buckhorn_2020 = buckhorn_2020 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
buckhorn_2023 = buckhorn_2023 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)


buckhorn_sub = rbind(buckhorn_2018, buckhorn_2020,buckhorn_2021, buckhorn_2022, buckhorn_2023, buckhorn_2024)

buckhorn_sub = buckhorn_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

buckhorn_sub$Date = mdy(buckhorn_sub$Date)
buckhorn_sub$DOY = yday(buckhorn_sub$Date)
buckhorn_sub$Year = year(buckhorn_sub$Date)
buckhorn_sub$Year = as.factor(buckhorn_sub$Year)


# Convert 30-minute intervals into a continuous time of day

buckhorn_sub$Time <- as.POSIXct(buckhorn_sub$Time, format="%H:%M", tz="UTC") 

buckhorn_sub <- buckhorn_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


buckhorn_sub$TimeofDay<- as.numeric(format(buckhorn_sub$Time, "%H")) + as.numeric(format(buckhorn_sub$Time, "%M")) / 60

buckhorn_sub <- buckhorn_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

buckhorn_sub = merge(buckhorn_sub, teapot_at, by = c("DOY", "Year", "TimeofDay")) 

buckhorn_sub = buckhorn_sub %>% rename(StreamTempC = Temp_C)


buckhorn_sub$Year = as.numeric(as.character(buckhorn_sub$Year))

buckhorn_sub = buckhorn_sub%>% drop_na(Year)

buckhorn_sub$site_ID = "buckhorn_E016"
############################################
# 
# ## write to drive

write.csv(buckhorn_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/buckhorn_E016_ast_sub.csv")



#######################################################################
#######################################################################
### Logan Cr E123 (E341)
#######################################################################
#######################################################################

logan_2017 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2017.csv", fileEncoding = "Latin1")
logan_2018 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2018.csv", fileEncoding = "Latin1")
logan_2019 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2019.csv", fileEncoding = "Latin1")
logan_2020 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2020.csv", fileEncoding = "Latin1")
logan_2021 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2021.csv")
logan_2022 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2022.csv")
logan_2023 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2023.csv", fileEncoding = "Latin1")
logan_2024 = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/logan_E123/2024.csv")


logan_2020 = logan_2020 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
logan_2023 = logan_2023 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)
logan_2019 = logan_2019 %>% rename(Date = ï..Date , Temp...C = Temp..Â.C)

logan_sub = rbind(logan_2017, logan_2018, logan_2019, logan_2020,logan_2021, logan_2022, logan_2023, logan_2024)


logan_sub = logan_sub %>% rename(Time = Time..GMT.06.00, Temp_C = Temp...C)

logan_sub$Date = mdy(logan_sub$Date)
logan_sub$DOY = yday(logan_sub$Date)
logan_sub$Year = year(logan_sub$Date)
logan_sub$Year = as.factor(logan_sub$Year)


# Convert 30-minute intervals into a continuous time of day

logan_sub$Time <- as.POSIXct(logan_sub$Time, format="%H:%M", tz="UTC") 

logan_sub <- logan_sub%>%
  mutate(Time = floor_date(Time, unit = "hour")) # round to nearest hour


logan_sub$TimeofDay<- as.numeric(format(logan_sub$Time, "%H")) + as.numeric(format(logan_sub$Time, "%M")) / 60

logan_sub <- logan_sub%>%
  group_by(TimeofDay, DOY, Year) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))

logan_sub = merge(logan_sub, mckorkle_at_sub, by = c("DOY", "Year", "TimeofDay")) 

logan_sub = logan_sub %>% rename(StreamTempC = Temp_C)





logan_sub$Year = as.numeric(as.character(logan_sub$Year))

logan_sub = logan_sub%>% drop_na(Year)

logan_sub$site_ID = "logan_E123"
############################################
# 
# ## write to drive

write.csv(logan_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/logan_E123_ast_sub.csv")


##############################################################################
## MERGE ALL TEMP DATA
#############################################################################


all_temp_sub = rbind(warren_sub, logan_sub, smith_sub, government_sub, lake_sub, mckorkle_sub, grouse_sub, LB_lower_sub, LB_upper_sub, chamberlain_sub, buckhorn_sub, BC_sub)

## limit to common timeframe

all_temp_sub = all_temp_sub %>% filter(DOY %in% 100:270)

# check for duplicates 

duplicates <- all_temp_sub %>%
  group_by(Year, DOY, TimeofDay, site_ID) %>%  
  filter(n() > 1) %>% 
  ungroup()

View(duplicates)


## write to drive

head(all_temp_sub)

write.csv(all_temp_sub, "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Temp_Data/PNF/Processed_temp_data/ALL_SITES_AST_sub.csv")






