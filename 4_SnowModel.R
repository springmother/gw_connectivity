##################################################
## Purpose: Access FTP and process SnowModel data
## Grace Peven, gpeven@gmail.com
## Date Created: 5/12/2025
##################################################

library(RCurl)

ftp_url <- "ftp://gliston.cira.colostate.edu/grace_project/100m_daily/"

# List files in the FTP directory (anonymous login)
filenames <- getURL(ftp_url, dirlistonly = TRUE)

# Print the list
cat(filenames)


destnames <- filenames <-  strsplit(filenames, "\r*\n")[[1]] # destfiles = origin file names
mapply(function(x,y) writeBin(getBinaryURL(x,  dirlistonly = FALSE), y), x = filenames, y = paste("D:/PhD/SnowModel/",destnames, sep = "")) #writing all zipped files in one directory


# download directory
download_dir = "D:/PhD/SnowModel/"


data = getBinaryURL(ftp_url)


ftp_base <- "ftp://gliston.cira.colostate.edu/grace_project/100m_daily/"
urls <- paste0(ftp_base, filenames)

dest_paths <- file.path("D:/PhD/SnowModel/", filenames)

mapply(function(x, y) {
  bin <- getBinaryURL(x, dirlistonly = FALSE)
  writeBin(bin, y)
  cat("Downloaded:", y, "\n")
}, x = urls, y = dest_paths)
#####################################################


##########################################################
## Process SnowModel Data
##########################################################



library(terra)
library(ggplot2)
library(dplyr)
library(tcltk)
library(stringr)
library(tidyr)
library(tidyterra)
library(tidyverse)
library(zoo)

############################
### Import watershed shapefiles
############################

all_watersheds= vect("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/Basin_shapefiles/all_watersheds.shp")
str(all_watersheds)
plot(all_watersheds)
crs(all_watersheds)

#UTM Zone 11N
utm_crs <- "EPSG:32611"

# Reproject
all_watersheds <- project(all_watersheds, utm_crs)


##############################
### Process Snowmodel rasters
##############################

# Set folder path
folder_path <- "D:/PhD/SnowModel/ID_STREAM_PROJECT_SM_100m_soff_tifs"

# List all .tif files
SnowModel <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)


# Create a brick
soff_brick <- rast(SnowModel)

# Double check that all files imported
print(soff_brick)


# save raster so I won't need to repeat above code
writeRaster(soff_brick, "D:/PhD/SnowModel/soff_clipped_2017_2024.tif", overwrite=TRUE)


###############################################################################


#############################################
## calculate statistics over each daily image
#############################################

######################################
## Average
#####################################

mean_df <- terra::extract(soff_brick, all_watersheds, fun = mean, na.rm = TRUE, bind = TRUE)
mean_df <- as.data.frame(mean_df)

# Pivot to long format
mean_runoff <- mean_df %>%
  pivot_longer(
    cols = starts_with("ID_STREAM_PROJECT_SM_100m_soff_"),
    names_to = "layer_name",
    values_to = "avg_runoff_m_day"
  ) %>%
  mutate(
    date = str_extract(layer_name, "\\d{4}_\\d{2}_\\d{2}"),
    date = as.Date(date, format = "%Y_%m_%d")
  ) %>%
  select(Name, avg_runoff_m_day, date)



ggplot(mean_runoff, aes(date, avg_runoff_m_day))+
  geom_point()+
  facet_wrap(~Name)


##############################################
## SD
##############################################
sd_df   <-  terra::extract(soff_brick, all_watersheds, fun = sd, na.rm = TRUE, bind = TRUE)
sd_df <- as.data.frame(sd_df)

# Pivot to long format
sd_runoff <- sd_df %>%
  pivot_longer(
    cols = starts_with("ID_STREAM_PROJECT_SM_100m_soff_"),
    names_to = "layer_name",
    values_to = "sd_runoff_m_day"
  ) %>%
  mutate(
    date = str_extract(layer_name, "\\d{4}_\\d{2}_\\d{2}"),
    date = as.Date(date, format = "%Y_%m_%d")
  ) %>%
  select(Name, sd_runoff_m_day, date)



ggplot(sd_runoff, aes(date, sd_runoff_m_day))+
  geom_point()+
  facet_wrap(~Name)

##############################################
## Max
##############################################

max_df  <-  terra::extract(soff_brick, all_watersheds, fun = max, na.rm = TRUE, bind = TRUE)
max_df <- as.data.frame(max_df)

# Pivot to long format
max_runoff <- max_df %>%
  pivot_longer(
    cols = starts_with("ID_STREAM_PROJECT_SM_100m_soff_"),
    names_to = "layer_name",
    values_to = "max_runoff_m_day"
  ) %>%
  mutate(
    date = str_extract(layer_name, "\\d{4}_\\d{2}_\\d{2}"),
    date = as.Date(date, format = "%Y_%m_%d")
  ) %>%
  select(Name, ,max_runoff_m_day, date)



ggplot(max_runoff, aes(date, max_runoff_m_day))+
  geom_point()+
  facet_wrap(~Name)



##############################################
## Combine results
#############################################

Snowmelt_runoff = left_join(mean_runoff, sd_runoff, by = c("Name", "date"))
Snowmelt_runoff = left_join(Snowmelt_runoff, max_runoff, by = c("Name", "date"))


# View results
head(Snowmelt_runoff)

########################################
## Make updates

Snowmelt_runoff = Snowmelt_runoff %>% rename(site_id_display = Name)

library(lubridate)
Snowmelt_runoff$date = ymd(Snowmelt_runoff$date)
Snowmelt_runoff$DOY = yday(Snowmelt_runoff$date)



# Function to calculate Water Year
calculate_water_year <- function(date) {
  ifelse(month(date) >= 10, year(date) + 1, year(date))
}

# Apply function to create the "Water Year" column
Snowmelt_runoff$Water_Year <- sapply(Snowmelt_runoff$date, calculate_water_year)


# Function to calculate Day of Water Year
calculate_dowy <- function(date) {
  # Define the water year start based on the current date's year
  wy_start <- as.Date(paste0(ifelse(month(date) >= 10, year(date), year(date) - 1), "-10-01"))
  # Calculate Day of Water Year
  as.numeric(difftime(date, wy_start, units = "days")) + 1
}

# Apply function to create the "Day of Water Year" column
Snowmelt_runoff$doy_wy <- sapply(Snowmelt_runoff$date, calculate_dowy)


# save to external drive and OneDrive
write.csv(Snowmelt_runoff,"D:/PhD/SnowModel/Snowmelt_runoff_ts.csv")
write.csv(Snowmelt_runoff,"C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/CSVs/Snowmelt_runoff_ts.csv")


#########################################################
## Calculate max annual runoff and snowmelt rate
#########################################################

Snowmelt_runoff = Snowmelt_runoff %>% filter(Water_Year !=2016)

Snowmelt_runoff  <- Snowmelt_runoff %>%
  arrange(site_id_display, Water_Year, DOY) %>%
  group_by(site_id_display, Water_Year) %>%
  mutate(cumulative_runoff = cumsum(avg_runoff_m_day))


snowmelt_annual <- Snowmelt_runoff %>%
  group_by(site_id_display, Water_Year) %>%
  filter(avg_runoff_m_day > 0) %>%
 mutate(runoff_80_pct = quantile(avg_runoff_m_day, 0.8, na.rm = TRUE), 
        total_runoff_avg = sum(avg_runoff_m_day, na.rm = TRUE)) %>%
  summarise(
    melt_days = n(),
    melt_rate_avg = total_runoff_avg/ melt_days, 
    total_runoff_max = sum(max_runoff_m_day, na.rm = TRUE),
    melt_rate_max = total_runoff_max/ melt_days, 
sd_snowmelt = mean(sd_runoff_m_day), 
DOY_peak_runoff = DOY[which.max(avg_runoff_m_day)], 
DOY_50_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.5)[1]], 
DOY_75_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.75)[1]], 
DOY_95_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.95)[1]],
DOY_90_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.90)[1]], 
DOY_85_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.85)[1]], 
DOY_80_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.80)[1]], 
DOY_91_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.91)[1]],
DOY_92_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.92)[1]],
DOY_93_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.93)[1]],
DOY_94_pct_runoff = DOY[which(cumulative_runoff >=  total_runoff_avg* 0.94)[1]],
total_runoff_avg = total_runoff_avg)



snowmelt_annual <- Snowmelt_runoff %>%
  group_by(site_id_display, Water_Year) %>%
  arrange(DOY) %>%
  mutate(
    total_runoff_avg = sum(avg_runoff_m_day, na.rm = TRUE),
    cumulative_runoff = cumsum(avg_runoff_m_day),
    DOY_90_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.9)[1]]
  ) %>%
  mutate(
    is_before_90pct = DOY <= DOY_90_pct_runoff
  ) %>%
  summarise(
    melt_days = sum(avg_runoff_m_day > 0, na.rm = TRUE),
    melt_days_to_90 = sum(avg_runoff_m_day > 0 & is_before_90pct, na.rm = TRUE),
    total_runoff_avg = sum(avg_runoff_m_day, na.rm = TRUE),
    total_runoff_to_90 = sum(avg_runoff_m_day[is_before_90pct], na.rm = TRUE),
    melt_rate_avg = total_runoff_avg / melt_days,
    melt_rate_to_90 = total_runoff_to_90 / melt_days_to_90,
    total_runoff_max = sum(max_runoff_m_day, na.rm = TRUE),
    melt_rate_max = total_runoff_max / melt_days,
    sd_snowmelt = mean(sd_runoff_m_day, na.rm = TRUE),
    DOY_peak_runoff = DOY[which.max(avg_runoff_m_day)],
    DOY_50_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.5)[1]],
    DOY_75_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.75)[1]],
    DOY_80_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.80)[1]],
    DOY_85_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.85)[1]],
    DOY_90_pct_runoff = DOY_90_pct_runoff,
    DOY_91_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.91)[1]],
    DOY_92_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.92)[1]],
    DOY_93_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.93)[1]],
    DOY_94_pct_runoff = DOY[which(cumulative_runoff >= total_runoff_avg * 0.94)[1]]
  )



# eliminate duplicates
snowmelt_annual = unique(snowmelt_annual)



# save to external drive and OneDrive
write.csv(snowmelt_annual,"D:/PhD/SnowModel/snowmelt_annual.csv")
write.csv(snowmelt_annual,"C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Stream_Temp/CSVs/snowmelt_annual.csv")



#############################################################################
## Ready to merge with SPEI data in 5_annual_climate.R
#############################################################################


