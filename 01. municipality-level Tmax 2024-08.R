# R FILE NAME:	          01. municipality-level Tmax.R
# 
# AUTHOR:			           	Huiqi Chen
# 
# VERSION:			        	v2
# 
# DATE VERSION CREATED: 	2024-08-14
# 
# DESCRIPTION OF FILE: 
# For REACH Obj1
# Convert daily maximum temperature data from grid level to city level
# SOURCE: ERA5 reanalysis dataset for Brazil from 1990-2019

# load packages
library(dplyr) 
library(readr) 
library(sf)
library(terra) 
library(exactextractr)
library(tidyverse)
library(lubridate) 

#input temperture data (nc file)
ncintmax <- rast("F:/data/ERA5_Tmax.nc") #change to your path

start_date <- as.Date(paste(1990, 01, 01, sep = "-"))
end_date <- as.Date(paste(2019, 12, 31, sep = "-"))
seqdate <- seq.Date(start_date, end_date, by = "day") 

 
# input city.shp ####
city_shp <- sf::st_read("D:/BR_Municipios_2022.shp", quiet=T)

# EXTRACT THE AREA-WEIGHTED TEMPERATURE AVERAGES
brtmean <- exact_extract(ncintmax, city_shp, fun="mean", progress=F)
colnames(brtmean) <- as.character(seqdate) 
brtmean <- brtmean-273.15

# RESHAPE IN LONG FORMAT AND RENAME
brtmean1 <- cbind(MSOA11CD=city_shp$CD_MUN, brtmean) %>%
  pivot_longer(cols=-1, names_to="date", values_to="tmean") %>%
  mutate(date=as.Date(date))

# Daily Tmax series #######################
  ### output csv data 
  write.csv(brtmean1,paste0("city_Temp_data","_daily",".csv"),row.names = FALSE) 
