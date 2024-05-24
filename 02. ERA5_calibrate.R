# R FILE NAME:	02. ERA5_calibrate.R
# 
# AUTHOR:				Huiqi Chen
# 
# VERSION:				v1
# 
# DATE VERSION CREATED: 	2024-05-24
# 
# DESCRIPTION OF FILE: 
# For REACH Obj1
# This R script compares the daily maximum temperatures 
# from ERA5 reanalysis data with station data 
# and performs calibration on the ERA5 data.


#### load the required package ####
library(data.table)
library(tidyverse)

library(dlnm)
library(mvmeta)
library(splines)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(lubridate)

library(tibble)
library(purrr)


# Daily Tmax observed station data in Brazil 
tmax_ts <- fread('F:/tmax-time-series-heatwave.csv') 
tmax_ts$MAX_TEMP  <- gsub(',','.',tmax_ts$MAX_TEMP) 
tmax_ts$MAX_TEMP <- as.numeric(tmax_ts$MAX_TEMP)

tmax_ts_year <- tmax_ts %>% group_by(YEAR) %>% summarise(mean_tmax = mean(MAX_TEMP)) %>% rename(obs=mean_tmax)
ggplot(tmax_ts_year, aes(x=YEAR, y=obs)) + 
  geom_line() + geom_point() + theme_bw() 

CHQ_Tmax_city <- read.csv("city_Temp_data_d.csv")


# Comparison of annual long-term trends #################
nation_data_yr <- read.csv("nation_Temp_data_yr.csv") %>% 
  rename(ERA5=Temp_avr,YEAR=year) %>% mutate(YEAR=gsub("y","",YEAR),
                                             ERA5=ERA5-273.15)
tmax_year <- merge(tmax_ts_year, nation_data_d) %>% 
  pivot_longer(-1,names_to = 'group',values_to = 'tmax')
ggplot(tmax_year, aes(x=YEAR, y=tmax, color=group)) + 
  geom_line() + geom_point() + theme_bw()
# ggsave("figure1_temporal trend.png")


# compare daily maximum temperature from observed station data and ERA5 reanalysis data ###########
# NM_MUN_i ####
Brazil_city_name_code <- read.csv("Brazil_city_name_code.csv")
NM_MUN_i <- c("Manaus")

MUN_i <- Brazil_city_State_Region[which(Brazil_city_State_Region$NM_MUN_new == NM_MUN_i),]
NM_MUN_i <- MUN_i$NM_MUN_new 
CD_MUN_i <- MUN_i$CD_MUN    


obs <- tmax_ts %>% filter(CD_MUN == CD_MUN_i) %>% 
  filter(YEAR %in% c(2008:2019))%>% 
  rename(date = DATE, Tmax = MAX_TEMP) 
agg_sub <- CHQ_metero_city %>% select(c(date, CD_MUN, Tmax)) %>% filter(CD_MUN == CD_MUN_i) %>% 
  group_by(date, CD_MUN) %>% summarise(Tmax = max(Tmax, na.rm = TRUE)) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(YEAR=year(date),
         MONTH=month(date))%>% 
  relocate(CD_MUN, date) %>% as.data.frame()
head(agg_sub)

# load the bias correction function
# This is a function for applying a bias correction method developed in ISI-MIP 
# (Hempel et al., 2013). For more details on the calibration procedure, see the code "fhempel.R".
source("D:/fhempel.r")
# RE-CALIBRATE USING THE BIAS CORRECTION FUNCTION
agg_subcal <- fhempel(obs[c("date","Tmax")],agg_sub[,c("date","Tmax")])
head(agg_subcal)


year_i <- 2019
daily_comparison <-  obs %>% 
  rename(obs=Tmax) %>% select(-c(YEAR,MONTH,CD_MUN,NM_MUN,SIGLA_UF)) %>%
  merge(agg_sub, by='date') %>% rename(ERA5=Tmax) %>% 
  merge(agg_subcal, by='date') %>% rename(ERA5_calibrated=Tmax) %>%
  filter(date >= as.Date(paste0(year_i,"-01-01")) & date < as.Date(paste0(year_i,"-12-31"))) %>% 
  select(-c(CD_MUN,YEAR,MONTH)) %>%
  pivot_longer(c(obs,ERA5,ERA5_calibrated),values_to = 'Tmax',names_to = 'group') %>% 
  mutate(YEAR=year(date),
         MONTH=month(date))
datebreaks <- seq(as.Date(paste0(year_i,"-01-01")), as.Date(paste0(year_i,"-12-31")),
                  by = "2 month")

daily_comparison %>% filter(YEAR %in% c(year_i) & MONTH %in% c(8,9)) #%>% arrange(desc(Tmax)) %>% head()



# plot: day-to-day fluctuations ####
{
  p1<- ggplot(daily_comparison, aes(x=date, y=Tmax,group=group,color=group)) + 
    geom_line() +
    scale_x_date(breaks = datebreaks) + 
    theme_bw()+
    labs(x = NULL,
         title = paste0("Day-to-day fluctuations (Year ",year_i ,")"))
  p1
  
  p2<- ggplot(subset(daily_comparison,YEAR %in% c(year_i) & MONTH %in% c(8,9)),
              aes(x=date, y=Tmax,group=group,color=group)) + 
    geom_line() +
    geom_point() +
    scale_x_date(breaks = seq(as.Date(paste0(year_i,"-08-01")), 
                              as.Date(paste0(year_i,"-09-30")),
                              by = "10 day")) +  
    labs(x = NULL,
         title = paste0("Day-to-day fluctuations (","Summer ",year_i ,")"))
  
  p2
}


# PLOT - CALIBRATION ######
{ 
  p3 <- ggplot(daily_comparison, aes(x=Tmax, fill=group)) +
    geom_density(alpha = 0.5) +
    theme_bw()+
    labs(y = "Distribution", x = expression(paste("Temperature (",degree,"C)")))+
    theme(legend.title = element_blank())+
    labs(title = "Calibration of ERA5 data")
  p3
  
  p4 <- ggplot(daily_comparison, 
               aes(x=Tmax, group=group, color=group)) +
    stat_ecdf(geom = "step") +
    theme_bw()+
    labs(y = "Distribution", x = expression(paste("Temperature (",degree,"C)")))+
    theme(legend.title = element_blank())+
    labs(title = "Calibration of ERA5 data")
  p4
} 

# ggsave("Fig1_Calibration.png")

