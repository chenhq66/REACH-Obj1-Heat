# R FILE NAME:	01.deal with Tmax.nc.R
# 
# AUTHOR:				Huiqi Chen
# 
# VERSION:				v1
# 
# DATE VERSION CREATED: 	2024-05-14
# 
# DESCRIPTION OF FILE: 
# For REACH Obj1
# Convert daily maximum temperature data from grid level to city level
# SOURCE: ERA5 reanalysis dataset for Brazil from 1990-2019

# load packages 
library(ncdf4)
library(dplyr) 
library(readr) 
library(sf)
library(tidyverse)
library(lubridate)

#input temperture data (nc file)
ncintmax <- nc_open(paste0("F:/data/ERA5_Tmax.nc"))

lon <- ncvar_get(ncintmax,"lon")
nlon <- dim(lon)
lat <- ncvar_get(ncintmax,"lat")
nlat <- dim(lat)

time <- ncvar_get(ncintmax,"time")
temp <- ncvar_get(ncintmax,"mx2t")

start_date <- as.Date(paste(1990, 01, 01, sep = "-"))
end_date <- as.Date(paste(2019, 12, 31, sep = "-"))
index_df <- seq.Date(start_date, end_date, by = "day") %>% as.data.frame() %>%  
  rename(date=1) %>% 
  mutate(month=month(date),day=day(date)) %>% 
  mutate(leap_day=ifelse(month==2&day==29,1,0))

index_leap_day <- which(index_df$leap_day==1)
index_df_del <- index_df %>% subset(leap_day!=1) %>% pull(date)

result <- temp[,,-index_leap_day]
dim(result)
nc_close(ncintmax)


library(lubridate)
year <- 1990:2019
nyear <- length(year)

Jan1 <- seq(from=1,by=365,length.out = 30) #1990-2019
Dec31 <- seq(from=365,by=365,length.out = 30) #1990-2019 
j2d <- t(data.frame(Jan1,Dec31))
colnames(j2d) <- year


Temp_array_yr <- array(NA,dim = c(nlon,nlat,nyear),dimnames = list(lon,lat,year)) 
for (p in 1:nyear) {
  for (i in 1:nlon) {
    for (j in 1:nlat) {
      Temp_array_yr[i,j,p] <- mean(result[i,j,(j2d[1,p]:j2d[2,p])],na.rm = TRUE)   
    }
  }
} 
Temp_yr_matrix <- matrix(nrow = (nlon*nlat), ncol = nyear)  
for (yr in c(1:nyear)) {
  Temp_yr_matrix[ , yr] <- c(Temp_array_yr[ , , yr]) 
}
colnames(Temp_yr_matrix) <- paste0("y",year)
Temp_yr_tibble <- as.data.frame.matrix(Temp_yr_matrix)   



# input city.shp ####
city_shp <- sf::st_read("D:/BR_Municipios_2022.shp")
# input country.shp ####
country_shp <- sf::st_read("D:/BR_Pais_2022.shp")


all_lon_lat <- expand.grid("lon" = lon, "lat" = lat)
lon_lat_with_locaion <- data.frame(matrix(ncol = length(city_shp$CD_MUN)+1))
colnames(lon_lat_with_locaion) <- c(city_shp$CD_MUN, "boundary")
lon_lat_with_locaion <- all_lon_lat %>% bind_cols(lon_lat_with_locaion)
lon_lat_to_st_point <- st_sfc(lapply(1:nrow(all_lon_lat), 
                                     FUN = function(i){return(st_point(c(all_lon_lat$lon[i], all_lon_lat$lat[i])))}),
                              crs = 4326)


for (temp_province in c(city_shp$CD_MUN, "boundary")) {
  if (temp_province == "boundary") {
    temp_geometry <- country_shp$geometry[1]
  } else {
    temp_geometry <- city_shp %>% filter(CD_MUN == temp_province) %>% pull(geometry) 
  }
  
  temp_result <- st_intersects(lon_lat_to_st_point, st_transform(temp_geometry, crs = 4326), sparse=FALSE)
  lon_lat_with_locaion[, temp_province] <- temp_result[, 1]
  
}

final_data <- lon_lat_with_locaion %>% bind_cols(Temp_yr_tibble)

### Temp for grid ################ 
grid_data_yr <- final_data %>% filter(boundary == TRUE) %>% 
  dplyr::select(-c( "boundary")) %>% 
  pivot_longer(cols = 3:5574, names_to = "city", values_to = "contain") %>% 
  filter(contain == TRUE) %>% dplyr::select(-contain) %>% 
 merge(city_shp) 

### Temp for city ################ 
city_data_yr <- grid_data_yr %>% select(-c("lon", "lat") ) %>%
  pivot_longer(cols = -c("city"),names_to="year",values_to="Temp") %>% 
  group_by(city,year) %>% 
  summarise(n = n(), Temp = mean(Temp,na.rm = TRUE))

### Temp for nation ################ 
nation_data_yr <- city_data_yr %>% group_by(year) %>% 
  dplyr::summarise(Temp_avr = mean(Temp,na.rm = TRUE)) #%>% ## 全国按年份的：先每个省dmt均值，再31个省的均值

### output csv data
write.csv(grid_data_yr,paste0("grid_Temp_data","_yr",".csv"),row.names = FALSE)
write.csv(city_data_yr,paste0("city_Temp_data","_yr",".csv"),row.names = FALSE)
write.csv(nation_data_yr,paste0("nation_Temp_data","_yr",".csv"),row.names = FALSE)

# Daily Tmax series #######################
{
  
  Temp_array_d <- array(NA,dim = c(nlon,nlat,dim(result)[3]),dimnames = list(lon,lat,dim(result)[3])) 

  Temp_d_matrix <- matrix(nrow = (nlon*nlat), ncol = dim(result)[3])  
  for (d in c(1:dim(result)[3])) {
    Temp_d_matrix[ , d] <- c(Temp_array_d[ , , d]) 
  }
  colnames(Temp_d_matrix) <- index_df_del
  Temp_d_tibble <- as.data.frame.matrix(Temp_d_matrix)   
  

  
  final_data <- lon_lat_with_locaion %>% bind_cols(Temp_d_tibble)
  
  grid_data_d <- final_data %>% filter(boundary == TRUE) %>% 
    dplyr::select(-c( "boundary")) %>% 
    pivot_longer(cols = 3:5574, names_to = "city", values_to = "contain") %>% 
    filter(contain == TRUE) %>% dplyr::select(-contain) %>% 
    merge(city_shp) 
  
  ### Temp for city ################ 
  city_data_d <- grid_data_d %>% select(-c("lon", "lat") ) %>%
    pivot_longer(cols = -c("city"),names_to="year",values_to="Temp") %>% 
    group_by(city,year) %>% 
    summarise(n = n(), Temp = mean(Temp,na.rm = TRUE))
  
  ### Temp for nation ################ 
  nation_data_d <- city_data_d %>% group_by(year) %>% 
    dplyr::summarise(Temp_avr = mean(Temp,na.rm = TRUE)) #%>% ## 全国按年份的：先每个省dmt均值，再31个省的均值
  
  ### output csv data
  write.csv(grid_data_d,paste0("grid_Temp_data","_d",".csv"),row.names = FALSE)
  write.csv(city_data_d,paste0("city_Temp_data","_d",".csv"),row.names = FALSE)
  write.csv(nation_data_d,paste0("nation_Temp_data","_d",".csv"),row.names = FALSE)
    
}
