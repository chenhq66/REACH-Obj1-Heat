install.packages("ncdf4")
install.packages("raster")
install.packages("RNetCDF")

library(ncdf4)
library(RNetCDF)

file_list <- list.files(path = "path/to/your/nc/files", pattern = "*.nc", full.names = TRUE)

# Open the first file to get dimensions and variables information
nc1 <- nc_open(file_list[1])

# Extract dimensions and variables
lon <- ncvar_get(nc1, "longitude")
lat <- ncvar_get(nc1, "latitude")
time <- ncvar_get(nc1, "time")  
temp <- ncvar_get(nc1, "t2m")  # replace "t2m" with your variable name

# Close the file
nc_close(nc1)

# Initialize a vector to store the time data
all_time <- time
all_temp <- temp

# Loop over the remaining files to append the data
for (i in 2:length(file_list)) {
  nc <- nc_open(file_list[i])
  
  # Extract time and temperature data
  time <- ncvar_get(nc, "time")
  temp <- ncvar_get(nc, "t2m")
  
  # Append the data
  all_time <- c(all_time, time)
  all_temp <- abind::abind(all_temp, temp, along = 3)
  
  # Close the file
  nc_close(nc)
}

# Define the dimensions for the new NetCDF file
time_dim <- ncdim_def("time", units = "hours since 1900-01-01", vals = all_time)
lat_dim <- ncdim_def("latitude", "degrees_north", lat)
lon_dim <- ncdim_def("longitude", "degrees_east", lon)

# Define the temperature variable
temp_var <- ncvar_def("t2m", "K", list(lon_dim, lat_dim, time_dim), -999, longname = "2 meter temperature")

# Create a new NetCDF file
nc_new <- nc_create("merged_temperature.nc", list(temp_var))

# Write data to the new file
ncvar_put(nc_new, temp_var, all_temp)

# Close the new file
nc_close(nc_new)

