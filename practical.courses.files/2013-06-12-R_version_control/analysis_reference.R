#-----------------------
# Load the R source code
#-----------------------



# We put all the heavy and complicated functions in a different source file. This
# enables us to keep only the main stream analysis in this file, which is clearer
# and easier to follow and modify.
# This approach is based on having a source file with well-written functions, i.e.
# well documented with comments and as free of bugs as possible !

source("R_source/source.R")



#------------------
# Load the CTD data
#------------------



CTD_data_full = read.table("data/CTD_Baltic_Sea_1975-2013.csv", sep = ",", header = T, fill = T)

# Let's have a look at what the data frame contains.

head(CTD_data_full)

# We have to process the table (convert factors to numerical values, convert date format to
# something that R can use, and keep only the relevant columns)

CTD_data = processCTD(CTD_data_full)

head(CTD_data)



#------------------------------------------
# Load the topography data (for bathymetry)
#------------------------------------------



bathy_full = read.table("data/iowtopo2_rev03.dat", header = T)

# The topography dataset is big. We can take a subset of it to make plots faster.

bathy = decreaseGridResolution(bathy_full, 2)

head(bathy)



#------------------------------------
# Filter the data for a specific area
#------------------------------------



x_lon_limits = c(16.1, 32.5) # Gulf of Finland and Aland Islands

y_lat_limits = c(56.3, 61.7)

CTD_data = extractAreaByCoordinates(CTD_data, x_lon_limits, y_lat_limits)

bathy = extractAreaByCoordinates(bathy, x_lon_limits, y_lat_limits)



#-----------------------------------------
# Draw a bathymetric map of the Baltic Sea
#-----------------------------------------



bathy = addSeaDepthColor(bathy, bathy$z_topo.m., depthExponent = 3) # add the color

plot(bathy$x_lon, bathy$y_lat, col = bathy$color, pch = 16, cex = 0.5)



#----------------------------------------
# Evolution of summer surface temperature
#----------------------------------------



# First let's plot all temperature values (all depths) across the
# year (from 1st of January to 31st of December).

plot(CTD_data$date$yday, CTD_data$temp_C, pch = 16, cex = 0.2)

# The temperature seems to be maximal between days 200 and 225.
# Let's get the yearly average for surface temperature for this period.


depth_limits = c(0, 10)

year_day_limits = c(200, 225)

year_limits = c(1975, 2013)

surface_temp = calculateAverageTempPerYear(CTD_data, depth_limits,
                                           year_day_limits, year_limits)

surface_temp

plot(surface_temp)

# The data is insufficient to lead a long-term analysis.



#-------------------------------------------
# Temperature profile in relation with depth
#-------------------------------------------



plot(CTD_data$depth_m, CTD_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200))



#----------------------------------------------------
# Seasonal temperature profile in relation with depth
#----------------------------------------------------



# We define the days corresponding roughly to the seasons.

winter = c(0, 90)

summer = c(180, 250)

# We extract the data for each season

winter_data = extractDataByYearDay(CTD_data, winter)

summer_data = extractDataByYearDay(CTD_data, summer)

# We plot separately the temperature profiles for each season.

plot(winter_data$depth_m, winter_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200), ylim = c(0, 20), main = "Winter")

plot(summer_data$depth_m, summer_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200), ylim = c(0, 20), main = "Summer")



#-----------------------------
# Bathymetric map with ggplot2
#-----------------------------



# Load ggplot2 library

library(ggplot2)

# Get the topography data for sea only

bathy_ggplot = subset(bathy, bathy$z_water.m. < 0)

# Convert the depth to a factor

bathy_ggplot$z_factor = cut(bathy_ggplot$z_water.m., seq(-400, 0, 25))

# Build the bathymetric map

bathymetric_map = ggplot(bathy_ggplot, aes(x = x_lon, y = y_lat, fill = z_factor)) +
  geom_tile() +
  scale_fill_manual("depth (m)", 
                    values = scales::seq_gradient_pal(low = "#131B23",
                                                      high = "#56B1F7",
                                                      space = "Lab")(seq(0, 1, length.out = 15)),
                    guide = guide_legend(reverse = T)) +
  geom_contour(aes(z = z_water.m., fill = NULL),
               breaks = seq(0, -200, -25),
               colour= "black") +
  xlab("Longitude") + ylab("Latitude")

# Display the map

bathymetric_map



#----------------------------------
# Draw the CTD locations on the map
#----------------------------------



# Get the CTD coordinates

CTD_coord = unique(CTD_data[, c("x_lon", "y_lat")])

# Build the CTD location map

CTD_map = geom_point(data = CTD_coord, aes(x = x_lon, y=y_lat, fill = NULL),
                     col = "orange")

# Display the maps

bathymetric_map + CTD_map



#--------------------------------------
# Save figures for temperature profiles
#--------------------------------------



# figure for winter profile

pdf("figures/temperature_profile_winter.pdf", 5, 4)

plot(winter_data$depth_m, winter_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200), ylim = c(0, 20), main = "Winter")

dev.off()

# figure for summer profile

pdf("figures/temperature_profile_summer.pdf", 5, 4)

plot(summer_data$depth_m, summer_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200), ylim = c(0, 20), main = "Summer")

dev.off()



#----------------------------------------------------
# Save figures for temperature profiles with Git hash
#----------------------------------------------------



# figure for winter profile

file_name = paste("figures/temperature_profile_winter_", getGitHash(),
                  ".pdf", sep = "")

pdf(file_name, 5, 4)

plot(winter_data$depth_m, winter_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200), ylim = c(0, 20), main = "Winter")

dev.off()

# figure for summer profile

file_name = paste("figures/temperature_profile_summer_", getGitHash(),
                  ".pdf", sep = "")

pdf(file_name, 5, 4)

plot(summer_data$depth_m, summer_data$temp_C, pch = 16, cex = 0.2,
     xlim = c(0, 200), ylim = c(0, 20), main = "Summer")

dev.off()



#-------------------------------------
# Save tables for temperature profiles
#-------------------------------------



# write the winter data

file_name = paste("results/temperature_profile_winter_", getGitHash(),
                  ".csv", sep = "")

write.csv(winter_data, file_name)

# write the summer data

file_name = paste("results/temperature_profile_summer_", getGitHash(),
                  ".csv", sep = "")

write.csv(summer_data, file_name)


