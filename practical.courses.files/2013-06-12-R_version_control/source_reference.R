# source code for Baltic Sea oceanographic data analysis



#-------------------------
# Process a CTD data frame
#-------------------------



processCTD = function(CTD_data) {
  
  # Process a CTD table from ICES website and extract the interesting
  # columns.
  #
  # TAKES
  # CTD_data: a CTD data table as downloaded from ICES
  #
  # RETURNS
  # a table with the relevant columns
  
  # The dissolved oyxgen column (DOXY..ml.l) is loaded as a factor because it contains
  # non-numerical values (e.g. "<0.01"). Some conversion is needed here. Note that 
  # dissolved oxygen values are not available for all locations.
  
  # set the low values to 0.01 ml/l
  
  CTD_data$DOXY..ml.l.[CTD_data$DOXY..ml. == "<0.01"] = "0.01"
  
  # convert the factor to numerical values
  
  CTD_data$DOXY..ml.l. = as.numeric(as.character(CTD_data$DOXY..ml.l.))
  
  # The date and time data also have to be converted to a format that R understands. The
  # simplest way is to convert it to POSIX format, see ?DateTimeClasses for more details.
  
  CTD_data$date = strptime(CTD_data$yyyy.mm.ddThh.mm, format = "%Y-%m-%dT%H:%M")
  
  # Now the table is ready. We prepare a lighter version with only the columns that we need.
  
  data = CTD_data[, c("Longitude..degrees_east.", 
                      "Latitude..degrees_north.", 
                      "date",
                      "PRES..db.",
                      "TEMP..deg.C.",
                      "DOXY..ml.l.")]
  
  names(data) = c("x_lon", "y_lat", "date", "depth_m", "temp_C", "DO_ml.l")
  
  # return
  
  data  
  
}



#--------------------------------------
# Decrease a topography grid resolution
#--------------------------------------



decreaseGridResolution = function(topo, step) {
  
  # Decrease the resolution of a topography grid.
  #
  # TAKES
  # topo: a table with x_lon and y_lat columns. Those columns must
  #   represent a regular grid.
  # step: the step between two successive subset points
  #
  # RETURNS
  # a table with a subset of the initial grid, with points regularly
  #   sampled in the initial grid every 'step' initial points.
  
  # get the x and y coordinates
  
  all_x = sort(unique(topo$x_lon))
 
  all_y = sort(unique(topo$y_lat))
  
  # prepare the subset of x and y coordinates
  
  subset_x = all_x[seq(1, length(all_x), by = step)]
  
  subset_y = all_y[seq(1, length(all_y), by = step)]
  
  # extract the new grid
  
  output = subset(topo, (topo$x_lon %in% subset_x & 
                         topo$y_lat %in% subset_y))
  
  # return
  
  output
  
}



#---------------------------------------------------------
# Add a color value for the sea depth to a topography grid
#---------------------------------------------------------



addSeaDepthColor = function(topo, z, surfaceColor = "deepskyblue", maxDepthColor = "navyblue", 
                            depthExponent = 1) {
  
  # Add a color corresponding to the sea depth to a topography table.
  #
  # TAKES
  # topo: a table with topography data
  # z: a vector of elevation. Sea depth corresponds to negative values.
  # surfaceColor: color for sea surface
  # maxDepthColor: color for the deepest point.
  # depthExponent : numerical value >= 1 used to increase the rate at which color
  #   changes with depth.
  #
  # RETURNS
  # the initial topo table with a new or updated color column
  
  # get the existing color column if any
  
  if ("color" %in% names(topo)) {
    
    color = topo$color
  }
  
  else {
    
    color = rep(NA, nrow(topo))
    
  }
  
  # prepare empty R, G, B vectors
  
  R = rep(NA, nrow(topo))
  
  G = rep(NA, nrow(topo))
  
  B = rep(NA, nrow(topo))
  
  # calculate the depth range
  
  depth_range = range(z[z<=0])
  
  max_depth = depth_range[1]
  
  delta_depth = diff(depth_range)
  
  # scale the z vector to depth (0, 1)
  
  z = ((z - max_depth) / delta_depth) ^ depthExponent
  
  which_sea = which(z < 1)
  
  # get the rgb values for surface and deepest point
  
  surfaceRGB = col2rgb(surfaceColor)
  
  maxDepthRGB = col2rgb(maxDepthColor)
  
  delta_R = surfaceRGB[1] - maxDepthRGB[1]
  
  intercept_R = maxDepthRGB[1]
  
  delta_G = surfaceRGB[2] - maxDepthRGB[2]
  
  intercept_G = maxDepthRGB[2]
  
  delta_B = surfaceRGB[3] - maxDepthRGB[3]
  
  intercept_B = maxDepthRGB[3]
  
  # calculate the new colors
  
  R[which_sea] = (z[which_sea] * delta_R + intercept_R) / 255
  
  G[which_sea] = (z[which_sea] * delta_G + intercept_G) / 255
  
  B[which_sea] = (z[which_sea] * delta_B + intercept_B) / 255
  
  for (i in 1:length(which_sea)) {
  
    color[which_sea[i]] = rgb(R[which_sea[i]],
                              G[which_sea[i]],
                              B[which_sea[i]])
    
  }
  
  # return
  
  topo$color = color
  
  topo
  
}



#------------------------------------
# Filter a table based on coordinates
#------------------------------------



extractAreaByCoordinates = function(data, x_lon_limits, y_lat_limits) {
  
  # Filter the rows of a table based on longitude and latitude coordinates.
  # 
  # TAKES
  # data: a table with x_lon and y_lat columns
  # x_lon_limits: a vector c(longiture_min, longitude_max)
  # y_lat_limits: a vector c(latitude_min, latitude_max)
  #
  # RETURNS
  # a table with all the rows for which the coordinates are comprised
  # in the limits defined by x_lon_limits and y_lat_limits
  
  # filtering
  
  output = subset(data, (data$x_lon >= x_lon_limits[1] &
                           data$x_lon <= x_lon_limits[2] &
                           data$y_lat >= y_lat_limits[1] &
                           data$y_lat <= y_lat_limits[2]))
  
  # return
  
  output
  
}



#---------------------------------
# Filter a table based on year day
#---------------------------------



extractDataByYearDay = function(data, year_day_limits) {
  
  # Filter the rows of a table based of the year day (1-365).
  #
  # TAKES
  # data: a table with a date column in the POSIXlt format
  # year_day_limits: a vector with the limits for year day values
  #   (e.g. c(1, 31) will select all of January, c(1,61) will select
  #    January and February)
  #
  # RETURNS
  # a subset of the table corresponding to the specified year day range.
  
  # filter
  
  output = subset(data, (data$date$yday >= year_day_limits[1] &
                         data$date$yday <= year_day_limits[2]))
  # return
  
  output
  
}



#------------------------------
# Filter a table based on depth
#------------------------------



extractDataByDepth = function(data, depth_limits) {
  
  # Filter the rows of a table based of the depth.
  #
  # TAKES
  # data: a table with a depth column
  # depth_limits: a vector with the limits for depth (e.g. c(0, 20) 
  #   for selection from surface until 20m deep)
  #
  # RETURNS
  # a subset of the table corresponding to the specified depth range.
  
  # filter
  
  output = subset(data, (data$depth_m >= depth_limits[1] &
                         data$depth_m <= depth_limits[2]))
  # return
  
  output
  
}



#-------------------------------------------
# Filter a table based on depth and year day
#-------------------------------------------



extractDataByDepthAndYearDay = function(data, depth_limits, year_day_limits) {
  
  # Filter the rows of a table based of the depth and year day date.
  #
  # TAKES
  # data: a table with a depth column and a date column in the POSIXlt 
  #   format
  # depth_limits: a vector with the limits for depth (e.g. c(0, 20) 
  #   for selection from surface until 20m deep)
  # year_day_limits: a vector with the limits for year day values
  #   (e.g. c(1, 31) will select all of January, c(1,61) will select
  #    January and February)
  #
  # RETURNS
  # a subset of the table corresponding to the specified depth and date 
  #   ranges.
  
  # filter
  
  output = subset(data, (data$depth_m >= depth_limits[1] &
                         data$depth_m <= depth_limits[2] &
                         data$date$yday >= year_day_limits[1] &
                         data$date$yday <= year_day_limits[2]))
  # return
  
  output
  
}



#---------------------------------------
# Filter a table based on depth and date
#---------------------------------------



extractDataByDepthAndDate = function(data, depth_limits, year_day_limits, 
                                        year) {
  
  # Filter the rows of a table based of the depth, year day date and
  # year.
  #
  # TAKES
  # data: a table with a depth column and a date column in the POSIXlt 
  #   format
  # depth_limits: a vector with the limits for depth (e.g. c(0, 20) 
  #   for selection from surface until 20m deep)
  # year_day_limits: a vector with the limits for year day values
  #   (e.g. c(1, 31) will select all of January, c(1,61) will select
  #    January and February)
  # year: year value (e.g. 1991)
  #
  # RETURNS
  # a subset of the table corresponding to the specified depth and date.
  
  # filter
  
  output = subset(data, (data$depth_m >= depth_limits[1] &
                           data$depth_m <= depth_limits[2] &
                           (data$date$year + 1900) == year &
                           data$date$yday >= year_day_limits[1] &
                           data$date$yday <= year_day_limits[2]))
  # return
  
  output
  
}



#-----------------------------------------------------------------
# Calculate average temperature per year for depth and date ranges
#-----------------------------------------------------------------



calculateAverageTempPerYear = function(data, depth_limits, year_day_limits,
                                       year_limits) {
  
  # Calculate the average temperature for every year within year_limits 
  # for some given depth and year day ranges.
  #
  # TAKES
  # data: a table with a depth column and a date column in the POSIXlt 
  #   format
  # depth_limits: a vector with the limits for depth (e.g. c(0, 20) 
  #   for selection from surface until 20m deep)
  # year_day_limits: a vector with the limits for year day values
  #   (e.g. c(1, 31) will select all of January, c(1,61) will select
  #    January and February)
  # year_limits: year limits (e.g. c(1991, 2001))
  #
  # RETURNS
  # a table with the average temperature for each year.
  
  # prepare output
  
  year = year_limits[1]:year_limits[2]
  
  average_temp = rep(NA, length(year))
  
  # do the calculations
  
  for (i in 1:length(year)) {
    
    data_year = extractDataByDepthAndDate(data, depth_limits, year_day_limits,
                                          year[i])
    
    if (nrow(data_year) == 0) {
      
      average_temp[i] = NA
      
    }
    
    else {
      
      average_temp[i] = mean(data_year$temp_C)
      
    }
    
  }
  
  # return
  
  data.frame(year = year, temp_C = average_temp)
  
}



#---------------------
# Get current Git hash
#---------------------



getGitHash = function() {
  
  # Get the hash of the current Git commit
  #
  # RETURNS
  # a string containing the short hash for the current commit
  
  # get the git hash using a shell git command
  
  a = pipe("git rev-parse HEAD")
  
  output = substr(readLines(a), 1, 8)
  
  close(a)
  
  # return
  
  output
  
}


