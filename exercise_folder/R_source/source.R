# source code for Baltic Sea oceanographic data analysis



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
