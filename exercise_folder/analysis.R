#------------------
# Load the CTD data
#------------------



data = read.table("../data/CTD_Baltic_Sea_1975-2013.csv", sep = ",", header = T, fill = T)

# Let's have a look at what the data frame contains.

names(data)

# We are mainly interested in:
#  - location (x = longitude, y = latitude)
#  - date (yyyy.mm.ddThh.mm)
#  - depth (PRES..db, which corresponds to the water pressure at sampling, while 
#      Bot..Depth..m. is the maximum depth reached by the CTD bottle)
#  - temperature (TEMP..deg.c)
#  - dissolved oxygen (DOXY..ml.l.)

# The dissolved oyxgen column (DOXY..ml.l) was loaded as a factor because it contains
# non-numerical values ("<0.01"). Some conversion is needed here. Note that dissolved
# oxygen values are not available for all locations.

  # set the low values to 0.01 ml/l

data$DOXY..ml.l.[data$DOXY..ml. == "<0.01"] = "0.01"

  # convert the factor to numerical values

data$DOXY..ml.l. = as.numeric(as.character(data$DOXY..ml.l.))

# The date and time data also have to be converted to a format that R understands. The
# simplest way is to convert it to POSIX format, see ?DateTimeClasses for more details.

data$date = strptime(data$yyyy.mm.ddThh.mm, format = "%Y-%m-%dT%H:%M")

# Now the table is ready. We can keep a copy of the full table and prepare a lighter
# version with only the columns that we need.

data_full = data

data = data_full[, c(6, 5, 12, 8, 9, 11)]

names(data) = c("x_lon", "y_lat", "date", "depth_m", "temp_C", "DO_ml.l")

head(data)



#-------------------------
# Load the topography data
#-------------------------



bathy_full = read.table("../data/iowtopo2_rev03.dat", header = T)

# The topography dataset is big. We can take a subset of it to make plots faster.

bathy = bathy_full[seq(1, nrow(bathy_full), 100), ]


