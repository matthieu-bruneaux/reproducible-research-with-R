# figure 01 - Map of earthquakes off Fiji
# using R dataset 'quakes'



library(RgoogleMaps)

source("./source.R")

source("./config.R")



# file properties
#----------------

file.name = "figure02.pdf"

pdf.width = 4

pdf.height = 4

pdf.pointsize = 12

figure.directory = "./output"



# data
#-----

long = quakes$long

lat = quakes$lat

depth = quakes$depth

mag = quakes$mag



# parameters
#-----------



### limits ###

xlim = c(165, 190)

ylim = c(-40, -10)

### colors ###

depth.ncol = 100

depth.col.shallow = cfg.depth.col.shallow

depth.col.deep = cfg.depth.col.deep

### axes and title ###

x.values.at = seq(165, 190, by = 5)

y.values.at = seq(-40, -10, by = 10)

title.label = "Locations of earthquakes off Fiji"




# plot
#-----



### open the file ###

pdf(paste(figure.directory, file.name, sep = "/"),
    width = pdf.width, height = pdf.height, pointsize = pdf.pointsize)



### get the map from google ###

map = GetMap.bbox(lonR = xlim, latR = ylim, zoom = 5, maptype = "satellite")



### prepare the color palette for depth ###

color.gradient = colorRampPalette(c(depth.col.shallow, depth.col.deep))

depth.colors = color.gradient(depth.ncol)



### draw the points ###

PlotOnStaticMap(map, 
                lon = long, lat = lat, 
                col = depth.colors[cut(depth, depth.ncol)], 
                pch = 16, cex = 0.5,
                mar = c(2, 2, 2, 2),
                axes = T)



### title ###

title(title.label)



### close the pdf ###

dev.off()



### clean the directory ###

file.remove("MyTile.png", "MyTile.png.rda")
