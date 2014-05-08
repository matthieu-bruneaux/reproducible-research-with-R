# figure 01 - Map of earthquakes off Fiji
# using R dataset 'quakes'



source("./source.R")

source("./config.R")



# file properties
#----------------

file.name = "figure01.pdf"

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



### limits and margins ###

margins = c(2, 3, 2, 0)

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

### legend ###

xlim.legend = c(166, 168)

ylim.legend = c(-32, -38)



# plot
#-----



### open the file ###

pdf(paste(figure.directory, file.name, sep = "/"),
    width = pdf.width, height = pdf.height, pointsize = pdf.pointsize)



### empty plot ###

par(mar = margins) # set the outer margins

plot(0, 0, type = "n", 
     xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", 
     axes = F, bty = "n",
     asp = 1)



### prepare the color palette for depth ###

color.gradient = colorRampPalette(c(depth.col.shallow, depth.col.deep))

depth.colors = color.gradient(depth.ncol)



### draw the points ###

points(long, lat, col = depth.colors[cut(depth, depth.ncol)], pch = 16, cex = 0.5)



### axes and title ###

axis(1, at = x.values.at)

axis(2, at = y.values.at, las = 1)

title(title.label)



### legend ###

draw.legend.stripe(xlim.legend, ylim.legend, depth.colors, min(depth), max(depth),
                   title = "Depth (km)")




### close the pdf ###

dev.off()



