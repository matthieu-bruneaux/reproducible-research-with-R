draw.legend.stripe = function(xcorners, ycorners, colors, startValue, endValue, title = "",
                              cex = 0.4) {
  
  # Draw a color strip as a legend for a plot
  #
  # TAKES
  # xcorners: c(x1, x2) defining the x limits of the stripe
  # ycorners: c(y1, y2) defining the y limits of the stripe
  # colors: vector containing all the colors used in the scale
  # startValue: numerical value mapping to the first color
  # endValue: numerical value mapping to the last color
  # title: string, title of the legend
  # cex: size factor for strings
  
  # reverse sort colors
  
  colors = rev(colors)
  
  # sort x and y corners
  
  x = sort(xcorners)
  
  y = sort(ycorners)
  
  # draw the color strip
  
  n.colors = length(colors)
  
  step.height = diff(y) / n.colors
  
  overlay = 0.1 * step.height
  
  for (i in 1:n.colors) {
    
    x1 = x[1]
    
    x2 = x[2]
    
    y1 = y[1] + (i - 1) * step.height - overlay
    
    y2 = y1 + step.height + overlay
    
    rect(x1, y1, x2, y2, col = colors[i], border = NA)
    
  }
  
  # add the start and end values
  
  text(x[2], y[2], as.character(startValue), pos = 4, cex = cex)

  text(x[2], y[1], as.character(endValue), pos = 4, cex = cex)
  
  # add the title
  
  if (title != "") {
    
    text(mean(x), y[2], title, pos = 3, cex = cex)
    
  }
  
}
