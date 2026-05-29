install.packages("magick")
library(magick)

densitytransparents <- image_read("/Users/violetlemley/Documents/128/QGIS/Clean/densitytransparent.png")
densitytransparents <- image_transparent(densitytransparents, color = "white", fuzz = 5)
image_write(densitytransparents, "/Users/violetlemley/Desktop/final_map_transparent.png")

carcasstransparent <- image_read("/Users/violetlemley/Documents/128/QGIS/Clean/carcasstransparent.png")
carcasstransparent <- image_transparent(carcasstransparent, color = "white", fuzz = 5)
image_write(carcasstransparent, "/Users/violetlemley/Documents/128/R/final_carcassmap_transparent.png")
