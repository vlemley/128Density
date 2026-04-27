install.packages(c("sf", "ggplot2"))
library(sf)
library(ggplot2)

#reading in 20260221 count 
ANMcount <- st_read(file.choose())

#displaying 20260221 count data with ggplot (SUCCESS)
ggplot(data = ANMcount) + geom_sf()

#reading in ANMmap shp
ANMmap <- st_read('/Users/violetlemley/Documents/128/QGIS/ANMbeachShapefile/Ano Nuevo Map.shp')

#displaying ANMmap (TBD)
ggplot(data = ANMmap) +
  geom_sf(fill = "lightblue", color = "black")

##troubleshooting, checking for polygons
st_geometry_type(ANMmap)

#fixing invalid geometries, seems like coordinates exceed valid range
ANMmap <- st_make_valid(ANMmap)
ggplot(data = ANMmap) + geom_sf()

#checking for coordinate reference system
st_crs(ANMmap)

#checking coordinates themselves
st_bbox(ANMmap)

#xmax is exactly 180, broke polygon rendering, need to a
shp <- st_wrap_dateline(shp, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
shp <- st_make_valid(shp)

ggplot(data = shp) +
  geom_sf(fill = "lightblue", color = "black")




