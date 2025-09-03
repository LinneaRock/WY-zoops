

library(tidyverse)
library(terra)


wy_lakes <- vect('C:/Users/linne/Downloads/major_lakes_reservoirs/major_lakes_reservoirs.shp')

str(wy_lakes)

wy_lakes$NAME

terra::plot(wy_lakes)

wy_lakes_df <- as.data.frame(wy_lakes)

wy_lakes_filtered <- wy_lakes[wy_lakes$NAME %in% c('Alcova Reservoir', 'Boysen Reservoir', 'Fontenelle Reservoir', 'Glendo Reservoir', 'Grayrocks Reservoir', 'Guernsey Reservoir'),]

wy_lakes_filtered$NAME

terra::plot(wy_lakes_filtered, col='NAME')
