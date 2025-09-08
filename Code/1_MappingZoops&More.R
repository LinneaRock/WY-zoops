#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Basic mapping and averages #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(sf)

WY <- map_data('state') |>
  filter(region=='wyoming')


# basic map showing lake locations
ggplot() +
  geom_polygon(WY, mapping=aes(long, lat), fill='white', color='black') +
  geom_sf(lakes_sf, mapping=aes(), color='blue4', fill='blue4')


# average stable isotope maps
sif_average <- sif |>
  group_by(Reservoir_Name) |>
  summarise(meand15N=mean(d15N, na.rm=TRUE),
            meand13C=mean(d13C, na.rm=TRUE),
            meand34S=mean(d34S, na.rm=TRUE)) |>
  ungroup() |>
  left_join(lakes_sf) |> st_as_sf()

st_crs(sif_average)
  
ggplot() +
  #geom_polygon(WY, mapping=aes(long, lat), fill='white', color='black') +
  geom_sf(sif_average, mapping=aes(fill=meand15N,color=meand15N)) +
  scale_fill_viridis_c()+ scale_color_viridis_c()

ggplot() +
 # geom_polygon(WY, mapping=aes(long, lat), fill='white', color='black') +
  geom_sf(sif_average, mapping=aes(fill=meand13C, color=meand13C)) +
  scale_fill_viridis_c() + scale_color_viridis_c()

ggplot() +
  # geom_polygon(WY, mapping=aes(long, lat), fill='white', color='black') +
  geom_sf(sif_average, mapping=aes(fill=meand34S, color=meand34S)) +
  scale_fill_viridis_c()+ scale_color_viridis_c()


sif_average_sites <- sif |>
  group_by(Site_Name) |> 
  summarise(meand15N=mean(d15N, na.rm=TRUE),
                               meand13C=mean(d13C, na.rm=TRUE),
                               meand34S=mean(d34S, na.rm=TRUE)) |>
  ungroup() |>
  left_join(sites |>
              group_by(Site_Name) |>
              summarise(Longitude=mean(Longitude),
                         Latitude=mean(Latitude)) |>
              ungroup()|>
              st_as_sf(coords=c('Longitude','Latitude'), crs=4269)) |>
  st_as_sf()
 
library(leaflet) 

palN <- colorNumeric(
  palette = "viridis",                   # color scale
  domain = sif_average_sites$meand15N         # numeric range
)

leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    data = lakes_sf,
    color = "blue",
    weight = 1,
    fillColor = "lightblue",
    fillOpacity = 0.5,
    popup = ~paste0("Lake ID: ", gnis_name)  # customize with your field
  ) %>%
  addCircleMarkers(
    data = sif_average_sites,
    radius = 6,
    color = ~palN(meand15N),   # color by meand15N
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(Site_Name, "<br>δ15N: ", meand15N)
  ) |>
  addLegend(
    pal = palN,
    values = sif_average_sites$meand15N,
    title = "Mean δ15N"
  )



palC <- colorNumeric(
  palette = "viridis",                   # color scale
  domain = sif_average_sites$meand13C         # numeric range
)


leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    data = lakes_sf,
    color = "blue",
    weight = 1,
    fillColor = "lightblue",
    fillOpacity = 0.5,
    popup = ~paste0("Lake ID: ", gnis_name)  # customize with your field
  ) %>%
  addCircleMarkers(
    data = sif_average_sites,
    radius = 6,
    color = ~palC(meand13C),   # color by meand15N
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(Site_Name, "<br>δ13C: ", meand13C)
  ) |>
  addLegend(
    pal = palC,
    values = sif_average_sites$meand13C,
    title = "Mean δ13C"
  )



palS <- colorNumeric(
  palette = "viridis",                   # color scale
  domain = sif_average_sites$meand34S         # numeric range
)


leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    data = lakes_sf,
    color = "blue",
    weight = 1,
    fillColor = "lightblue",
    fillOpacity = 0.5,
    popup = ~paste0("Lake ID: ", gnis_name)  # customize with your field
  ) %>%
  addCircleMarkers(
    data = sif_average_sites,
    radius = 6,
    color = ~palS(meand34S),   # color by meand15N
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(Site_Name, "<br>δ34S: ", meand34S)
  ) |>
  addLegend(
    pal = palS,
    values = sif_average_sites$meand34S,
    title = "Mean δ34S"
  )



detach('package:sf', unload=TRUE)
