#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Filter NHD dataset to get just our lakes #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




#U.S. Geological Survey, National Geospatial Program. (2023). USGS National Hydrography Dataset Best Resolution (NHD) – Wyoming (published 20231227) Shapefile. U.S. Geological Survey. https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/Shape/NHD_H_Wyoming_State_Shape.zip



library(tidyverse)
library(sf)



wy_nhd <- st_read("C:/Users/linne/Downloads/NHD_H_Wyoming_State_Shape/Shape/NHDWaterbody.shp")
str(wy_nhd)



sites_sf <- read.csv('Data/clean_data/site_metadata.csv') |>
  rename(Reservoir_Name=Reservoir_FullName,
         Reservoir_ID=Reservoir) |>
  select(-c(Sample_Year, Combined_Res_Site_ID, Location)) |>
  st_as_sf(coords=c('Longitude','Latitude'), crs=4269)


st_crs(sites_sf) == st_crs(wy_nhd)
# TRUE


# perform spatial join to find which polygons contain the sampling points
lakes_sf <- st_join(wy_nhd, sites_sf, join=st_contains)

lakes_sf_filtered <- lakes_sf |>
  filter(!is.na(Reservoir_ID)) |>
  distinct(geometry, .keep_all = TRUE) # Post Lake is Wheatland #3

# check that we have all our lakes
unique(sites$Reservoir_Name)



leaflet::leaflet() |>
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = lakes_sf_filtered,
    color = "blue",
    weight = 1,
    fillColor = "lightblue",
    fillOpacity = 0.5,
    popup = ~paste0("Lake ID: ", gnis_name)  # customize with your field
  ) %>%
  addCircleMarkers(
    data = sites_sf,
    radius = 4,
    color = "red",
    fill = TRUE,
    fillOpacity = 0.8,
    popup = ~Reservoir_ID   # replace with your point ID column
  )

# Great! This looks really good


# save a filtered dataset for easy access later to shapefiles

wy_nhd_filtered <- lakes_sf_filtered |>
  select(-c(fdate,resolution,ftype,fcode,visibility,SHAPE_Leng,SHAPE_Area,Site_ID,Res_Zone,Site_Name))

st_write(wy_nhd_filtered, 'Data/clean_data/Spatial/WY_NHD.gpkg', layer='WY_NHD', delete_layer = TRUE)
st_write(sites_sf, 'Data/clean_data/Spatial/WY_NHD.gpkg', layer='sites_sf', delete_layer = TRUE)

st_layers('Data/clean_data/Spatial/WY_NHD.gpkg') # looks good!
