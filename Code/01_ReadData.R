#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to pull and combine data #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)


sites <- read.csv('Data/clean_data/site_metadata.csv') |>
  rename(Reservoir_Name=Reservoir_FullName,
         Reservoir_ID=Reservoir) |>
  select(-c(Sample_Year, Combined_Res_Site_ID, Location))


chl21 <- read.csv('Data/raw_data/chl_data_2021_final.csv') |>
  mutate(Reservoir_ID = substr(lake_id, 1, 3)) |>
  rename(chl_a_ugL=chl_a..ug.L.,
         Site_ID = siteID,
         Collection_Date = Date) |>
  mutate(chl_a_ugL=as.numeric(chl_a_ugL)) |>
  mutate(Collection_Date = as.Date(as.character(Collection_Date), format='%Y%m%d')) |>
  select(Reservoir_ID, Site_ID, Collection_Date, chl_a_ugL)


wq21 <- read.csv('Data/clean_data/2021_WQ_SS_clean.csv') |>
  mutate(datecode=Collection_Date, 
         Collection_Date = as.Date(as.character(Collection_Date), format='%Y%m%d')) |>
  select(-chl_a_ugL) |> # I trust the chla 2021 datasheet more than what was in the daily sample logs
  left_join(chl21) |>
  group_by(Reservoir_ID,Reservoir_Name,Site_ID,Collection_Date) |>
  mutate(across(c(Temp_C:chl_a_ugL), mean, na.rm=TRUE)) |>
  ungroup() |>
  select(-DUP) |>
  distinct() |>
  select(-Reservoir_Name)

rm(chl21)

wq22 <- read.csv('Data/clean_data/2022_WQ_AP_clean.csv')  |>
  mutate(datecode=Collection_Date,
         Collection_Date = as.Date(as.character(Collection_Date), format='%Y%m%d')) |>
  group_by(Reservoir_ID,Site_ID,Collection_Date) |>
  mutate(across(c(Temp_C:chl_a_ugL), mean, na.rm=TRUE)) |>
  ungroup() |>
  select(-DUP) |>
  distinct()

water_quality <- rbind(wq21, wq22) |>
  left_join(sites)

rm(wq21,wq22)

zoops <- read.csv('Data/clean_data/2122_Zoops_AP_clean.csv') |>
  mutate(Collection_Date=as.Date(Collection_Date, format='%m/%d/%Y'),
         datecode=format(Collection_Date, '%Y%m%d')) |>
  select(-c(X,Site_ID_pseudonym,Sample_ID,FTG_Combined,Trophic_Group_General)) |>
  group_by(Reservoir_ID, Reservoir_Name,Site_ID,Collection_Date,Species_Name, Functional_Group,Trophic_Group,Feeding_Type) |>
  mutate(across(c(biomass_ugL, Count, Size), mean, na.rm=TRUE)) |>
  ungroup() |>
  select(-DUP, -Reservoir_Name) |>
  distinct() |>
  left_join(sites)

sif <- read.csv('Data/clean_data/SIF_data_20250626_LipidCorrected.csv') |>
  separate_wider_delim(
    cols = Sample_ID,
    delim = "_",
    names = c("Reservoir_ID", "Site_ID", "Collection_Date", "Extra_Info"),
    too_few = "align_start",
    too_many = "merge"   # merge everything after the 3rd underscore
  ) |>
  mutate(datecode=Collection_Date,
         Collection_Date = as.Date(Collection_Date, format = "%Y%m%d")
  ) |>
  group_by(Reservoir_ID, Site_ID,Collection_Date) |>
  mutate(across(c(percent_N:d34S), mean, na.rm=TRUE)) |>
  ungroup() |>
  select(-c(Extra_Info, Notes, C_N_ratio, perc_lipid, d13C_corrected)) |>  
  filter(!is.na(Collection_Date)) |>
  distinct() |>
  #fixing mislabeled dates
  mutate(Collection_Date=ifelse(Reservoir_ID=='ALS' & Collection_Date==as.Date('2022-07-25'), as.Date('2022-07-15'), Collection_Date)) |>
  mutate(Collection_Date=as.Date(Collection_Date)) |>
  mutate(datecode=ifelse(Reservoir_ID=='ALS' & Collection_Date==as.Date('2022-07-15'), '20220715', datecode)) |>
  mutate(Collection_Date=ifelse(Reservoir_ID=='WHE' & Collection_Date==as.Date('2021-07-01'), as.Date('2021-07-07'), Collection_Date)) |>
  mutate(Collection_Date=as.Date(Collection_Date)) |>
  mutate(datecode=ifelse(Reservoir_ID=='ALS' & Collection_Date==as.Date('2021-07-07'), '20210707', datecode)) |>
  left_join(sites)


# NOTE:
# Need to get lipid correction calculations from Willie + citation for that 


sites_sf <- sf::st_read('Data/clean_data/Spatial/WY_NHD.gpkg', layer='sites_sf')
lakes_sf <- sf::st_read('Data/clean_data/Spatial/WY_NHD.gpkg', layer='WY_NHD')


