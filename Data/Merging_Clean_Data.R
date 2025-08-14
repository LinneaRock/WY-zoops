#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Merging data to see what we have 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)

# read in data and average across duplicates 
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
  distinct()

wq22 <- read.csv('Data/clean_data/2022_WQ_AP_clean.csv')  |>
  mutate(datecode=Collection_Date,
         Collection_Date = as.Date(as.character(Collection_Date), format='%Y%m%d')) |>
  group_by(Reservoir_ID,Site_ID,Collection_Date) |>
  mutate(across(c(Temp_C:chl_a_ugL), mean, na.rm=TRUE)) |>
  ungroup() |>
  select(-DUP) |>
  distinct()

zoops <- read.csv('Data/clean_data/2122_Zoops_AP_clean.csv') |>
  mutate(Collection_Date=as.Date(Collection_Date, format='%m/%d/%Y'),
         datecode=format(Collection_Date, '%Y%m%d')) |>
  select(-c(X,Site_ID_pseudonym,Sample_ID,FTG_Combined,Trophic_Group_General)) |>
  group_by(Reservoir_ID, Reservoir_Name,Site_ID,Collection_Date,Species_Name, Functional_Group,Trophic_Group,Feeding_Type) |>
  mutate(across(c(biomass_ugL, Count, Size), mean, na.rm=TRUE)) |>
  ungroup() |>
  select(-DUP) |>
  distinct()

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
  distinct()


sites <- read.csv('Data/clean_data/site_metadata.csv') |>
  rename(Reservoir_Name=Reservoir_FullName,
         Reservoir_ID=Reservoir) |>
  select(-c(Sample_Year, Combined_Res_Site_ID, Location))




HASWQ <- bind_rows(wq21,wq22) |> select(-Reservoir_Name) |>
  mutate(unique_site = paste(Reservoir_ID, Site_ID, datecode, sep='_')) |>
  mutate(
    unique_site = gsub(" ", "", unique_site)) |>
  select(unique_site) |>
  mutate(WQ='YES') |>
  distinct()

table(HASWQ$unique_site) > 1



HASCHL <- bind_rows(wq21,wq22) |> select(-Reservoir_Name) |>
  mutate(unique_site = paste(Reservoir_ID, Site_ID, datecode, sep='_'))|>
  mutate(
    unique_site = gsub(" ", "", unique_site)) |>
  filter(!is.na(chl_a_ugL)) |>
  select(unique_site) |>
  mutate(CHLA='YES') |>
  distinct()

table(HASCHL$unique_site) > 1


HASSI <- sif |>
  mutate(unique_site = paste(Reservoir_ID, Site_ID, datecode, sep='_')) |>
  select(unique_site) |>
  mutate(
    unique_site = gsub(" ", "", unique_site)) |>
  mutate(SI='YES') |>
  distinct()

table(HASSI$unique_site) > 1


HASZOOP <- zoops |>
  mutate(unique_site = paste(Reservoir_ID, Site_ID, datecode, sep='_')) |>
  select(unique_site) |>
  mutate(
    unique_site = gsub(" ", "", unique_site)) |>
  mutate(ZOOP='YES') |>
  distinct() 

table(HASZOOP$unique_site) > 1 



ALL_CURRENT_DATA <- full_join(HASWQ,HASCHL) |>
  full_join(HASSI) |>
  full_join(HASZOOP) |>
  distinct() |>
  mutate(
    COMPLETE = if_else(if_all(WQ:ZOOP, ~ !is.na(.)), "YES", "no")
  ) |>
  mutate(Needs_CHLA = ifelse(is.na(CHLA),"YES","no")) |>
  mutate(Needs_SIA = ifelse(is.na(SI), "YES","no")) |>
  mutate(Needs_WQ = ifelse(is.na(WQ), "YES", "no")) |>
  mutate(Needs_ZOOP = ifelse(is.na(ZOOP), "YES", "no"))

table(ALL_CURRENT_DATA$unique_site) > 1


# notes <- readxl::read_xlsx('Archive/DataArchive/FULL_DATA_REPORT_withnotes.xlsx', 'Sheet2') |>
#   select(-c(Reservior,ReservoirCode,Site,Year,Month,Day,)) |>
#   separate_wider_delim(
#   cols = Sample_ID,
#   delim = "_",
#   names = c("Reservoir_ID", "Site_ID", "deletedate", "Extra_Info"),
#   too_few = "align_start",
#   too_many = "merge"   # merge everything after the 3rd underscore
# ) |>
#   mutate(datecode=format(Collection_Date, '%Y%m%d'),
#          Collection_Date = as.Date(Collection_Date)
#   ) |>
#   mutate(unique_site = paste(Reservoir_ID, Site_ID, datecode, sep='_')) |>
#   mutate(
#     unique_site = gsub(" ", "", unique_site)) |>
#   select(unique_site, `Linnea/Nicole Comments`, `GET WQ`) |>
#   full_join(ALL_CURRENT_DATA)


notes <- readxl::read_xlsx('Data/New_Data.Report_notes.xlsx', '20250813') |>
  select(unique_site, Reservoir_ID, Site_ID, datecode, `Linnea/Nicole Comments - incl. new comments`, `FIND SAMPLE`, `GET WQ`) |>
  full_join(ALL_CURRENT_DATA) |>
  filter(`Linnea/Nicole Comments - incl. new comments` != 'duplicate')

table(notes$unique_site) > 1 

write.csv(notes,'Data/New_Data.Report.csv')
