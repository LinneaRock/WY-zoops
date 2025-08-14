# Clean 2022 raw YSI data

library(tidyverse)


raw22wq <- read.csv('Data/raw_data/2022_wq_data(YSI_Data).csv') |>
  rename(Collection_Date=Date,
         DO_percent=DO_.,
         Depth_m=YSI_Depth_m,
         rawsiteid=Site_ID) |>
  mutate(Reservoir_ID=sub("_.*", "", rawsiteid)) |>
  mutate(Site_ID=word(rawsiteid,2,sep='_')) |>
  #mutate(Site_ID=sub(".*_","",rawsiteid)) |>
  filter(Depth_m < 1.5) |>

#table(raw22wq$Collection_Date)
  filter(Depth_m==0.5) |>
  select(Reservoir_ID,Site_ID,Collection_Date,Depth_m,Temp_C,pH,SPC_uScm,DO_mgL,DO_percent,Secchi_m,WaterDepth_m) |>
  mutate(
    Site_ID = gsub(" ", "", Site_ID))


# add chla data

chla2022 <- readxl::read_xlsx('Data/raw_data/Summer_2022_chl_a_import.xlsx') |>
  rename(Collection_Date=Sample_Date) |>
  select(Reservoir_ID,Site_ID,Collection_Date, chl_a_ugL) 


wq2022 <- left_join(raw22wq,chla2022) |> distinct()




write.csv(wq2022, 'Data/clean_data/2022_WQ_AP_clean.csv') 
