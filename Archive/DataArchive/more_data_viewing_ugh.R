
# filling in missing data after email from Ashleigh 07/21/25

library(tidyverse)

needwq <- readxl::read_xlsx('C:/Users/linne/OneDrive/Desktop/WY_Zoops_needWQ.xlsx') |> select(match)

raw22wq <- read.csv('Data/raw_data/2022_wq_data(YSI_Data).csv') |>
  mutate(match=paste(Site_ID, Date, sep='_')) |>
  filter(YSI_Depth_m==0.5)

matchedwq <- left_join(needwq,raw22wq) |> distinct() |>
  # 2 temp measures for SAR_P1_20220831 (21.5 and 21.3 deg C) 
  mutate(Temp_C=ifelse(match=='SAR_P1_20220831', 21.4, Temp_C)) |>
  distinct()

### SERIOUS PROBLEMS WITH LAT/LONGS HERE

raw21wq <- readxl::read_xlsx('Data/raw_data/2021_wq_data_long.xlsx') |>
  mutate(Value=as.numeric(Value)) |>
  mutate(match=paste(SiteID, Date, sep='_')) |>
  # need these samples: 
  # VIV_P3_20210827
  # 
  # VIV_P2_20210827
  # 
  # VIV_P1_20210827  ## These VIV samples do NOT have WQ data :(
  # 
  # OCE_P2_20210616  ## We have this at depth = 0.5
  # 
  # GLN_P2_20210712  ## We have this at depth = 0.5
  filter(match%in%c('OCE_P2_20210616','OCE_P1_20210616','OCE_P3_20210616', 'GLN_P2_20210712')) |>
  pivot_wider(names_from = SampleID, values_from = Value) |>
  group_by(match) |>
  mutate(Secchi_m = (`Secchi disappears (m)` +`Secchi reappears (m)`)/2) |>
  ungroup() |>
  # manual formatting to get the samples we need
  mutate(Depth_m=ifelse(!is.na(Total_Depth_ft), 0.5, Depth_m),
         Depth_m=ifelse(!is.na(Secchi_m), 0.5, Depth_m))



## working on location issues from 2021 data ####
spatial <- readxl::read_xlsx('Data/raw_data/2021_wq_data_long.xlsx') |>
  select(SiteID, Lat, Long, Date) |> 
  #filter(Date%in%c('20210614'))|>
  #filter(SiteID!='VIV_P3') |>
  distinct() |>
  drop_na()
