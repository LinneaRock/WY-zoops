# initial viewing of data ####

library(tidyverse)


sif <- read.csv('Data/SIF_data_20250626.csv')


# find data matches ####

zoops <- read.csv('Data/zoops.csv') |>
  mutate(Sample_ID = if_else(
    str_detect(Sample_ID, "^BOY_"),
    str_replace(Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
    Sample_ID
  ))
wq <- read.csv('Data/water_quality.csv') |> distinct() |>
  mutate(Sample_ID = if_else(
    str_detect(Sample_ID, "^BOY_"),
    str_replace(Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
    Sample_ID
  ))

# Only needed to do this commented below code once.
# res_ids <- read.csv('Data/raw_data/res_ids.csv')
# 
# zoops1 <- zoops |>
#   left_join(res_ids, by='Reservoir_ID') |>
#   relocate(Reservoir_Name, .after = Reservoir_ID)
# write.csv(zoops1, 'Data/zoops.csv')
# 
# wq1 <- wq |>
#   left_join(res_ids, by='Reservoir_ID') |>
#   relocate(Reservoir_Name, .after = Reservoir_ID)
# write.csv(wq1, 'Data/water_quality.csv')


wq_matched <- sif |> left_join(wq) |> filter(!is.na(d15N)) 

table((wq_matched |> drop_na())$Reservoir_Name) # there's actually only 44 SIF measures that have complete WQ data 
# Alcova      Boysen      Glendo     Granite   Grayrocks    Saratoga Wheatland 3 # 2           5           3           2          11           8          13 




zoop_matched <- sif |> left_join(zoops) |> filter(!is.na(d15N)) |> distinct() |>
  drop_na(Count)

table((zoop_matched |> select(Sample_ID, Reservoir_Name) |> distinct())$Reservoir_Name)
#   Alcova          Alsop         Boysen     Fontanelle         Glendo 
# 2              3              5              4              7 
# Granite      Grayrocks New Forks Lake       Saratoga  Viva Naughton 
# 5             14             11             12              3 
# Wheatland 3 
# 20 

count(zoop_matched |> distinct(Sample_ID)) # 86 sample ids matched


# There are 3 red flagged SIF samples -- 2 have complete associated zoop community data and water quality data (GLN_P2_20210809 & GLN_P3_20210809)

# The third red flag (GRY_P1_20210902) has missing secchi, chla, but does have zoop data 

# COMPLETE DATA ####
# == meaning zoop counts and full WQ data

complete_data <- left_join(zoops, wq) |>
  drop_na(Count)

samples_with_zoop_counts <- complete_data |> select(Site_ID_pseudonym, Reservoir_Name, Sample_ID) |> distinct() # 160 individual samples


samples_with_zoop_WQ_complete <- complete_data |> 
  drop_na(c(16:23)) |> select(Site_ID_pseudonym, Reservoir_Name, Sample_ID) |> distinct() # 81 individual samples


samples_with_zoop_WQ_incomplete <- complete_data |> 
  drop_na(Temp_C)

table((samples_with_zoop_WQ_incomplete |> filter(is.na(chl_a_ugL)))$Sample_ID)

# ALC_P1_20210824 ALC_P3_20210614 ALC_P3_20210720 ALC_P3_20210824 FON_P1_20210630 
# 9              12              10              11              13 
# FON_P1_20211001 FON_P2_20210630 FON_P2_20211001 FON_P3_20210630 FON_P3_20211001 
# 13              26              14              13              11 
# GLN_P3_20210609 GLN_P3_20210712 GLN_P3_20210809 GRN_P1_20220701 GRN_P2_20220701 
# 10              11               6               9              11 
# GRN_P3_20220701 GRY_P1_20210902 GRY_P2_20210902 GRY_P3_20210604 GRY_P3_20210714 
# 10               5              11               9              13 
# GRY_P3_20210902 GUE_P3_20210611 OCE_P2_20210810 OCE_P3_20210616 OCE_P3_20210727 
# 12              10              11               8               8 
# PIL_P3_20210617 SAR_P1_20220912 SAR_P2_20220912 SAR_P3_20220912 VIV_P1_20210629 
# 10               5               7               6              10 
# VIV_P2_20210629 VIV_P3_20210629 WHE_P1_20210903 WHE_P2_20210903 WHE_P3_20210707 
# 10              10               9               9              11 
# WHE_P3_20210903 
# 9 


table((samples_with_zoop_WQ_incomplete |> filter(is.na(Temp_C)))$Sample_ID) # none
table((samples_with_zoop_WQ_incomplete |> filter(is.na(pH)))$Sample_ID)
# WHE_P1_20210602 WHE_P2_20210602 


table((samples_with_zoop_WQ_incomplete |> filter(is.na(SPC_uScm)))$Sample_ID)
# GLN_P3_20210609 GLN_P3_20210712 


table((samples_with_zoop_WQ_incomplete |> filter(is.na(DO_mgL)))$Sample_ID) # none
table((samples_with_zoop_WQ_incomplete |> filter(is.na(Secchi_m)))$Sample_ID)

# ALC_P1_20210824 ALC_P3_20210824 FON_P1_20211001 FON_P2_20211001 FON_P3_20211001 
# GRY_P1_20210902 GRY_P2_20210902 GRY_P3_20210902 VIV_P1_20210629 WHE_P1_20210903 
# WHE_P2_20210903 WHE_P3_20210903 

table((samples_with_zoop_WQ_incomplete |> filter(is.na(WaterDepth_m)))$Sample_ID)

# ALS_P1_20220621 ALS_P2_20220621 ALS_P3_20220621 GRN_P1_20220725 GRN_P2_20220725 
# 12              10               9               9              11 
# GRN_P3_20220725 SAR_P3_20220621 WHE_P1_20220726 WHE_P2_20220726 WHE_P3_20220726 
# 10               8               8               8               9 


check_complete <- left_join(samples_with_zoop_WQ_complete, sif) |> drop_na(d15N)
# there are a total of 39 sample ids with complete data records

sifNeeds_complete <- left_join(samples_with_zoop_WQ_complete, sif) |> filter(is.na(d15N))
# there are a total of 42 complete records that we should prioritise for SIF analyses