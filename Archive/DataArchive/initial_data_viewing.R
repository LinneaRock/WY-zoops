# initial viewing of data ####

library(tidyverse)


sif <- read.csv('Data/SIF_data_20250626.csv')


# find data matches ####

zoops <- read.csv('Data/zoops.csv') |>
  mutate(Sample_ID = if_else(
    str_detect(Sample_ID, "^BOY_"),
    str_replace(Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
    Sample_ID
  )) |> select(-X)
wq <- read.csv('Data/water_quality.csv') |> distinct() |>
  mutate(Sample_ID = if_else(
    str_detect(Sample_ID, "^BOY_"),
    str_replace(Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
    Sample_ID
  )) |> select(-X)

# Only needed to do this commented below code once.
res_ids <- read.csv('Data/raw_data/res_ids.csv')
# 
#orig |> mutate(Sample_ID = if_else(
# str_detect(Sample_ID, "^BOY_"),
# str_replace(Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
# Sample_ID
# ))
#
# zoops1 <- zoops |>
#   left_join(res_ids, by='Reservoir_ID') |>
#   relocate(Reservoir_Name, .after = Reservoir_ID)
# zoops2 <- zoops1 |> left_join(orig |> mutate(Sample_ID = if_else(
#   str_detect(Full_Sample_ID, "^BOY_"),
#   str_replace(Full_Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
#   Full_Sample_ID
# )) |> select(Sample_ID, Site_ID)) |> relocate(Site_ID, .after=Sample_ID) |> distinct()
# write.csv(zoops2, 'Data/zoops.csv')
# 
# wq1 <- wq |>
#   left_join(res_ids, by='Reservoir_ID') |>
#   relocate(Reservoir_Name, .after = Reservoir_ID)
# wq2 <- wq1 |> left_join(orig |> mutate(Sample_ID = if_else(
#   str_detect(Full_Sample_ID, "^BOY_"),
#   str_replace(Full_Sample_ID, "^([A-Z]+_[A-Z]+)_[^_]+_(\\d{8})$", "\\1_\\2"),
#   Full_Sample_ID
# )) |> select(Sample_ID, Site_ID)) |> relocate(Site_ID, .after=Sample_ID) |> distinct()
# write.csv(wq2, 'Data/water_quality.csv')



# 
# wq_matched <- sif |> left_join(wq) |> filter(!is.na(d15N)) 
# 
# table((wq_matched |> drop_na())$Reservoir_Name) # there's actually only 44 SIF measures that have complete WQ data 
# # Alcova      Boysen      Glendo     Granite   Grayrocks    Saratoga Wheatland 3 
# # 2           5           3           2          11           8          13 
# 
# 
# 
# 
# zoop_matched <- sif |> left_join(zoops) |> filter(!is.na(d15N)) |> distinct() |>
#   drop_na(Count)
# 
# table((zoop_matched |> select(Sample_ID, Reservoir_Name) |> distinct())$Reservoir_Name)
# #   Alcova          Alsop         Boysen     Fontanelle         Glendo 
# # 2              3              5              4              7 
# # Granite      Grayrocks New Forks Lake       Saratoga  Viva Naughton 
# # 5             14             11             12              3 
# # Wheatland 3 
# # 20 
# 
# count(zoop_matched |> distinct(Sample_ID)) # 86 sample ids matched
# 
# 
# # There are 3 red flagged SIF samples -- 2 have complete associated zoop community data and water quality data (GLN_P2_20210809 & GLN_P3_20210809)
# 
# # The third red flag (GRY_P1_20210902) has missing secchi, chla, but does have zoop data 
# 
# 
# 
# # COMPLETE DATA ####
# # == meaning zoop counts and full WQ data
# 
# complete_data <- left_join(zoops, wq) |>
#   drop_na(Count)
# 
# samples_with_zoop_counts <- complete_data |> select(Site_ID_pseudonym, Reservoir_Name, Sample_ID) |> distinct() # 160 individual samples
# 
# 
# samples_with_zoop_WQ_complete <- complete_data |> 
#   drop_na(c(16:23)) |> select(Site_ID_pseudonym, Reservoir_Name, Sample_ID) |> distinct() # 91 individual samples
# 
# 
# samples_with_zoop_WQ_incomplete <- complete_data |> 
#   drop_na(Temp_C)
# 
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(chl_a_ugL)))$Sample_ID)
# 
# # ALC_P1_20210824 ALC_P3_20210614 ALC_P3_20210720 ALC_P3_20210824 FON_P1_20210630 
# # 9              12              10              11              13 
# # FON_P1_20211001 FON_P2_20210630 FON_P2_20211001 FON_P3_20210630 FON_P3_20211001 
# # 13              26              14              13              11 
# # GLN_P3_20210609 GLN_P3_20210712 GLN_P3_20210809 GRN_P1_20220701 GRN_P2_20220701 
# # 10              11               6               9              11 
# # GRN_P3_20220701 GRY_P1_20210902 GRY_P2_20210902 GRY_P3_20210604 GRY_P3_20210714 
# # 10               5              11               9              13 
# # GRY_P3_20210902 GUE_P3_20210611 OCE_P2_20210810 OCE_P3_20210616 OCE_P3_20210727 
# # 12              10              11               8               8 
# # PIL_P3_20210617 SAR_P1_20220912 SAR_P2_20220912 SAR_P3_20220912 VIV_P1_20210629 
# # 10               5               7               6              10 
# # VIV_P2_20210629 VIV_P3_20210629 WHE_P1_20210903 WHE_P2_20210903 WHE_P3_20210707 
# # 10              10               9               9              11 
# # WHE_P3_20210903 
# # 9 
# 
# 
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(Temp_C)))$Sample_ID) # none
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(pH)))$Sample_ID)
# # WHE_P1_20210602 WHE_P2_20210602 
# 
# 
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(SPC_uScm)))$Sample_ID)
# # GLN_P3_20210609 GLN_P3_20210712 
# 
# 
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(DO_mgL)))$Sample_ID) # none
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(Secchi_m)))$Sample_ID)
# 
# # ALC_P1_20210824 ALC_P3_20210824 FON_P1_20211001 FON_P2_20211001 FON_P3_20211001 
# # GRY_P1_20210902 GRY_P2_20210902 GRY_P3_20210902 VIV_P1_20210629 WHE_P1_20210903 
# # WHE_P2_20210903 WHE_P3_20210903 
# 
# table((samples_with_zoop_WQ_incomplete |> filter(is.na(WaterDepth_m)))$Sample_ID)
# 
# # ALS_P1_20220621 ALS_P2_20220621 ALS_P3_20220621 GRN_P1_20220725 GRN_P2_20220725 
# # 12              10               9               9              11 
# # GRN_P3_20220725 SAR_P3_20220621 WHE_P1_20220726 WHE_P2_20220726 WHE_P3_20220726 
# # 10               8               8               8               9 
# 
# 
# check_complete <- left_join(samples_with_zoop_WQ_complete, sif) |> drop_na(d15N) |> distinct()
# # there are a total of 54 sample ids with complete data records, one red flag
# 
# sifNeeds_complete <- left_join(samples_with_zoop_WQ_complete, sif) |> filter(is.na(d15N))
# # there are a total of 37 complete records that we should prioritise for SIF analyses
# 



samplinglist<-readxl::read_xlsx('Data/raw_data/Zooplankton_SIA_WYReservoirs.xlsx') |>
  mutate(date=as.Date(paste(Year, Month, Day, sep='-'), format='%Y-%m-%d'))

zoopsidslist<-zoops |> filter(!is.na(Count)) |>
  select(Site_ID_pseudonym, Reservoir_Name, Sample_ID,Site_ID, Collection_Date) |>
  distinct()|>
  mutate(date=as.Date(Collection_Date, format='%m/%d/%Y')) |>
  select(-Collection_Date) |>
  mutate(hasZoopdata = 'Yes')



wqidslist <- wq |> 
  mutate(date=as.Date(Collection_Date, format='%m/%d/%Y'))

# Define the identifying columns and the data columns to check
id_cols <- c("Site_ID_pseudonym", "Reservoir_ID", "Reservoir_Name", "Sample_ID", "Site_ID", "date")
data_cols <- c("chl_a_ugL", "Temp_C", "pH", "SPC_uScm", "DO_mgL", "DO_percent", "Secchi_m", "WaterDepth_m")


wq_report_complete <- wqidslist |>
  mutate(row_id = row_number()) |>
  pivot_longer(cols = all_of(data_cols), names_to = "Variable", values_to = "Value") %>%
  group_by(row_id, across(all_of(id_cols))) |>
  summarise(
    Missing_WQ = paste(Variable[is.na(Value)], collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(Missing_WQ = ifelse(Missing_WQ == "", "Complete WQ Record", Missing_WQ)) |>
  select(-row_id) 
  


sif_report <- sif |>
  #annoying manual corrections because everyone decided to randomly change ids
  mutate(
         Sample_ID=ifelse(Sample_ID=='GRN_P3_20220920_11m_Vertical_Tow','GRN_P3_11m_20220920', Sample_ID),
         Sample_ID=ifelse(Sample_ID=='GRN_P2_20220920_11m_Vertical_Tow','GRN_P2_11m_20220920_20220920', Sample_ID)) |>
  select(Sample_ID,  Notes) |>
  mutate(Notes=ifelse(Notes=='', "Complete SIF Analysis", Notes)) 


FULL_DATA_REPORT <- full_join(wq_report_complete, zoopsidslist) |>
  full_join(sif_report) |>
  mutate(hasZoopdata=ifelse(is.na(hasZoopdata), 'No Zoop data', hasZoopdata)) |>
  mutate(Missing_WQ = ifelse(Missing_WQ=='chl_a_ugL, Temp_C, pH, SPC_uScm, DO_mgL, DO_percent, Secchi_m, WaterDepth_m', 'No WQ data', Missing_WQ)) |>
  mutate(Missing_WQ = ifelse(Missing_WQ=='chl_a_ugL, Temp_C, pH, SPC_uScm, DO_mgL, DO_percent, Secchi_m', 'No WQ data', Missing_WQ)) |>
  mutate(Missing_WQ = ifelse(is.na(Missing_WQ),'No WQ data', Missing_WQ)) |>
  mutate(Notes=ifelse(hasZoopdata=='Yes'&
                        Missing_WQ=='Complete WQ Record' &
                        is.na(Notes), 'Prioritise SIF Analysis', Notes)) |>
  filter(Sample_ID != 'Replicate C9') |>
  rename(SI_Notes = Notes)
write.csv(FULL_DATA_REPORT, 'Data/FULL_DATA_REPORT.csv')

nas <- FULL_DATA_REPORT|>filter(is.na(SI_Notes))
na_zoop <- FULL_DATA_REPORT|>filter(is.na(Site_ID_pseudonym))
check_sif <- FULL_DATA_REPORT|>filter(SI_Notes %in% c('yellow flag', 'red flag', 'Complete SIF Analysis')) |>
  filter(Missing_WQ != 'Complete WQ Record',
         hasZoopdata != 'No Zoop data')



sif_quick_data <- FULL_DATA_REPORT |> left_join(sif) |>
  drop_na(percent_N) |>
  filter(hasZoopdata=='Yes') |>
  filter(SI_Notes != 'red flag') |>
  mutate(Year = year(date)) |>
  mutate(Year = as.factor(Year)) |>
  mutate(correctedC13= (1.158*(d13C+29.132))-30.819) |>
  group_by(Reservoir_Name, Year) |>
  mutate(d15Nmean=mean(d15N),
         d13Cmean=mean(correctedC13), 
         d34Smean=mean(d34S),
         sdN=sd(d15N),
         sdC=sd(correctedC13),
         sdS=sd(d34S))


ggplot(sif_quick_data, aes(d34Smean, d15Nmean)) +
  geom_errorbar(aes(xmin=d34Smean-sdS, xmax=d34Smean+sdS), color='grey80', width=0) +
  geom_errorbar(aes(ymin=d15Nmean-sdN, ymax=d15Nmean+sdN), color='grey80', width=0) +
  geom_point(aes(color=Reservoir_Name, shape=Year),size=2) +
  theme_bw() +
  labs(x=~delta~S34,
       y=~delta~N15)
ggsave('Figures/sulfur_nitrogen.png',height=4.5,width=6.5,units='in',dpi=1200)

ggplot(sif_quick_data, aes(d13Cmean, d15Nmean)) +
  geom_errorbar(aes(xmin=d13Cmean-sdC, xmax=d13Cmean+sdC), color='grey80', width=0) +
  geom_errorbar(aes(ymin=d15Nmean-sdN, ymax=d15Nmean+sdN), color='grey80', width=0) +
  geom_point(aes(color=Reservoir_Name, shape=Year),size=2) +
  theme_bw() +
  labs(x=~delta~C13~(with~ethanol~correction),
       y=~delta~N15)
ggsave('Figures/carbon_nitrogen.png',height=4.5,width=6.5,units='in',dpi=1200)
