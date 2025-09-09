#~~~~~~~~~~~~~#
# Basic stats #
#~~~~~~~~~~~~~#

source('Code/01_ReadData.R')

# water quality ####
wq_stats_site <- water_quality |>
  pivot_longer(c(Temp_C:WaterDepth_m, chl_a_ugL), names_to = 'parameter', values_to = 'value') |>
  group_by(Site_Name,parameter) |>
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE),
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            n_obs = n()) |>
  ungroup()


wq_stats_lake <- water_quality |>
  pivot_longer(c(Temp_C:WaterDepth_m, chl_a_ugL), names_to = 'parameter', values_to = 'value') |>
  group_by(Reservoir_Name,parameter) |>
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE), 
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            n_obs = n()) |>
  ungroup()

wq_stats_state <- water_quality |>
  pivot_longer(c(Temp_C:WaterDepth_m, chl_a_ugL), names_to = 'parameter', values_to = 'value') |>
  group_by(parameter) |>
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE),
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            n_obs = n()) |>
  ungroup()


# stable isotopes ####
SI_stats_site <- sif |>
  pivot_longer(c(percent_N:d34S), names_to = 'parameter', values_to = 'value') |>
  group_by(Site_Name,parameter) |>
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE),
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            n_obs = n()) |>
  ungroup()


SI_stats_lake <- sif |>
  pivot_longer(c(percent_N:d34S), names_to = 'parameter', values_to = 'value') |>
  group_by(Reservoir_Name,parameter) |>
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE),
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            n_obs = n()) |>
  ungroup()

SI_stats_state <- sif |>
  pivot_longer(c(percent_N:d34S), names_to = 'parameter', values_to = 'value') |>
  group_by(parameter) |>
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE),
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            n_obs = n()) |>
  ungroup()

