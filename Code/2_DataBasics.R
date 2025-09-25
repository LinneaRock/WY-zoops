#~~~~~~~~~~~~~#
# Basic stats #
#~~~~~~~~~~~~~#

source('Code/01_ReadData.R')

library(khroma)
plot_scheme(color('muted')(9), colours = TRUE, names = TRUE, size = 0.9)
lake_pal <- c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD')
zone_pal <- c('#FEDA8B','#EAECCC','#C2E4EF')

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



sif<-sif |>
  left_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name,gnis_name)) |>
  mutate(Res_Zone=factor(Res_Zone, levels=c('Riverine', 'Transitional', 'Lacustrine')))


ggplot() +
  geom_violin(sif |> drop_na(gnis_name), mapping=aes(gnis_name, d15N, fill=gnis_name)) +
  geom_hline(yintercept = 7.6102951, color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y=~delta^15~N) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
ggsave('Figures/Exploration/violin_d15N.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif |> drop_na(gnis_name), mapping=aes(gnis_name, d13C, fill=gnis_name)) +
  scale_fill_manual('',values=lake_pal) +
  geom_hline(yintercept = -28.5700794, color='grey50') +
  theme_bw() +
  labs(x='',y=~delta^13~C) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
ggsave('Figures/Exploration/violin_d13C.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif |> drop_na(gnis_name), mapping=aes(gnis_name, d34S, fill=gnis_name)) +
  scale_fill_manual('',values=lake_pal) +
  geom_hline(yintercept = -2.2362226, color='grey50') +
  theme_bw() +
  labs(x='',y=~delta^34~S) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
ggsave('Figures/Exploration/violin_d34S.png',height=4.5,width=6.5,dpi=1200)





ggplot() +
  geom_violin(sif |> drop_na(gnis_name), mapping=aes(Res_Zone, d15N, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y=~delta^15~N) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
ggsave('Figures/Exploration/violin_d15N.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif |> drop_na(gnis_name), mapping=aes(Res_Zone, d13C, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y=~delta^13~C) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
ggsave('Figures/Exploration/violin_d13C_zone.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif |> drop_na(gnis_name), mapping=aes(Res_Zone, d34S, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y=~delta^34~S) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave('Figures/Exploration/violin_d34S_zone.png',height=4.5,width=6.5,dpi=1200)

