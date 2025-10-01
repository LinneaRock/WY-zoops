#~~~~~~~~~~~~~#
# Basic stats #
#~~~~~~~~~~~~~#

source('Code/01_ReadData.R')

library(khroma)
plot_scheme(color('muted')(9), colours = TRUE, names = TRUE, size = 0.9)
# lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(lakes_sf$gnis_name))
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

# stable isotope plots ####

sif1<-sif |>
  full_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name,gnis_name)) |>
  mutate(Res_Zone=factor(Res_Zone, levels=c('Riverine', 'Transitional', 'Lacustrine'))) |>
  mutate(gnis_name=factor(gnis_name))

lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(sif1$gnis_name))


ggplot() +
  geom_violin(sif1 |> drop_na(gnis_name), mapping=aes(gnis_name, d15N, fill=gnis_name)) +
  geom_hline(yintercept = 7.6102951, color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y=~delta^15*N) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/StableIsotopes/violin_d15N.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif1 |> drop_na(gnis_name), mapping=aes(gnis_name, d13C, fill=gnis_name)) +
  scale_fill_manual('',values=lake_pal) +
  geom_hline(yintercept = -28.5700794, color='grey50') +
  theme_bw() +
  labs(x='',y=~delta^13*C) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/StableIsotopes/violin_d13C.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif1 |> drop_na(gnis_name), mapping=aes(gnis_name, d34S, fill=gnis_name)) +
  scale_fill_manual('',values=lake_pal) +
  geom_hline(yintercept = -2.2362226, color='grey50') +
  theme_bw() +
  labs(x='',y=~delta^34*S) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none')  
ggsave('Figures/Exploration/StableIsotopes/violin_d34S.png',height=4.5,width=6.5,dpi=1200)





ggplot() +
  geom_violin(sif |> drop_na(Res_Zone), mapping=aes(Res_Zone, d15N, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y=~delta^15*N) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/StableIsotopes/violin_d15N.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, d13C, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y=~delta^13*C) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/StableIsotopes/violin_d13C_zone.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(sif1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, d34S, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y=~delta^34*S) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/StableIsotopes/violin_d34S_zone.png',height=4.5,width=6.5,dpi=1200)



# format for plotting
SI_stats_fplot <- SI_stats_lake |>
  filter(!parameter %in% c('percent_C', 'percent_N', 'percent_S')) |>
  select(-c(n_obs, min, max)) |>
  pivot_wider(id_cols = Reservoir_Name,
              names_from = parameter,
              values_from = c(mean, median, sd),
              names_glue = '{parameter}_{.value}') |>
  full_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name, gnis_name))

lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(SI_stats_fplot$gnis_name))

ggplot() +
  geom_point(sif1 |> drop_na(gnis_name), mapping=aes(d34S, d15N, color=gnis_name),alpha=0.5) +
  geom_errorbar(SI_stats_fplot, mapping=aes(y=d15N_mean, xmin=d34S_mean-d34S_sd, xmax=d34S_mean+d34S_sd), color='grey80', width=0) +
  geom_errorbar(SI_stats_fplot, mapping=aes(x=d34S_mean, ymin=d15N_mean-d15N_sd, ymax=d15N_mean+d15N_sd), color='grey80', width=0) +
  geom_point(SI_stats_fplot|> drop_na(gnis_name), mapping=aes(d34S_mean, d15N_mean, fill=gnis_name), size=2,shape=21) +
  scale_color_manual('',values=lake_pal) +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x=~delta^34*S, y=~delta^15*N)
ggsave('Figures/Exploration/sulfur_nitrogen.png',height=4.5,width=6.5,units='in',dpi=1200)



ggplot() +
  geom_point(sif1 |> drop_na(gnis_name), mapping=aes(d13C, d15N, color=gnis_name),alpha=0.5) +
  geom_errorbar(SI_stats_fplot, mapping=aes(y=d15N_mean, xmin=d13C_mean-d13C_sd, xmax=d13C_mean+d13C_sd), color='grey80', width=0) +
  geom_errorbar(SI_stats_fplot, mapping=aes(x=d13C_mean, ymin=d15N_mean-d15N_sd, ymax=d15N_mean+d15N_sd), color='grey80', width=0) +
  geom_point(SI_stats_fplot|> drop_na(gnis_name), mapping=aes(d13C_mean, d15N_mean, fill=gnis_name), size=2,shape=21) +
  scale_color_manual('',values=lake_pal) +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x=~delta^13*C, y=~delta^15*N)
ggsave('Figures/Exploration/carbon_nitrogen.png',height=4.5,width=6.5,units='in',dpi=1200)


# water quality plots ####

wq1<-water_quality |>
  full_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name,gnis_name)) |>
  mutate(Res_Zone=factor(Res_Zone, levels=c('Riverine', 'Transitional', 'Lacustrine'))) |>
  mutate(gnis_name=factor(gnis_name))

lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(wq1$gnis_name))


ggplot() +
  geom_violin(wq1 |> drop_na(pH), mapping=aes(gnis_name, pH, fill=gnis_name)) +
  geom_hline(yintercept = mean(wq1$pH, na.rm=TRUE), color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y='pH') +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_pH.png',height=4.5,width=6.5,dpi=1200)

ggplot() +
  geom_violin(wq1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, pH, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y='pH') +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_pH_zone.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(wq1 |> drop_na(Temp_C), mapping=aes(gnis_name, Temp_C, fill=gnis_name)) +
  geom_hline(yintercept = mean(wq1$Temp_C, na.rm=TRUE), color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y='Temperature'~(degree~C)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_Temp_C.png',height=4.5,width=6.5,dpi=1200)

ggplot() +
  geom_violin(wq1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, Temp_C, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y='Temperature'~(degree~C)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_Temp_C_zone.png',height=4.5,width=6.5,dpi=1200)


ggplot() +
  geom_violin(wq1 |> drop_na(SPC_uScm), mapping=aes(gnis_name, SPC_uScm, fill=gnis_name)) +
  geom_hline(yintercept = mean(wq1$SPC_uScm, na.rm=TRUE), color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y='Specific Conductivity'~(mu*S~cm^-1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_SPC_uScm.png',height=4.5,width=6.5,dpi=1200)

ggplot() +
  geom_violin(wq1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, SPC_uScm, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y='Specific Conductivity'~(mu*S~cm^-1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_SPC_uScm_zone.png',height=4.5,width=6.5,dpi=1200)



ggplot() +
  geom_violin(wq1 |> drop_na(DO_mgL), mapping=aes(gnis_name, DO_mgL, fill=gnis_name)) +
  geom_hline(yintercept = mean(wq1$DO_mgL, na.rm=TRUE), color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y='Dissolved Oxygen'~(mg~L^-1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_DO_mgL.png',height=4.5,width=6.5,dpi=1200)

ggplot() +
  geom_violin(wq1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, DO_mgL, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y='Dissolved Oxygen'~(mg~L^-1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_DO_mgL_zone.png',height=4.5,width=6.5,dpi=1200)




ggplot() +
  geom_violin(wq1 |> drop_na(Secchi_m), mapping=aes(gnis_name, Secchi_m, fill=gnis_name)) +
  geom_hline(yintercept = mean(wq1$Secchi_m, na.rm=TRUE), color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y='Secchi depth'~(m)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_Secchi_m.png',height=4.5,width=6.5,dpi=1200)

ggplot() +
  geom_violin(wq1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, Secchi_m, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y='Secchi depth'~(m)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_Secchi_m_zone.png',height=4.5,width=6.5,dpi=1200)





ggplot() +
  geom_violin(wq1 |> drop_na(chl_a_ugL), mapping=aes(gnis_name, chl_a_ugL, fill=gnis_name)) +
  geom_hline(yintercept = mean(wq1$chl_a_ugL, na.rm=TRUE), color='grey50') +
  scale_fill_manual('',values=lake_pal) +
  theme_bw() +
  labs(x='',y='Chlorophyll'~italic(a)~(mu*g~L^-1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_chl_a_ugL.png',height=4.5,width=6.5,dpi=1200)

ggplot() +
  geom_violin(wq1 |> drop_na(Res_Zone), mapping=aes(Res_Zone, chl_a_ugL, fill=Res_Zone)) +
  scale_fill_manual('',values=zone_pal) +
  theme_bw() +
  labs(x='',y='Chlorophyll'~italic(a)~(mu*g~L^-1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = 'none') 
ggsave('Figures/Exploration/WaterQuality/violin_chl_a_ugL_zone.png',height=4.5,width=6.5,dpi=1200)
