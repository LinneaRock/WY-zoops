#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Relationships between stable 
# isotopes & in-lake variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Code/01_ReadData.R')

library(khroma)
# lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(abundances$gnis_name))

# water quality ####
si_wq <- left_join(sif |> mutate(datecode=as.numeric(datecode)), water_quality |> mutate(datecode=as.numeric(datecode))) |>
  full_join(lakes_sf |> select(gnis_name, Reservoir_Name)) |>
  pivot_longer(c(d15N:d34S), names_to = 'si', values_to = 'value') |>
  mutate(si=factor(si, levels=c('d15N', 'd13C', 'd34S'), labels=c(expression(delta^13*C),expression(delta^15*N),expression(delta^34*S)))) |>
  drop_na(gnis_name)


lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(si_wq$gnis_name)) 


ggplot(si_wq, aes(Temp_C, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Temperature'~(degree*C))
ggsave('Figures/Exploration/StableIsotopes/SI_Temp_C.png',height=6.5,width=6.5,dpi=1200)


ggplot(si_wq, aes(pH, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='pH')
ggsave('Figures/Exploration/StableIsotopes/SI_pH.png',height=6.5,width=6.5,dpi=1200)


ggplot(si_wq, aes(SPC_uScm, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Specific Conductivity'~(mu*S~cm^-1))
ggsave('Figures/Exploration/StableIsotopes/SI_SPC_uScm.png',height=6.5,width=6.5,dpi=1200)



ggplot(si_wq, aes(DO_mgL, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Dissolved Oxygen'~(mg~L^-1))
ggsave('Figures/Exploration/StableIsotopes/SI_DO_mgL.png',height=6.5,width=6.5,dpi=1200)



ggplot(si_wq, aes(Secchi_m, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Secchi depth'~(m))
ggsave('Figures/Exploration/StableIsotopes/SI_Secchi_m.png',height=6.5,width=6.5,dpi=1200)



ggplot(si_wq, aes(WaterDepth_m, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Maximum depth'~(m))
ggsave('Figures/Exploration/StableIsotopes/SI_WaterDepth_m.png',height=6.5,width=6.5,dpi=1200)



ggplot(si_wq, aes(chl_a_ugL, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Chlorophyll'~italic(a)~(mu*g~L^-1))
ggsave('Figures/Exploration/StableIsotopes/SI_chl_a_ugL.png',height=6.5,width=6.5,dpi=1200)



# location ####
ggplot(si_wq, aes(Latitude, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Latitude')
ggsave('Figures/Exploration/StableIsotopes/SI_Latitude.png',height=6.5,width=6.5,dpi=1200)


ggplot(si_wq, aes(Longitude, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='', x='Longitude')
ggsave('Figures/Exploration/StableIsotopes/SI_Longitude.png',height=6.5,width=6.5,dpi=1200)




# zoop communities ####
# NEED to run 3_AlphaBeta_Diversity_Zoops.R for plots using NMDS scores
nmds_si <- scores |>
  select(Group, gnis_name, NMDS1, NMDS2, d15N:d34S) |>
  pivot_longer(c(d15N:d34S), names_to = 'si', values_to = 'value') |>
  mutate(si=factor(si, levels=c('d15N', 'd13C', 'd34S'), labels=c(expression(delta^13*C),expression(delta^15*N),expression(delta^34*S))))


lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(nmds_si$gnis_name))


ggplot(nmds_si, aes(NMDS1, value )) +
  geom_point(aes(color=gnis_name)) +
  #geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual('', values = lake_pal) +
  facet_wrap(~si, nrow = 3, scales = 'free',labeller=as_labeller(label_parsed)) +
  labs(y='')
ggsave('Figures/Exploration/StableIsotopes/SI_NMDS.png',height=6.5,width=6.5,dpi=1200)







