#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Zooplankton community dynamics #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Code/01_ReadData.R')

library(vegan)

library(khroma)
plot_scheme(color('muted')(9), colours = TRUE, names = TRUE, size = 0.9)
#lake_pal <- c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD')
zone_pal <- c('#FEDA8B','#EAECCC','#C2E4EF')


# prepare dataframes ####
sites_norepeats <- sites |>
  group_by(Reservoir_Name,Reservoir_ID, Res_Zone, Site_Name) |>
  summarise(Latitude=mean(Latitude),
            Longitude=mean(Longitude)) |>
  full_join(lakes_sf |> select(Reservoir_Name,gnis_name)) |>
  ungroup()

metadata <- left_join(water_quality |> mutate(datecode=as.numeric(datecode)),sif|> mutate(datecode=as.numeric(datecode))) |>
  select(-Latitude, -Longitude) |>
  mutate(Group=paste(Site_Name, datecode, sep="_"),
         Year=year(Collection_Date),
         Month=month(Collection_Date),
         JulianDay=yday(Collection_Date)) |>
  full_join(sites_norepeats) |>
  select(Group, Reservoir_Name, Res_Zone, Year, Month, JulianDay, Latitude, Longitude, percent_N:d34S, Temp_C:WaterDepth_m, chl_a_ugL) |>
  distinct()
  # needs to be filtered more when running analyses that require complete entries



# Alpha diversity analysis ####
# answers questions related to how diverse communities are 

# relative abundance at each sampling event (site and date)
zoop_rel_abund <- zoops |>
  filter(!is.na(Reservoir_Name)) |>
  mutate(Group=paste(Site_Name, datecode, sep="_")) |>
  select(Group, Species_Name, biomass_ugL) |>
  drop_na() |>
  group_by(Group) |>
  mutate(rel_abund = biomass_ugL/sum(biomass_ugL, na.rm=TRUE) * 100) |>
  ungroup() |>
  select(-biomass_ugL) |>
  mutate(Site_Name = str_extract(Group, "^[^_]+_[^_]+")) |>
  left_join(sites |> select(Site_Name, Reservoir_Name))

taxon_pool <- zoop_rel_abund |>
  mutate(Site_Name = str_extract(Group, "^[^_]+_[^_]+")) |>
  left_join(sites |> select(Site_Name, Reservoir_Name)) |>
  group_by(Reservoir_Name, Species_Name) |>
  summarise(mean=mean(rel_abund,na.rm=TRUE), groups='drop') |>
  ungroup() |>
  group_by(Species_Name) |>
  summarise(pool = max(mean) < 3,
            mean = mean(mean),
            .groups="drop")

abundances <- inner_join(zoop_rel_abund, taxon_pool) |>
mutate(Species_Name = if_else(pool, "Other", Species_Name)) |>
  group_by(Group, Reservoir_Name, Species_Name) |>
  summarise(rel_abund=sum(rel_abund),
            mean = min(mean),
            .groups="drop") |>
  drop_na(Species_Name) |>
  full_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name,gnis_name))
 
lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(abundances$gnis_name))


#Taxonomy Line Plot - aggregated over all time within each lake
abundances |>
  drop_na()|>
  arrange(desc(mean)) |>
  mutate(Species_Name=factor(Species_Name)) |>
  #filter(mean>5) |> # annoying filter just to make plot less complicated by removing the lowest taxa
  ggplot(aes(rel_abund, reorder(Species_Name, rel_abund), fill=gnis_name, color=gnis_name)) +
  stat_summary(fun.data=median_hilow, geom = "pointrange",
               fun.args=list(conf.int=0.5),
               position = position_dodge(width=0.6), shape=21) +
  theme_classic() +
  labs(y=NULL,
       x="Relative Abundance (%)") +
  theme(legend.position = c(0.8,0.35)) +
  scale_color_manual('',values=lake_pal) +
  scale_fill_manual('',values=lake_pal)
ggsave('Figures/Exploration/Zoops/Rel_abund.png',height=6.5,width=6.5,dpi=1200)

all_spp <- zoops |> filter(!is.na(Reservoir_Name)) |> select(Count, Species_Name) |> drop_na() |> select(Species_Name) |> distinct()


# Beta diversity analysis ####
# answers questions related to how different communities are from each other 

# from running first time - need to manually remove some groups
removal <- c('ALS_Transitional_20220603',
             'ALS_Riverine_20220603'    ,
             'GRN_Riverine_20220701'    ,
             'GRN_Transitional_20220701',
             'GRN_Lacustrine_20220701'  ,
             'NFL_Lacustrine_20210817'  ,
             'NFL_Lacustrine_20210914'  ,
             'NFL_Lacustrine_20211019'  ,
             'NFL_Transitional_20210720',
             'NFL_Transitional_20210817',
             'NFL_Transitional_20210914',
             'NFL_Transitional_20211019',
             'NFL_Riverine_20210720'    ,
             'NFL_Riverine_20210817'    ,
             'NFL_Riverine_20210914'    ,
             'NFL_Riverine_20211019' )

# create distance matrix of zoop communities
dist_zoop <- zoops |>
  filter(Reservoir_ID != 'OCE') |>
  mutate(Group=paste(Site_Name, datecode, sep="_")) |>
  select(Group, Species_Name, biomass_ugL) |>
  distinct() |>
  filter(!Group %in% c(removal)) |>
  drop_na() |>
  pivot_wider(
    names_from = Species_Name,
    values_from = biomass_ugL
  ) |> 
  as.data.frame()
rownames(dist_zoop) <- dist_zoop$Group
dist_zoop <- dist_zoop[,-1]
dist_zoop <- as.matrix(dist_zoop)
dist_zoop <- replace(dist_zoop, is.na(dist_zoop), 0)


#   Distance Matrix:calculates a distance matrix that quantifies the dissimilarity between all pairs of samples based on their multivariate data. In your case, this could be a measure of how different the zooplankton communities are from each other.

dist <- vegdist(dist_zoop, method = 'bray')

dist


# PERMANOVA  with adonis2 can help test relationships between metadata and community composition

#   PERMANOVA is a statistical test used to compare the differences between groups of multivariate data.
# It is useful when you have complex data with many variables and you want to see if the composition of these variables differs significantly between groups.


# Permutations: permutes (randomly rearranges) the data many times to create a distribution of possible outcomes under the null hypothesis (that there are no differences between groups).
# Comparison: It compares the observed differences between groups to this distribution to determine if the observed differences are statistically significant.

#   Unlike traditional ANOVA, PERMANOVA is nonparametric (i.e., it does not assume normal distribution of the data), making it suitable for ecological data, which often do not meet these assumptions.
# Multivariate: It can handle multiple variables at once, making it ideal for complex datasets like those involving multiple species of zoops.

adonis2(dist~Latitude, metadata)
adonis2(dist~Longitude, metadata)


# NMDS - beta dispersion plots 
set.seed(69420)
nmds <- metaMDS(dist)
nmds
# make note of the stress value, this shows how easy it was to condense multidimensional data into two dimensional space, below 0.2 is generally good -- 0.1846036 
# hmmm, not very good?

# pull out scores for beautiful plotting
scores <- scores(nmds) |>
  as_tibble(rownames='Group') |>
  # and join to metadata
  left_join(metadata)  |>
  full_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name,gnis_name)) |>
  mutate(Res_Zone=factor(Res_Zone, levels=c('Riverine', 'Transitional', 'Lacustrine')))

# # go back and remove these from NMDS and re-run
# removal <- scores |>
#   filter(is.na(Reservoir_Name)) |>
#   select(Group)
# # pull these out manually & bring them back up to the top of beta diversity
# removal

lake_pal <- setNames(c('black', 'grey30', color('muted')(9), 'grey50', '#DDDDDD'), levels(scores$gnis_name))


scores |>
  #filter(NMDS1<1000) |>
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(fill=gnis_name),size=2,shape=21) +
  theme_bw() +
  scale_fill_manual('', values=lake_pal)
ggsave('Figures/Exploration/Zoops/NMDS_lakes.png',height=4.5,width=6.5,dpi=1200)

scores |>
  #filter(NMDS1<1000) |>
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(fill=as.factor(Month)),size=2,shape=21) +
  theme_bw() +
  scale_fill_viridis_d('Month') 


scores |>
  drop_na(Res_Zone) |>
  #filter(NMDS1<1000) |>
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(fill=Res_Zone),size=2,shape=21) +
  theme_bw() +
  scale_fill_manual('', values=zone_pal)
ggsave('Figures/Exploration/Zoops/NMDS_zones.png',height=4.5,width=6.5,dpi=1200)

scores |>
  #filter(NMDS1<1000) |>
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(fill=Longitude),size=2,shape=21) +
  theme_bw() +
  scale_fill_viridis_c('Longitude') 
ggsave('Figures/Exploration/Zoops/NMDS_longitude.png',height=4.5,width=6.5,dpi=1200)

scores |>
  #filter(NMDS1<1000) |>
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(fill=Latitude),size=2,shape=21) +
  theme_bw() +
  scale_fill_viridis_c('Latitude') 

## Intrinsic variables ####
# investigate the species which drive the distance distribution 
set.seed(69420)
spp.fit <- envfit(nmds, dist_zoop, permutations=999)
head(spp.fit)



spp.fit_df <- as.data.frame(scores(spp.fit, display='vectors'))  #extracts relevant scores from sppfit
spp.fit_df <- cbind(spp.fit_df, spp.variables = rownames(spp.fit_df)) #and then gives them their names

spp.fit_df <- cbind(spp.fit_df, pval = spp.fit$vectors$pvals) # add pvalues to dataframe
sig.spp.fit <- subset(spp.fit_df, pval<=0.05) #subset data to show significant variables

sig.spp.fit

#Now we have the relevant information for plotting the ordination!
ggplot() +
  geom_point(scores, mapping=aes(x=NMDS1, y=NMDS2), color='grey50') +
  theme_bw() +
  geom_segment(sig.spp.fit, mapping=aes(x=0, xend=NMDS1*2, y=0, yend=NMDS2*2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(sig.spp.fit, mapping=aes(x=NMDS1*2, y=NMDS2*2, label = spp.variables), cex = 4, direction = "both", segment.size = 0.25) + #add labels, use ggrepel::geom_text_repel so that labels do not overlap
  theme(legend.position= 'none')
ggsave('Figures/Exploration/Zoops/NMDS_intrinsic.png',height=4.5,width=6.5,dpi=1200)


# basics: trophic grouping ####
zoops_tg <- zoops |>
  drop_na(biomass_ugL) |>
  mutate(Trophic_Group = ifelse(is.na(Trophic_Group) &
                                  Functional_Group=='Nauplii',
                                '(nauplii)', Trophic_Group)) |>
  left_join(lakes_sf |> as.data.frame() |> select(Reservoir_Name, gnis_name)) |>
  mutate(DOY = lubridate::yday(Collection_Date)) |>
  group_by(gnis_name, DOY, Collection_Date, Trophic_Group) |>
  summarise(tg_biomass = sum(biomass_ugL))

ggplot(zoops_tg |> filter(!is.na(gnis_name),
                          year(Collection_Date)==2021)) +
  geom_bar(aes(DOY, tg_biomass, fill=Trophic_Group),stat='identity', width=7) +
  facet_wrap(~gnis_name, scales='free_y') +
  theme_bw() +
  labs(x='Day of year', y='Zooplankton biomass'~(mu*g~L^-1)) +
  scale_fill_brewer('Trophic group', type="qual", palette="Set2")
ggsave('Figures/Exploration/Zoops/Zoops_barplot_2021.png', height=5.5, width=7.5, dpi=1200)

ggplot(zoops_tg |> filter(!is.na(gnis_name),
                          year(Collection_Date)==2022)) +
  geom_bar(aes(DOY, tg_biomass, fill=Trophic_Group),stat='identity', width=7) +
  facet_wrap(~gnis_name, scales='free_y') +
  theme_bw() +
  labs(x='Day of year', y='Zooplankton biomass'~(mu*g~L^-1)) +
  scale_fill_brewer('Trophic group', type="qual", palette="Set2")
ggsave('Figures/Exploration/Zoops/Zoops_barplot_2022.png', height=5.5, width=7.5, dpi=1200)


