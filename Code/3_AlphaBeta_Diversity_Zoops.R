#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Zooplankton community dynamics #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Code/01_ReadData.R')

library(vegan)

# prepare dataframes ####
sites_norepeats <- sites |>
  group_by(Reservoir_Name,Reservoir_ID, Res_Zone, Site_Name) |>
  summarise(Latitude=mean(Latitude),
            Longitude=mean(Longitude)) |>
  ungroup()

metadata <- left_join(water_quality |> mutate(datecode=as.numeric(datecode)),sif|> mutate(datecode=as.numeric(datecode))) |>
  select(-Latitude, -Longitude) |>
  mutate(Group=paste(Site_Name, datecode, sep="_"),
         Year=year(Collection_Date),
         Month=month(Collection_Date),
         JulianDay=yday(Collection_Date)) |>
  left_join(sites_norepeats) |>
  select(Group, Reservoir_Name, Res_Zone, Year, Month, JulianDay, Latitude, Longitude, percent_N:d34S, Temp_C:WaterDepth_m, chl_a_ugL) |>
  distinct()
  # needs to be filtered more when running analyses that require complete entries



# Alpha diversity analysis ####
# answers questions related to how diverse communities are 

# relative abundance at each sampling event (site and date)
zoop_rel_abund <- zoops |>
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
  drop_na(Species_Name)
 

#Taxonomy Line Plot - aggregated over all time within each lake
abundances |>
  drop_na()|>
  arrange(desc(mean)) |>
  mutate(Species_Name=factor(Species_Name)) |>
  #filter(mean>5) |> # annoying filter just to make plot less complicated by removing the lowest taxa
  ggplot(aes(rel_abund, reorder(Species_Name, rel_abund), fill=Reservoir_Name, color=Reservoir_Name)) +
  stat_summary(fun.data=median_hilow, geom = "pointrange",
               fun.args=list(conf.int=0.5),
               position = position_dodge(width=0.6), shape=21) +
  theme_classic() +
  labs(y=NULL,
       x="Relative Abundance (%)") +
  theme(legend.position = c(0.8,0.35)) 


# Beta diversity analysis ####
# answers questions related to how different communities are from each other 

# create distance matrix of zoop communities
dist_zoop <- zoops |>
  mutate(Group=paste(Site_Name, datecode, sep="_")) |>
  select(Group, Species_Name, biomass_ugL) |>
  pivot_wider(
    names_from = Species_Name,
    values_from = biomass_ugL,
    values_fn = sum,        # collapse duplicates
    values_fill = 0         # replace NA/NULL with 0
  ) |> as.data.frame()
rownames(dist_zoop) <- dist_zoop$Group
dist_zoop <- dist_zoop[,-1]
dist_zoop <- as.matrix(dist_zoop)
dist_zoop <- replace(dist_zoop, is.na(dist_zoop), 0)

dist <- vegdist(dist_zoop, method = 'bray')

dist


# PERMANOVA  with adonis2 can help test relationships between metadata and community composition

#   PERMANOVA is a statistical test used to compare the differences between groups of multivariate data.
# It is particularly useful when you have complex data with many variables and you want to see if the composition of these variables differs significantly between groups.

#   Distance Matrix: First, it calculates a distance matrix that quantifies the dissimilarity between all pairs of samples based on their multivariate data. In your case, this could be a measure of how different the zooplankton communities are from each other.
# Permutations: It then permutes (randomly rearranges) the data many times to create a distribution of possible outcomes under the null hypothesis (that there are no differences between groups).
# Comparison: It compares the observed differences between groups to this distribution to determine if the observed differences are statistically significant.

#   Unlike traditional ANOVA, PERMANOVA is nonparametric (i.e., it does not assume normal distribution of the data), making it suitable for ecological data, which often do not meet these assumptions.
# Multivariate: It can handle multiple variables at once, making it ideal for complex datasets like those involving multiple species of zoops.

adonis2(dist~Latitude, metadata)
adonis2(dist~Longitude, metadata)


# NMDS - beta dispersion plots 
set.seed(69420)
nmds <- metaMDS(dist)
nmds
# make note of the stress value, this shows how easy it was to condense multidimensional data into two dimensional space, below 0.2 is generally good -- 0.18 
# hmmm, we may not have enough data to perform NMDS

# pull out scores for beautiful plotting
scores <- scores(nmds) |>
  as_tibble(rownames='Group') |>
  # and join to metadata
  left_join(metadata)



scores |>
  filter(NMDS1<1000) |>
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(fill=Reservoir_Name),size=2,shape=21) +
  theme_minimal() +
  scale_fill_viridis_d('', option='magma') +
 # geom_text(label='dist~location \np = 0.97', mapping = aes(x = 1, y = 2)) +
  theme(legend.position = 'none')
