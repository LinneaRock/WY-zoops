# initial viewing of data ####

library(tidyverse)


sif <- read.csv('Data/SIF_data_20250626.csv')


# find data matches ####

zoops <- read.csv('Data/zoops.csv')
wq <- read.csv('Data/water_quality.csv') |> distinct()

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


wq_matched <- wq |> left_join(sif)
