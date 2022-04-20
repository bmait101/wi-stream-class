# Linking surveys sites to WHD 24k Hydro Plus (reachIDs)
# Bryan M Maitland
# April 2022

# Overview ---------------------------------------------------------------------

# Key link to stream data is reach_id (and hydro_id)

# WDNR Fish Management surveys should each have a Site ID and a SWIMS ID, 
# and SWIMS stations should be snapped onto WHD and have associated hydroids.

# BUT, there are tons of stations that were put in at confluences or not snapped,
# so they snapped to multiple hydroids, or that were snapped 
# to a polygon instead of a flowline, etc 

# - xref sites with SWIMS/WHD xwalk table 
# - identify sites that need to be associaed with WHD
# - spatially reference sites

## Prep
library(here)
library(tidyverse)
library(sf)
library(nngeo)


## Data ---------------

# WHD VA lines
whd_lines <- readRDS(here("data", "whd_lines.rds"))

# Fish surevys
load(here::here("data", "fish_data_clean.RData"))

## Prep data ---------------

# Get list of distinct surveys sites (and write tmp file for QGIS)
df_sites <- df_surveys %>%
  select(site.seq.no, swims.station.id, wbic, latitude, longitude) %>%
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  mutate(across(c(swims.station.id,site.seq.no), as.character))  %>% 
  as_tibble() %>% 
  write_csv(here("data", "tmp", "sites.csv"))


## Xref with SWIMS/hydro xwalk -------------------------------------------------

# First, load SWIMS/hydroid xwalk table (from Aaron Reusch)

# NOTE: some SWIMS stations have multiple hydroids and reach ids
# - some with multiple 6s (on edge between two features)
# - some with 2s and 6s
# - some with multiples (site on a confluence)
# - some with NAs (station is not snapped)
# - Namekegon issue: whole river is a lake with a 6 reach id

# load xwalk
xwalk_swims_hydro <- readRDS(here("data","xwalk_swims_hydro.rds")) 

# Clean it up
xwalk_swims_hydro <- xwalk_swims_hydro %>% 
  filter(!is.na(reach_id)) %>% 
  # if a station has more than two records, get rid of them
  # we use spatial joins to get the best one
  group_by(swims.station.id) %>% 
  filter(!n() > 1) %>% 
  ungroup()


# New table with all sites in x and y
sites_xref <- 
  inner_join(df_sites, xwalk_swims_hydro, by="swims.station.id") %>% 
  select(site.seq.no, hydro_id, reach_id)

# And a table of sites to link (no match in xwalk) (and write tmp file for QGIS)
sites_to_fix <- df_sites %>% 
  filter(!site.seq.no %in% sites_xref$site.seq.no) %>%
  write_csv(here("data","tmp","sites_to_fix.csv"))
# 940

rm(xwalk_swims_hydro)


# Spatial analysis -------------------------------------------------------------

# Make sites spatial 
points <- sites_to_fix %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(whd_lines))  # CRS of the 24k WHD

# Join sites to flowlines using the nearest neighbor
system.time(
  nn_join <- st_join(points, whd_lines, join = nngeo::st_nn, k = 3)
  )
# 25 minutes

# NN search: identify nearest 3 flowlines to site and get their distances
system.time(
  nn_trace <- st_nn(points, whd_lines, k = 3, returnDist = TRUE)
  )
# 25 minutes


# extract the nn flowlines indexes and distances from nn search
nn_distances <- 
  bind_cols(
    index = unlist(nn_trace$nn),
    distance = unlist(nn_trace$dist)
    )

# Compile join and nn search information
nn_join_full <- bind_cols(nn_join, nn_distances) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(-confl2conflid, -traceid, -shape_length, -index) %>% 
  filter(!seedtype == "lake") %>% 
  mutate(across(where(is.integer), as.character)) %>%
  mutate(wbic_match = wbic.x == wbic.y)  %>% 
  group_by(site.seq.no) %>% 
  mutate(rank = row_number(distance)) %>% 
  ungroup()

# save intermediary
saveRDS(nn_join_full, here("data", "tmp", "nn_join.rds"))
# nn_join_full <- read_rds(here("data","tmp","nn_join.rds"))

# clean up
rm(nn_join); rm(nn_trace); rm(nn_distances); rm(points)


# Process join -----------------------------------------------------------------

# Keep the hydro record closest to the point IF wbic matches survey data
sites_to_fix_1 <- nn_join_full %>% 
  filter(rank==1 & wbic_match==TRUE)

sites_to_fix_2 <- nn_join_full %>% 
  filter(!site.seq.no %in% sites_to_fix_1$site.seq.no) %>% 
  filter(rank==2 & wbic_match==TRUE) 

sites_to_fix_3 <- nn_join_full %>% 
  filter(!site.seq.no %in% sites_to_fix_1$site.seq.no) %>% 
  filter(!site.seq.no %in% sites_to_fix_2$site.seq.no) %>% 
  filter(rank==3 & wbic_match==TRUE) 

# Many surveys have 9999s for wbics, so not matched
sites_to_fix_4 <- nn_join_full %>% 
  filter(str_detect(wbic.x, "^99")) %>% 
  filter(rank == 1) %>% 
  arrange(desc(distance))

sites_fixed <- 
  bind_rows(
    sites_to_fix_1,
    sites_to_fix_2, 
    sites_to_fix_3, 
    sites_to_fix_4
    ) %>% 
  select(site.seq.no, hydro_id = hydroid, reach_id = reachid) 

rm(sites_to_fix_1); rm(sites_to_fix_2); rm(sites_to_fix_3); rm(sites_to_fix_4)

# How many still not fixed? (update the object)
sites_to_fix <- sites_to_fix %>% 
  anti_join(sites_fixed, by = "site.seq.no") %>%
  write_csv(here("data","tmp","sites_to_fix_short.csv"))
# 44

# How many surveys is this?
df_surveys %>% 
  filter(site.seq.no %in% sites_to_fix_short$site.seq.no) %>% 
  as_tibble() %>% 
  print(n=Inf)
# so 80 surveys on 44 sites. 

# These probably need manual fixing and we will drop them for this analysis


# Add spatially referenced sites to the xref table -----------------------------
sites_xref <- sites_xref %>% 
  bind_rows(sites_fixed) %>% 
  # 96843735 needs same hydro as 89213237
  mutate(reach_id = if_else(site.seq.no=="96843735", "89213237", reach_id))



#  Deal with 6s ----------------------------------------------------

sites_sixes <- sites_xref %>% 
  filter(str_detect(reach_id, "^6")) %>% 
  left_join(df_sites %>% select(site.seq.no,latitude,longitude),
            by="site.seq.no") %>% 
  write_csv(here("data","tmp","sites_to_fix_sixes.csv"))
# 34 sites (and all from initial xref - non got added by spatial joins)

# how many surveys?
df_surveys %>% 
  filter(site.seq.no %in% sites_sixes$site.seq.no) %>% 
  as_tibble() %>% arrange(site.seq.no) %>% 
  print(n=Inf) %>% View()
# 101 surveys on 34 sites (26 reachids)


# These all appear to good wadable or non-wadable stream sites, 
# but the whd location they are snapped to or near is incorrectly classified
# as a lake or pond. So, 
# - there is limited data for these as they were not attributed
# - no worth putting them on a close  by segment with data, 
#   because some are far away, so data not representative. 

# Remove these sites from the xref list
sites_xref <- sites_xref %>% 
  filter(!site.seq.no %in% sites_sixes$site.seq.no)

# These should add up to the number of disctint sites in data (n = 12,417)
nrow(sites_xref) + nrow(sites_to_fix) + nrow(sites_sixes)

# Save final site list with linked whd key
sites_xref %>% saveRDS(here("data", "sites_whd_xref.rds"))


#========= END =========#