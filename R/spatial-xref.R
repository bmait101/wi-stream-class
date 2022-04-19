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
xwalk_swims_hydro <- readRDS(here("data","xref_swims_hydro.rds")) 

# Clean it up
xwalk_swims_hydro <- xwalk_swims_hydro %>% 
  filter(!is.na(reach_id)) %>% 
  # if a station has more than two records, get rid of them
  # we use spatial joins to get the best one
  group_by(swims.station.id) %>% 
  filter(!n() > 1) %>% 
  ungroup()


# New table with all sites in x and y
sites_xref <- inner_join(df_sites, xwalk_swims_hydro, by="swims.station.id") 

# And a table of sites to link (no match in xwalk) (and write tmp file for QGIS)
sites_to_fix <- df_sites %>% 
  filter(!site.seq.no %in% sites_xref$site.seq.no) %>%
  write_csv(here("data","tmp","sites_to_fix.csv"))
# 971

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

sites_fixed <- bind_rows(
  sites_to_fix_1,
  sites_to_fix_2, 
  sites_to_fix_3, 
  sites_to_fix_4
  ) %>% 
  select(-wbic.y, -seedtype, -hydrocode, -hydrotype,
         -distance, -wbic_match, -rank) %>% 
  rename(wbic = wbic.x, hydro_id = hydroid, reach_id = reachid) %>% 
  left_join(df_sites %>%
              select(site.seq.no, latitude, longitude), 
            by = "site.seq.no") %>% 
  relocate(c(latitude, longitude), .after = wbic)

rm(sites_to_fix_1); rm(sites_to_fix_2); rm(sites_to_fix_3); rm(sites_to_fix_4)

# How many still not fixed?
sites_to_fix_short <- 
  anti_join(sites_to_fix, sites_fixed, by = "site.seq.no") %>%
  write_csv(here("data","tmp","sites_to_fix_short.csv"))
# 53

nn_join_full_short <- nn_join_full %>% 
  filter(site.seq.no %in% sites_to_fix_short$site.seq.no) %>% 
  print(n=Inf)



# 96843735 needs same hydro as 89213237


sites_xref_amend <- sites_xref %>% 
  mutate(wbic = as.character(wbic)) %>% 
  bind_rows(sites_fixed) %>% 
  select(site.seq.no, hydro_id, reach_id)







# 1e. Deal with 6s =========================

xwalk_sites_whd %>% filter(str_detect(reach_id, "^6")) %>% 
  distinct(reach_id, .keep_all=TRUE)
# 11 6s - these can be used in trends, but not in other analysis right now

# how many surreys?
sixes <- xwalk_sites_whd %>% filter(str_detect(reach_id, "^6")) %>% 
  pull(site.seq.no)
df_surveys %>% filter(site.seq.no %in% sixes) %>% as_tibble() 
# ~ 51 sites

# Fix for reaches ids 
xwalk_sites_whd <- xwalk_sites_whd %>% 
  mutate(reach_id = case_when(
    reach_id == "600012990" ~ "200181039",
    reach_id == "600046690" ~ "200107673",
    reach_id == "600055815" ~ "200076898", 
    reach_id == "600084294" ~ "200149549", 
    
    reach_id == "600091889" ~ "200194156", # Namekegon main stem; gave it lagre trib
    reach_id == "600018043" ~ "200171508",  # Just upstream on Trout Creek
    reach_id == "600013543" ~ "200212215",  # Just upstream on Beaver Brook
    
    reach_id == "600031894" ~ "200142791",  # Just upstream 
    reach_id == "600008762" ~ "200187067",  # Just upstream
    reach_id == "600089491" ~ "200036761",  # Just upstream 
    reach_id == "600018757" ~ "200170441",  # Just upstream
    TRUE ~ reach_id
  ))

xwalk_sites_whd %>% 
  filter(str_detect(reach_id, "^6")) %>% 
  distinct(reach_id, .keep_all=TRUE)



# # get manual codes for these
# df_sites %>% filter(site.seq.no %in% nas$site.seq.no) %>% 
#   as_tibble() %>% 
#   select(site.seq.no, latitude, longitude) %>% 
#   write_csv(here("output", "data","nn_manual_fixes_nas.csv"))
# # do manual checks #
# 
# # load corrected data
# manual_fixed <- read_csv(here("output", "data","nn_manual_fixes_nas.csv"))
# 
# # sites to remove
# sites_to_remove <- manual_fixed %>% 
#   filter(is.na(hydro_id)) %>%
#   pull(site.seq.no)
# 
# sites_to_fix_matched4 <- manual_fixed %>% 
#   filter(!site.seq.no %in% sites_to_remove) %>% 
#   select(-notes, -latitude, -longitude) %>% 
#   mutate(across(where(is.double), as.character))
# 
# xwalk_sites_whd <- xwalk_sites_whd %>% 
#   select(-swims.station.id) %>% 
#   bind_rows(sites_to_fix_matched4) %>% 
#   distinct()
# 
# xwalk_sites_whd

# 2. Site x Watershed xref =====================================================

# Find points within polygons
sites_in_hucs8 <- st_join(sf_sites, sf_huc8, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, huc8_code)
sites_in_hucs10 <- st_join(sf_sites, sf_huc10, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, huc10_code)
sites_in_hucs12 <- st_join(sf_sites, sf_huc12, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, huc12_code)
sites_in_ecoreg <- st_join(sf_sites, sf_ecoreg, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, eco_code = us_l3code, ecoregion = us_l3name)



# 3. Site x stream class xref  =================================================


# # identify nearest classes stream to site and get their distances
# nn_trace <- st_nn(sf_sites, lines_classed, k = 1, returnDist = TRUE)
# 
# # extract the nn flowlines indexes and distances for each
# nn_distances <- 
# bind_cols(index = unlist(nn_trace$nn), distance = unlist(nn_trace$dist))
# 
# # join sites to flowlines using the nearest neighbor
# nn_join <- st_join(sf_sites, lines_classed, join = nngeo::st_nn, k = 1)
# 
# # bind the distances to the nn_join and check for wbic matches
# nn_join_full_stmcls <-
#   bind_cols(nn_join, nn_distances) %>%
#   st_drop_geometry() %>%
#   as_tibble() %>% 
#   select(site.seq.no, TROUT_CLAS, distance) %>% 
#   mutate(across(where(is.integer), as.character)) 

# save intermediary
# saveRDS(nn_join_full_stmcls, here("output","tmp","nn_join_full_stmcls_20210922.rds"))
nn_join_full_stmcls <- 
  read_rds(here("output","tmp","nn_join_full_stmcls_20210922.rds")) %>% 
  rename(trout_class = TROUT_CLAS)

# tmp <- sf_sites %>% left_join(nn_join_full_stmcls, by = "site.seq.no")
# tmp %>% filter(is.na(TROUT_CLAS))
# tmp %>% filter(distance>=26) %>% arrange(distance) 

# plot it
# ggplot() + 
#   geom_sf(data = lines_classed, aes(color=TROUT_CLAS)) + 
#   geom_sf(data = tmp, aes(color=TROUT_CLAS), size=.5, alpha=0.5) + 
#   geom_sf(data = tmp %>% filter(distance>=26), color = "black", size=.7) 

# So its not perfect, but use 26 m to filter out bad spatial joins ()
# people need to snap their damned sites 

nn_join_full_stmcls_26m <- nn_join_full_stmcls %>% 
  filter(distance <= 26)%>% 
  select(-OFFICIAL_N, -distance)


# 4. Link keys to sites ========================================================

df_sites %>% as_tibble() 

df_sites_va <- df_sites %>% 
  as_tibble() %>% select(-swims.station.id) %>% 
  left_join(xwalk_sites_whd, by = "site.seq.no") %>% 
  left_join(sites_in_hucs8, by = "site.seq.no") %>%
  left_join(sites_in_hucs10, by = "site.seq.no") %>%
  left_join(sites_in_hucs12, by = "site.seq.no") %>%
  left_join(sites_in_ecoreg, by = "site.seq.no") %>% 
  left_join(nn_join_full_stmcls_26m, by = "site.seq.no")

# Replace huc codes for actual names using Paul's wdnr.gis package
df_sites_va <- df_sites_va %>%
  left_join(wdnr.gis::watershed_lookup %>% 
              filter(huc_level == "HUC_8") %>% 
              select(-huc_level),
            by = c("huc8_code"="huc_codes"))  %>% 
  rename(huc_names8 = huc_names) %>%
  left_join(wdnr.gis::watershed_lookup %>% 
              filter(huc_level == "HUC_10") %>%
              select(-huc_level),
            by = c("huc10_code"="huc_codes"))  %>% 
  rename(huc_names10 = huc_names) %>%
  left_join(wdnr.gis::watershed_lookup %>%
              filter(huc_level == "HUC_12") %>% 
              select(-huc_level),
            by = c("huc12_code"="huc_codes"))  %>% 
  rename(huc_names12 = huc_names) %>%
  mutate(across(c(huc_names8, huc_names10, huc_names12), as.factor))


map(df_sites_va, ~sum(is.na(.)))


# 4. Deal with  NAs ============================================================

df_sites_va %>% filter(is.na(reach_id))

# Fix 8 NA reaches ids 
df_sites_va <- df_sites_va %>% 
  mutate(reach_id = case_when(
    site.seq.no == "130032" ~ "200142691",
    site.seq.no == "34941584" ~ "200187018",
    site.seq.no == "81689389" ~ "200094875", 
    site.seq.no == "1606534" ~ "200211780", 
    site.seq.no == "62227241" ~ "200211455", 
    site.seq.no == "1576800" ~ "200136625", 
    site.seq.no == "1632786" ~ "200204063", 
    site.seq.no == "265060511" ~ "200199092", 
    TRUE ~ reach_id
  )) %>% 
  mutate(hydro_id = if_else(is.na(hydro_id), reach_id, hydro_id))

map(df_sites_va, ~sum(is.na(.)))


# 5. Final fixes ===============================================================

# Fix for reaches ids that need to be switched for good daymet data
df_sites_va <- df_sites_va %>% 
  mutate(reach_id = case_when(
    reach_id == "200048608" ~ "200048644",
    reach_id == "200148328" ~ "200148707",
    reach_id == "200173988" ~ "200174095", 
    reach_id == "200197203" ~ "200197197", 
    TRUE ~ reach_id
  ))



# Save key added site data =====================================================

write_csv(df_sites_va, here("output","data","sites_list_va.csv"))
write_rds(df_sites_va, here("output","data","sites_list_va.rds"))


# # list of reach ids for Aaron Rusch for pulling WHDPlus data via SQL
# df_sites_va %>% distinct(reach_id) %>%
#   write_csv(here("output","tmp","trout_reachids.csv"))


# Clean up 
rm(sites_in_hucs8); rm(sites_in_hucs10); rm(sites_in_hucs12)
rm(sites_in_wtrmgnt); rm(sites_in_ecoreg)

rm(nn_trace)
rm(nn_join); rm(nn_distances); rm(nn_join_full)
rm(manual_fixed); rm(manual_fixes)
rm(sites_to_fix); rm(sites_to_fix_cln); rm(sites_to_fix_matched_final); 
rm(sites_to_fix_matched); rm(sites_to_fix_matched2); rm(sites_to_fix_matched3); rm(sites_to_fix_matched4)
rm(sites_to_fix)
rm(sites_to_fix_cln)
rm(sites_to_fix_matched_final)
rm(sites_x_swims)
rm(xwalk_sites_whd)
rm(xwalk_swims_hydro)
