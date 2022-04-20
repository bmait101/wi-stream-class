# Process raw WDNR FWIS data pulls
# Bryan M Maitland
# April 2022

## Process raw data pulls from WDNR's FMIS

# - read in raw data pulls
# - tidy and wrangle

library(here)
library(tidyverse)

# Data -------------------------------------------------------------------------

load(here("data", "fish_raw_20220418.RData"))

# fix waterbody type column
df_efforts_raw <- df_efforts_raw %>% 
  mutate(waterbody.type = tolower(waterbody.type)) %>% 
  mutate(waterbody.type = str_replace(waterbody.type, " ", "_"))



# Filter for proofed and complete data  ----------------------------------------

df_surveys <- df_surveys_raw %>% 
  # surveys with fish data
  semi_join(df_fishraw, by = "survey.seq.no") %>%  
  # remove test site
  filter(site.seq.no != 315) %>%  
  # historical / complete and proofed data (don't keep recent, un-proofed data)
  filter(survey.status %in% c(
    "data_entry_complete_and_proofed",
    "historical_data_complete_and_proofed",
    "historical_data_entry_complete",
    "historical_data_load_status_unknown"
  ))


# Remove any lakes -------------------------------------------------------------


sites_to_remove <- c(
  122868,  # petenwell lake
  128668,  # gannet lake on MISS
  129991,  # MISS island
  122541,  # lake dubay
  122541,  # lake saint croix
  52346035, # lake saint croix
  122590,  # lake Wisco
  142536387,  # lake SUP
  100822749, # lake eau clair
  49494326,  # lake Mich
  129035,  # says oconomowoc_river but obvi Okauchee lake
  122096,  # Biron Flowage
  129982,  # lily pon MISS
  134239, # Toekn Creek 'lake"?? VA erroneously has this as a lake feature
  123664, # Upper Scott Flowage
  122161,  # CR lock
  37197582, # unnamed small stream not on hydro and v far from a line
  122205,  # cox hollow lake
  130038  # battle slow - MISS
)

df_surveys <- df_surveys %>%
  filter(!site.seq.no %in% sites_to_remove) %>% 
  filter(!str_detect(waterbody.name, 
                     "mississippi|flowage|millpond|mill_pond|_r_fl")) %>% 
  filter(!str_detect(station.name, 
                     "mississippi|flowage|millpond|mill_pond|_r_fl")) 



# reduce fish and efforts to good surveys
df_fish <- df_fishraw %>% semi_join(df_surveys, by = "survey.seq.no")
df_efforts <- df_efforts_raw %>% semi_join(df_surveys, by = "survey.seq.no")


## QC fish data ----------------------------------------------------------------

### Zeros and no fish captured 

# Get list of surveys species == "no_fish_captured"  
no_fish_surveys <- df_fish %>% 
  filter(species == "no_fish_captured") %>% 
  distinct(survey.seq.no) %>% 
  pull() 
# 440

# remove these records 
df_fish <- df_fish %>% 
  filter(!survey.seq.no %in% no_fish_surveys) %>% 
  droplevels()

# How many NAs | zeros for fish counts?
df_fish %>% 
  filter(is.na(fish.count) | fish.count == 0) %>% 
  tally()  # 0

# So all the zero counts were associated with "species==no_fish_captured"


### Species prevalence in surveys (min 100 observations)

# How many surveys are each species observed in?
surveys_per_species <- df_fish %>% 
  group_by(species) %>% 
  mutate(n_survs_present = n_distinct(survey.seq.no)) %>% 
  ungroup() %>% 
  distinct(species, .keep_all = TRUE) %>% 
  select(species, n_survs_present) %>% 
  left_join(wdnr.fmdb::spp_ref, by = "species") %>%  # link species ref data
  arrange(thermal.guild.name, desc(n_survs_present)) %>% 
  relocate(thermal.guild.name, .before = species)

# Get list of species to keep
spp_to_keep <- surveys_per_species %>% 
  filter(n_survs_present >= 100) %>%
  filter(!is.na(latin.name)) %>%  
  filter(!stringr::str_detect(latin.name, "_spp")) %>% 
  filter(!stringr::str_detect(latin.name, "idae")) %>% 
  filter(!stringr::str_detect(species, "crayfish")) %>% 
  filter(!stringr::str_detect(species, "_x_")) %>% 
  filter(!stringr::str_detect(species, "ammocoete")) %>% 
  filter(!species %in% c(
    # GL salmonids
    "coho_salmon", "lake_trout", "chinook_salmon", "lake_whitefish", "siscowet",
    # Other GLs and lake fish
    "sea_lamprey", "alewife", "brook_silverside"
  )) %>% 
  select(2:3)
# 89 species

# Filter fish data for good species
df_fish <- df_fish %>% 
  filter(species %in% spp_to_keep$species) # ~24k records removed


### Expand counts to one row per fish and add lengths from bins
df_fish_long <- df_fish %>% 
  wdnr.fmdb::expand_counts() %>% 
  wdnr.fmdb::length_bin_to_length()


### Reduce surveys/efforts to seqs in filtered fish data
df_surveys <- df_surveys %>% semi_join(df_fish, by = "survey.seq.no") 
df_efforts <- df_efforts %>% semi_join(df_fish, by = "survey.seq.no")



# Save cleaned data objects ----------------------------------------------------

save(
  df_surveys, df_efforts, df_fish_long, 
  file = here::here("data", "fish_data_clean.RData")
)

# To load the data again
# load(here("data","fish_clean.RData"))


# Compile -----------------------------------

# NEED REACH-LEVEL PA DATASET - SO GO MAKE REACHid X SITE.SEQ XWALK






