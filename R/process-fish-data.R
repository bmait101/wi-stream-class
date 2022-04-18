# Process raw WDNR FWIS data pulls
# Bryan M Maitland
# April 2022


library(dplyr)


# Data -------------------------------------------------------------------------

df_surveys_raw <- readRDS(here::here("data", "raw_surveys_20220413.rds"))
df_efforts_raw <- readRDS(here::here("data", "raw_efforts_20220413.rds"))
df_fishraw <- readRDS(here::here("data", "raw_fish_20220413.rds"))


# Filter for proofed and complete data  -------------------------

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


# reduce fish 
df_fish <- df_fishraw %>% semi_join(df_surveys, by = "survey.seq.no")

# reduce efforts 
df_efforts <- df_efforts_raw %>% semi_join(df_surveys, by = "survey.seq.no")


# QC fish data ------------------------

# How many species == "no_fish_captured" 
df_fish %>% 
  filter(species == "no_fish_captured") %>% 
  tally() # 757

# Save these surveys seq 
no_fish_surveys <- df_fish %>% 
  filter(species == "no_fish_captured") %>% 
  distinct(survey.seq.no) %>% 
  pull()

# and Remove these records 
df_fish <- df_fish %>% 
  filter(!species == "no_fish_captured") %>% 
  droplevels()


# How many NAs for fish counts?
df_fish %>% filter(is.na(fish.count)) %>% 
  tally()  # 0

# How many zero counts?
df_fish %>% filter(fish.count == 0) %>% 
  tally()  # 0


# reduce surveys to seqs in filtered fish data
df_surveys <- df_surveys %>% semi_join(df_fish, by = "survey.seq.no")

# reduce efforts to seqs in filtered fish data
df_efforts <- df_efforts %>% semi_join(df_fish, by = "survey.seq.no")


# Check and remove species -------------------------

df_fish %>% group_by(species) %>% count() %>% arrange(desc(n)) %>% print(n=Inf)

# How many surveys are each species observed in?
surveys_per_species <- df_fish %>% 
  group_by(species) %>% 
  mutate(count = n_distinct(survey.seq.no)) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  select(species, count) %>% 
  left_join(wdnr.fmdb::spp_ref, by = "species") %>% 
  filter(!is.na(latin.name)) %>% 
  filter(!stringr::str_detect(latin.name, "_spp")) %>% 
  filter(!stringr::str_detect(latin.name, "idae")) %>% 
  filter(!stringr::str_detect(species, "crayfish")) %>% 
  filter(!stringr::str_detect(species, "_x_")) %>% 
  filter(!stringr::str_detect(species, "ammocoete")) %>% 
  filter(!species %in% c(
    "coho_salmon", "lake_trout", "chinook_salmon", "lake_whitefish", "siscowet"
  )) %>% 
  arrange(thermal.guild.name, desc(count))

surveys_per_species %>% 
  filter(thermal.guild.name=="transitional") %>% select(1:4) %>% print(n=Inf)
surveys_per_species %>% filter(species=="slimy_sculpin")


# species to remove 
# - ammocetes
# - latin name = NA


## Expand counts ------------------

df_trout <- df_trout %>% 
  wdnr.fmdb::expand_counts() %>% 
  wdnr.fmdb::length_bin_to_length()

# convert lengths to mm
df_trout <- df_trout %>% 
  mutate(length_mm = length * 25.4)

