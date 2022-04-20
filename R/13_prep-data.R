# Compile and prepare fish and covariate data for multivariate analysis
# Bryan M Maitland
# April 2022


## Libraries
library(here)
library(tidyverse)


## Data ------------------------------------------------------------------------

load(here("data", "fish_data_clean.RData"))
sites_xref <- readRDS(here("data", "sites_whd_xref.rds"))
whd_ncm <- readRDS(here("data", "whd_ncm_preds.rds"))
whd_attr <- readRDS(here("data", "whd_data.rds"))
whd_lines <- readRDS(here("data", "whd_lines.rds"))



## Add WHD key (reachid) to fish data ------------------------------------------

df_fish <- df_fish_long %>% 
  as_tibble() %>% 
  mutate(site.seq.no = as.character(site.seq.no)) %>% 
  left_join(sites_xref, by = "site.seq.no")

# Check for missing reachids - should relate to 78 sites
df_fish %>% 
  filter(is.na(reach_id)) %>% 
  distinct(site.seq.no) %>% count()

# Remove data with missing reachids
df_fish <- df_fish %>% filter(!is.na(reach_id))



## Summarize PA for each species and each stream reach -------------------------

df_fish_pa <- df_fish %>% 
  group_by(species, reach_id) %>% 
  summarise(present = sum(fish.count), 
            .groups = 'drop') %>% 
  # note no zeros because zeros not recorded in surveys
  mutate(present = if_else(present >= 1, 1, 0)) %>% 
  pivot_wider(names_from = "species", values_from = "present", values_fill = 0)

# Save PA df
df_fish_pa %>% saveRDS(here("data", "df_fish_pa"))


## Extract whd attributes ------------------------------------------------------

whd_attr_info <- whd_attr[1,2] %>% unnest(cols = layer_data) %>% 
  select(-table_name) %>% mutate(field_name = tolower(field_name))
whd_attr_base <- whd_attr[2,2] %>% unnest(cols = layer_data)
whd_attr_c <- whd_attr[3,2] %>% unnest(cols = layer_data)
whd_attr_r <- whd_attr[4,2] %>% unnest(cols = layer_data)
whd_attr_trr <- whd_attr[5,2] %>% unnest(cols = layer_data)
whd_attr_trw <- whd_attr[6,2] %>% unnest(cols = layer_data)
whd_attr_w <- whd_attr[7,2] %>% unnest(cols = layer_data)
