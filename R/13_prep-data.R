# Compile and prepare fish and covariate data for multivariate analysis
# Bryan M Maitland
# April 2022


## Libraries
library(here)
library(tidyverse)


## Data ------------------------------------------------------------------------

load(here("data", "fish_data_clean.RData"))
sites_xref <- readRDS(here("data", "sites_whd_xref.rds"))
whd_ncm <- readRDS(here("data", "whd_ncm_preds.rds")) %>% janitor::clean_names() %>% as_tibble()
whd_data <- readRDS(here("data", "whd_data.rds")) %>% filter(hyd_cat == "stream")
whd_metadata <- readRDS(here("data", "whd_metadata.rds"))
whd_lines <- readRDS(here("data", "whd_lines.rds"))


## Prep covariate --------------------------------------------------------------

# Pull modeled flow and temp variables used in Diebel et al fish analyses
whd_ncm_metrics <- whd_ncm %>% 
  select(1 | 
           c(temp_max_cl_cc,  temp_july_cl_cc, temp_summer_cl_cc) | 
           c(apr_e10_c, aug_e50_c, aug_e50_c, wy_e50_c, wy_e90_c)) %>% 
  rename(reach_id = reachid)

# Reduce ncm to just nat comm classes
whd_ncm <- whd_ncm %>% select(1:3)

# Pull important attributes
whd_data <- whd_data %>% select(
  c(
    reach_id = reachid,
    # stream size 
    trw_area,
    # channel
    gradient, sinuosity, 
    # connectivity
    gl_dist, lake_sm_dist, lake_md_dist, lake_lg_dist, shed_100_dist, shed_1000_dist,
    # Valley form, soil permeability, and groundwater potential
    r_darcy, trr_darcy, w_darcy, trw_darcy, 
    r_slope, trr_slope, w_slope, trw_slope, 
    r_perm, trr_perm, w_perm, trw_perm, 
    # Surficial and bedrock geology in upstream watershed
    trw_br_1, trw_br_2, trw_br_3, trw_br_41, trw_br_42, 
    # Climate
    w_prcp_ann, trw_prcp_ann, 
    w_temp_ann, w_temp_gs, w_temp_july, 
    trw_temp_ann, trw_temp_gs, trw_temp_july,
    # adjacent land cover
    r_lu06_water = r_lu06_11,                      # water
    r_lu06_81, r_lu06_82,                          # ag
    r_lu06_41, r_lu06_42, r_lu06_43,               # forest
    r_lu06_90, r_lu06_95,                          # wetland
    r_lu06_21, r_lu06_22, r_lu06_23, r_lu06_24,    # urban
    r_lu06_grass = r_lu06_71,                      # grassland
    # upstream land cover
    trr_lu06_water = trr_lu06_11, 
    trr_lu06_81, trr_lu06_82,
    trr_lu06_41, trr_lu06_42, trr_lu06_43,
    trr_lu06_90, trr_lu06_95,   
    trr_lu06_21, trr_lu06_22, trr_lu06_23, trr_lu06_24, 
    trr_lu06_grass = trr_lu06_71,
    # adjacent watershed land cover
    w_lu06_water = w_lu06_11, 
    w_lu06_81, w_lu06_82,
    w_lu06_41, w_lu06_42, w_lu06_43,
    w_lu06_90, w_lu06_95,   
    w_lu06_21, w_lu06_22, w_lu06_23, w_lu06_24, 
    w_lu06_grass = w_lu06_71,
    # upstream watershed land cover
    trw_lu06_water = trw_lu06_11, 
    trw_lu06_81, trw_lu06_82,
    trw_lu06_41, trw_lu06_42, trw_lu06_43,
    trw_lu06_90, trw_lu06_95,   
    trw_lu06_21, trw_lu06_22, trw_lu06_23, trw_lu06_24, 
    trw_lu06_grass = trw_lu06_71
    )
  ) %>% 
  mutate(
    r_lu06_agric = r_lu06_81 + r_lu06_82, 
    r_lu06_forst = r_lu06_41 + r_lu06_42 + r_lu06_43, 
    r_lu06_wetld = r_lu06_90 + r_lu06_95,
    r_lu06_urban = r_lu06_21 + r_lu06_22 + r_lu06_23 + r_lu06_24, 
    
    trr_lu06_agric = trr_lu06_81 + trr_lu06_82, 
    trr_lu06_forst = trr_lu06_41 + trr_lu06_42 + trr_lu06_43, 
    trr_lu06_wetld = trr_lu06_90 + trr_lu06_95,
    trr_lu06_urban = trr_lu06_21 + trr_lu06_22 + trr_lu06_23 + r_lu06_24, 
    
    w_lu06_agric = w_lu06_81 + w_lu06_82, 
    w_lu06_forst = w_lu06_41 + w_lu06_42 + w_lu06_43, 
    w_lu06_wetld = w_lu06_90 + w_lu06_95,
    w_lu06_urban = w_lu06_21 + w_lu06_22 + w_lu06_23 + w_lu06_24, 
    
    trw_lu06_agric = trw_lu06_81 + trw_lu06_82, 
    trw_lu06_forst = trw_lu06_41 + trw_lu06_42 + trw_lu06_43, 
    trw_lu06_wetld = trw_lu06_90 + trw_lu06_95,
    trw_lu06_urban = trw_lu06_21 + trw_lu06_22 + trw_lu06_23 + trw_lu06_24, 
    ) %>% 
  select(
    -r_lu06_81, -r_lu06_82, -r_lu06_41, -r_lu06_42, -r_lu06_43, 
    -r_lu06_90, -r_lu06_95, -r_lu06_21, -r_lu06_22, -r_lu06_23, -r_lu06_24,
    -trr_lu06_81, -trr_lu06_82, -trr_lu06_41, -trr_lu06_42, -trr_lu06_43, 
    -trr_lu06_90, -trr_lu06_95, -trr_lu06_21, -trr_lu06_22, -trr_lu06_23, -trr_lu06_24,
    -w_lu06_81, -w_lu06_82, -w_lu06_41, -w_lu06_42, -w_lu06_43, 
    -w_lu06_90, -w_lu06_95, -w_lu06_21, -w_lu06_22, -w_lu06_23, -w_lu06_24,
    -trw_lu06_81, -trw_lu06_82, -trw_lu06_41, -trw_lu06_42, -trw_lu06_43, 
    -trw_lu06_90, -trw_lu06_95, -trw_lu06_21, -trw_lu06_22, -trw_lu06_23, -trw_lu06_24
  )


covars <- whd_data %>% left_join(whd_ncm_metrics, by = "reach_id")


## Prep fish data --------------------------------------------------------------


## Add WHD key (reachid) to fish data --------------

df_fish <- df_fish_long %>% 
  as_tibble() %>% 
  mutate(site.seq.no = as.character(site.seq.no)) %>% 
  left_join(sites_xref, by = "site.seq.no")

# Remove data with missing reachids (relates to 78 sites)
df_fish <- df_fish %>% filter(!is.na(reach_id))


## Summarize PA for each species and each stream reach -------------

# For all fish
df_fish_pa <- df_fish %>% 
  group_by(species, reach_id) %>% 
  summarise(present = sum(fish.count), 
            .groups = 'drop') %>% 
  # note no zeros because zeros not recorded in surveys
  mutate(present = if_else(present >= 1, 1, 0)) %>% 
  pivot_wider(names_from = "species", values_from = "present", values_fill = 0)

df_fish_pa %>% saveRDS(here("data", "df_fish_pa"))

# Sportfish
df_fish_pa_sport <- df_fish %>% 
  left_join(wdnr.fmdb::spp_ref %>% select(species, gamefish.flag), 
            by = "species") %>% 
  filter(gamefish.flag == "Y") %>% 
  group_by(species, reach_id) %>% 
  summarise(present = sum(fish.count), 
            .groups = 'drop') %>% 
  # note no zeros because zeros not recorded in surveys
  mutate(present = if_else(present >= 1, 1, 0)) %>% 
  pivot_wider(names_from = "species", values_from = "present", values_fill = 0)

df_fish_pa_sport %>% saveRDS(here("data", "df_fish_pa_sport"))


