# Explore / Prep WDNR 24k Hydro Lines and Datasets
# Bryan Maitland
# April 2022


# GDB available at https://www.arcgis.com/home/item.html?id=c4bc634ba115498487174bda137f8de8
# Other shapefiles can be found at https://data-wi-dnr.opendata.arcgis.com/



# libraries
library(tidyverse)
library(here)
library(sf)

# Set some paths and crs
path.24k <- here("data","spatial","wdnr_24k_hydro.gdb")
path.24k.va <- here("data","spatial","wdnr_24k_hydro_va.gdb")
wi_crs <- 3071  # NAD83(HARN) / Wisconsin Transverse Mercator



## Data

### 24k WHD flowlines ----------------------------------------------------------

# check layers in 24kWHDPlus VA
st_layers(dsn = path.24k.va)

# read in 24k WHD VA flowlines
whd_lines  <-
  st_read(dsn = path.24k.va, layer = "WD_HYDRO_VA_FLWLN_NTWRK_LN_24K") %>% 
  st_transform(crs = wi_crs) %>%
  janitor::clean_names()

# filter out streams and lake line connectors
#  this removes GLs and dangles
whd_lines  <- whd_lines  %>% 
  filter(seedtype %in% c("isolated stream","network stream", "lake")) %>% 
  filter(!is.na(hydroid))  # removes 1 very small lake


# Read 24k base flowlines to get WBICS for streams 
tmp_lines <-
  rgdal::readOGR(
    dsn = path.24k,
    layer = "WD_HYDRO_FLOWLINE_LN_24K") %>%
  st_as_sf() %>%
  janitor::clean_names() 

tmp_lines <- tmp_lines %>%
  st_drop_geometry() %>%
  select(hydroid, hydrocode, hydrotype, wbic = river_sys_wbic) %>%
  as_tibble()

# links wbics to the whd_lines lines
whd_lines <- left_join(whd_lines, tmp_lines, by = "hydroid")

# check for NAs
map(tmp, ~sum(is.na(.)))
# 244 reaches without WBICS

# remove tmps
rm(tmp_lines)


### 24k WHD attribute data -----------------------------------------------------

# Characteristic of hydrographic features calculated at 5 spatial scales
# - channel
# - incremental riparian
# - trace (cumulative) riparian
# - incremental watershed
# - trace (cumulative) watershed

# list layers again
st_layers(dsn = path.24k.va)

# list of layers to read in
target_layers <- 
  st_layers(dsn = path.24k.va)$name %>% 
  as_tibble() %>% 
  filter(str_detect(value, "INFO|BASE|CHANNEL|RIPARIAN|WATERSHED")) %>% 
  pull()


# function to read and clean tables
load_24k_data <- function(path, layer_name) {
  st_read(dsn = path, layer = layer_name) %>% 
    janitor::clean_names() %>% 
    as_tibble()
}

# # test funtion
# load_24k_data(path.24k.va, "WD_HYDRO_VA_ATTRIBUTE_INFO_REF")
# 
# # test mapping function over target layer names
# target_layers[2] %>% map(~load_24k_data(path = path.24k.va, layer_name = .))

# read the data into a nested dataframe
whd_data <-
  data_frame(layer = target_layers) %>%
  mutate(
    layer_data = map(
      layer, 
      load_24k_data(path = path.24k.va, layer_name = .))
    )
whd_data


### 24k WHD catchements (HUC16s) -----------------------------------------------

whd_catchs <- load_24k_data(path.24k.va, "WD_HYDRO_VA_CATCHMENT_AR_24K")


### Other layers ---------------------------------------------------------------

#### Watersheds

huc8 <- st_read(here("data","spatial","shapefiles","hucs","huc8.shp")) %>% 
  clean_names() %>% st_transform(crs = 3071)
huc10 <- st_read(here("data","spatial","shapefiles","hucs","huc10.shp")) %>% 
  clean_names() %>% st_transform(crs = 3071)
huc12 <- st_read(here("data","spatial","shapefiles","hucs","huc12.shp")) %>% 
  clean_names() %>% st_transform(crs = 3071)
ecoreg <- st_read(
  here("data","spatial","shapefiles","ecoregions","wi_eco_l3.shp")) %>%
  clean_names() %>% st_transform(crs = 3071)


#### Wisconsin polygon
wisco_border <- wdnr.gis::wi_poly %>% 
  st_transform(crs = wi_crs) 




