library(dplyr)
library(shiny)
library(sf)
library(leaflet)
library(stringr)
library(geosphere)
library(waiter)
library(DT)

# lakes = st_read("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Resources\\unneeded\\dnr_hydro_features_all.shp")
# lakes2 = lakes %>%
#  filter(!is.na(dowlknum))
# lakes3 = lakes2 %>%
#    filter(cty_name != "NOT IN MN", outside_mn != "Y") %>%
#    select(DOW = dowlknum, pw_basin_n, pw_parent_, acres, shore_mi, center_utm, center_u_1, cty_name, map_label, shape_Leng, shape_Area)
# 
# lakes3 = st_transform(lakes3, crs = 4326)
# 
#  lakes3 = lakes3 %>%
#    group_by(DOW) %>%
#    slice_max(acres, with_ties = F) %>%
#    ungroup()
# 
#  lakes3 = lakes3 %>% st_make_valid()
# 
#  saveRDS(lakes3, "inputs/lakes.RDS")

lake_polys = readRDS("inputs/lakes.RDS")

calculate_bearing_point <- function(start_point, bearing, distance) {
  dest_point <- destPoint(start_point, bearing, distance)
  return(dest_point)
}
