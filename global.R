
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(sf)
library(leaflet)
library(stringr)
library(geosphere)
library(waiter)
library(DT)
library(shinydisconnect)


# Prepare lake polys object -----------------------------------------------


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


# Define convenience functions --------------------------------------------

##FUNCTION TO TURN A STARTING POINT AND A BEARING ANGLE AND DISTANCE INTO A SECOND POINT TO DRAW A LINE TO.
calculate_bearing_point <- function(start_point, bearing, distance) {
  dest_point <- destPoint(start_point, bearing, distance)
  return(dest_point)
}

##FUNCTION FOR CONVERTING A BEARING TO ITS CARDINAL DIRECTION (N = 16).
bearing_funct = function(bearing) {
  
  bearing_dirs = c("S", "SSE", "SE", "ESE", "E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW")
  
  #START AT S, OTHERWISE, USE FOR LOOP TO COUNT DOWN TO APPROPRIATE DIR.
  if(bearing > 168.75 || bearing <= -168.75) { return("S") } else {
    curr = 146.25
    for(i in 2:16) {
      if(bearing > curr) {return(bearing_dirs[i])}
      curr = curr - 22.5
    }
  }
}


