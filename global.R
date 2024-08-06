library(dplyr)
library(shiny)
library(sf)
library(leaflet)
library(stringr)
library(geosphere)
library(waiter)
library(DT)
library(shinydisconnect)

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

#CREATING A LOOKUP TABLE FOR CONVERTING BEARING ANGLES TO CARDINAL DIRECTIONS
bearing_lbs = c(seq(from = -11.25, to = 168.75, by = 22.5), seq(from = -168.75, to = -33.75, by = 22.5))
bearing_ubs = c(seq(from = 11.25, to = 168.75, by = 22.5), seq(from = -168.75, to = -11.25, by = 22.5)) 
bearing_dirs = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
bearing_trans = data.frame(bearing_lbs, bearing_ubs, bearing_dirs)
#SOUTH IS A LITTLE WEIRD, AS IT STRADDLES -180/180.
bearing_trans[9,] = c(168.75, 180, "S")
bearing_trans[17,] = c(-180, -168.75, "S")
#SO IS NORTH
bearing_trans[1,] = c(-11.25, 0, "N")
bearing_trans[18, ] = c(0, 11.25, "N")


bearing_funct = function(bearing) {
  
  bearing_dirs = c("S", "SSE", "SE", "ESE", "E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW")
  
  if(bearing > 168.75 || bearing <= -168.75) { return("S") } else {
    curr = 146.25
    for(i in 2:16) {
      if(bearing > curr) {return(bearing_dirs[i])}
      curr = curr - 22.5
    }
  }
}
