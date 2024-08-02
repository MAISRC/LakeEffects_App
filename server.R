server = function(input, output, session) {
  
  #REACTIVES
  # pin_df = reactiveVal(lake_polys[1,])
  most_recent_bearing_num = reactiveVal(314)
  most_recent_lake_choice = reactiveVal("00000000")
  
  pin_map_waiter = Waiter$new(id = "fetch_fourth", 
                              hide_on_render = T, 
                              html = spin_loaders(7, color = "lightblue"), 
                              color = "darkred", 
                              fadeout = TRUE)

  entire_enter2calc_waiter = Waiter$new(id = "entire_enter2calc",
                                        hide_on_render = T, 
                                        html = spin_loaders(7, color = "lightblue"), 
                                        color = "darkred", 
                                        fadeout = TRUE)
  entire_template2calc_waiter = Waiter$new(id = "entire_template2calc",
                                        hide_on_render = T, 
                                        html = spin_loaders(7, color = "lightblue"), 
                                        color = "darkred", 
                                        fadeout = TRUE)
  enter2calc_df = reactiveVal(data.frame(lat = numeric(0), 
                                         lng = numeric(0), 
                                         lake = character(0)))
  allow_fetch_processing = reactiveVal(FALSE)
  

output$methods = renderUI({
  
  if(input$pick_effect != "No selection") {
    
    removeUI("#startup-header")
    
    if(input$pick_effect == "Calculate fetch") {
      
      selectInput("fetch_method", 
                  label = "How'd you like to calculate fetch?", 
                  choices = c("No selection", "By clicking a map", "By entering coordinates", "By uploading a CSV file"), 
                  selected = "No selection",
                  width = "100%")
      
   # session$sendCustomMessage("fade", list(id = "methods", action = "insert"))
      
    }
    
  } else {
    removeUI("#fetch_first"); removeUI("#fetch_second"); removeUI("#fetch_third"); removeUI("#fetch_fourth")
    NULL
  #  session$sendCustomMessage("fade", list(id = "methods", action = "remove"))
  }
  
})

##HANDLER FOR MOVING PAST THE FIRST FETCH QUESTION--WHICH IS LAKE SEARCH OR DOWNLOAD CSV FILE.
observeEvent(input$fetch_method, {
  
  enter2calc_df = reactiveVal(data.frame(lat = numeric(0), 
                                         lng = numeric(0), 
                                         lake = character(0))) #TOGGLE THIS BACK ANY TIME THE METHOD CHANGES
  
  if(isTruthy(input$fetch_method) &&
     input$fetch_method != "No selection") {

    if(input$fetch_method == "By clicking a map" |
       input$fetch_method == "By entering coordinates") {
      
      removeUI("#fetch_first"); removeUI("#fetch_second"); removeUI("fetch_third"); removeUI("#fetch_fourth"); output$dropped_pin_coords = renderText({}); output$report_pin_fetch = renderText({})
      
      insertUI(selector = "#sidebar", 
               where = "beforeEnd",
               ui = div(id = "fetch_first",
                        textInput("fetch_lake_search", 
                                  "Search for a specific lake here.", 
                                  value = "")
                        )
               )
    } else {
     
    removeUI("#fetch_first"); removeUI("#fetch_second"); removeUI("fetch_third"); removeUI("#fetch_fourth")
      
      insertUI(selector = "#sidebar", 
               where = "beforeEnd",
               ui = div(id = "fetch_first",
                        p("If you haven't already, download a template CSV file below.", class = "control-label"),
                        downloadButton("fetch_template",
                                       label = "Download"),
                        br(),
                        actionButton("fetch_first_proceed", "Got it!")
               )
      )
      
    }
  } else { removeUI("#fetch_first"); removeUI("#fetch_second"); removeUI("#fetch_third"); removeUI("#fetch_fourth") }
  
})

#HANDLER FOR DEALING WITH LAKE SEARCHES ON THE FETCH TAB.
observeEvent(input$fetch_lake_search, priority = 2, {
  
  if(isTruthy(input$fetch_lake_search) && 
     nchar(input$fetch_lake_search) > 2) {
    
    choices = lake_polys %>% 
      st_drop_geometry() %>% 
      select(DOW, map_label) %>% 
      filter(grepl(input$fetch_lake_search, DOW, ignore.case = T) |
               grepl(input$fetch_lake_search, map_label, ignore.case = T)) %>% 
      head(200) %>% 
      select(DOW, map_label)
    
    if(nrow(choices) > 0) {
      
      pretty = unlist(lapply(1:nrow(choices), function(x) {
        
        paste0(choices$map_label[x], " (DOW: ", choices$DOW[x], ")")
        
      }))
      
      choices_done = pull(choices, DOW)
      names(choices_done) = pretty
      choices_done = c("No selection" = "No selection", choices_done)
      
      removeUI("#fetch_second"); removeUI("#fetch_third"); removeUI("#fetch_fourth")
      
      insertUI(selector = "#sidebar", 
               where = "beforeEnd",
               ui = div(id = "fetch_second", 
                        selectInput(inputId = "fetch_lake_choice",
                                    label = "Select a lake.", 
                                    choices = choices_done)
               ))
      
    } else { removeUI("#fetch_second"); removeUI("#fetch_third"); removeUI("#fetch_fourth") }
  } else { removeUI("#fetch_second"); removeUI("#fetch_third"); removeUI("#fetch_fourth"); most_recent_lake_choice("00000000") }
  
  most_recent_bearing_num("No selection")
  output$dropped_pin_coords = renderText({})
  output$report_pin_fetch = renderText({})

})


observeEvent(input$fetch_lake_choice, {

  if(isTruthy(input$fetch_lake_choice) &&
     (input$fetch_lake_choice != "No selection")) {
    
    removeUI("#fetch_third"); removeUI("#fetch_fourth")
    
    insertUI(selector = "#sidebar", 
             where = "beforeEnd",
             ui = div(id = "fetch_third", 
                      selectInput("fetch_num_bearings", 
                                  "Select a number of bearings to try.",
                                   choices = c("No selection", "180", "90", "60", "45", "36", "30", "20", "18", "15", "12", "10", "9", "6", "5", "4", "3", "2", "1"), 
                                  selected = "No selection")
             ))
    
  } else { removeUI("#fetch_third"); removeUI("#fetch_fourth"); output$click2calc_lake = renderLeaflet({}) }
  
  most_recent_bearing_num("No selection")
  output$dropped_pin_coords = renderText({})
  output$report_pin_fetch = renderText({})
  
})

observeEvent(input$fetch_first_proceed, {
  
  if(isTruthy(input$fetch_first_proceed) &&
      !is.null(input$fetch_first_proceed) &&
       input$fetch_first_proceed > 0) {
    
    removeUI("#fetch_third"); removeUI("#fetch_fourth")
    
    insertUI(selector = "#sidebar", 
             where = "beforeEnd",
             ui = div(id = "fetch_third", 
                      selectInput("fetch_num_bearings", 
                                  "Select a number of bearings to try.",
                                  choices = c("No selection", "180", "90", "60", "45", "36", "30", "20", "18", "15", "12", "10", "9", "6", "5", "4", "3", "2", "1"), 
                                  selected = "No selection")
             ))
    
  } else { removeUI("#fetch_third"); removeUI("#fetch_fourth") }
  
  most_recent_bearing_num("No selection")
  
})


##NOW, WE HAVE TO START BRINGING UP THE MORE SPECIFIC FETCH INPUT WIDGETS

observeEvent(list(input$fetch_lake_choice,input$fetch_num_bearings), {

  if(isTruthy(input$fetch_num_bearings) &&
     input$fetch_num_bearings != "No selection") {
    
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "No selection" &&
       input$fetch_method == "By clicking a map" &&
       most_recent_bearing_num() == "No selection") { #Only run when switching from No selection to any value.

      #IF THEY ARE FIDDLING WITH THE LAKE CHOICE SELECTOR, CLEAR THE MAP OUT
      if(most_recent_lake_choice() != "No selection" && 
        most_recent_lake_choice() != input$fetch_lake_choice) {
        removeUI("#fetch_fourth"); output$click2calc_lake = renderLeaflet({}) 
      } else {

     removeUI("#startup_main")

     insertUI(selector = "#main-panel",
               where = "beforeEnd",
               ui = div(id = "fetch_fourth",
                        p("Click a location within your chosen lake's polygon (blue line) below to proceed.", id = "pin_instructions"),
                        leafletOutput("click2calc_lake", width = "500px"),
                        htmlOutput("dropped_pin_coords")
               ))

      output$click2calc_lake = renderLeaflet({
        
        tmp1 = lake_polys %>% 
           filter(DOW == input$fetch_lake_choice)
        
        bounds = unname(st_bbox(tmp1))
          
        leaflet(options = tileOptions(maxZoom = 20, minZoom = 6)) %>% 
          addTiles() %>% 
          addPolygons(data = tmp1$geometry, 
                      stroke = TRUE, 
                      group = "lake_polygon",
                      color = "darkblue", 
                      weight = 4, 
                      opacity = 0.9, 
                      fill = T, 
                      fillColor = "lightblue",
                      fillOpacity = 0.7,
                      label = HTML(paste0("Lake: ", tmp1$map_label,
                                          "<br>", "DOW: ", tmp1$DOW))) %>% 
        fitBounds(bounds[1], bounds[2],
                    bounds[3], bounds[4])
            
      })
      }

    }
    
    
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "No selection" &&
       input$fetch_method == "By entering coordinates" &&
       input$fetch_lake_choice != "No selection" &&
       input$fetch_num_bearings != "No selection") {
      
      removeUI("#startup_main")
      
      tmp1 = lake_polys %>% 
        filter(DOW == input$fetch_lake_choice)
      
      bounds = round(unname(st_bbox(tmp1)), 4)
      
      insertUI(selector = "#main-panel",
               where = "beforeEnd",
               ui = div(id = "fetch_fourth",
                        numericInput("enter2calc_lat", 
                                     "Enter latitude value",
                                     min = bounds[2], 
                                     value = round(mean(c(bounds[4],bounds[2])), 4),
                                     max = bounds[4],
                                     step = 0.00001),
                        numericInput("enter2calc_lng", 
                                     "Enter longitude value",
                                     min = bounds[1], 
                                     value = round(mean(c(bounds[1],bounds[3])), 4),
                                     max = bounds[3],
                                     step = 0.00001),
                        actionButton("submit_entered_pt", 
                                     "Add point to table"),
                        htmlOutput("invalid_pt_warn"),
                        br(), #^^^^
                        dataTableOutput("enter2calc_table"),
                        actionButton("remove_selected", 
                                     "Remove selected rows")
                        ))
      
    }
    
    
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "No selection" &&
       input$fetch_method == "By uploading a CSV file") {
      
      
    }
    
  } else { removeUI("#fetch_fourth")
    output$click2calc_lake = renderLeaflet({}) 
    }
  
  most_recent_bearing_num(input$fetch_num_bearings) #Set this to the most recent outcome
  most_recent_lake_choice(input$fetch_lake_choice)
  
})


observeEvent(input$click2calc_lake_click, {

  req(input$click2calc_lake_click)
  
  removeUI("#pin_instructions")
  removeUI("#fetch_action")
  output$dropped_pin_coords = renderText({})
  output$report_pin_fetch = renderText({})

  df = data.frame(lat = input$click2calc_lake_click$lat,
             lng = input$click2calc_lake_click$lng)

  sfdf = st_as_sf(df,
                  coords = c("lng", "lat"),
                  crs = 4326)

  tmp1 = lake_polys %>% 
    filter(DOW == input$fetch_lake_choice)
  
  sfdf_T = st_transform(sfdf, crs = 32615)
  lake_T = st_transform(tmp1[1,], crs = 32615)

  if(lengths(st_intersects(lake_T, sfdf_T)) > 0) {

  leafletProxy("click2calc_lake") %>%
    clearGroup("fetch_lines") %>%
    clearMarkers() %>%
    addMarkers(data = sfdf)

  output$dropped_pin_coords = renderText({

    paste0("Pin latitude: ", round(input$click2calc_lake_click$lat, 4), "<br>",
           "Pin longitude: ", round(input$click2calc_lake_click$lng, 4))

  })
  
  insertUI("#fetch_fourth", 
           ui = div(id = "fetch_action", 
                    actionButton("calc_fetch_button", 
                             "Calculate fetch!"),
                    htmlOutput("report_pin_fetch")))

  } else {

    output$dropped_pin_coords = renderText({
      
      leafletProxy("click2calc_lake") %>%
        clearGroup("fetch_lines") %>%
        clearMarkers()

      paste0("You clicked outside of the lake's polygon! Click within the polygon to proceed.")

    })

  }

})


observeEvent(input$calc_fetch_button, {

    pin_map_waiter$show()

    #CACHE BEARINGS AND DEGREE STEP LENGTHS
    ttl_bearings = as.numeric(input$fetch_num_bearings)
    step_size = 180/ttl_bearings

    bearings2try = seq(-180, 180, by = step_size)
    bearings2try = bearings2try[-length(bearings2try)]

    #CREATE POINTS IN THE DIRECTIONS OF ALL THE PROPER BEARINGS
    bearing_pts_aslist = lapply(bearings2try, function(x) {
      calculate_bearing_point(start_point = c(input$click2calc_lake_click$lng,
                                input$click2calc_lake_click$lat),
                              bearing = x,
                              distance = 50000 #meters
                              )
    })
    bearing_pts_asmat = do.call(rbind, bearing_pts_aslist)
    colnames(bearing_pts_asmat)[1] = "lng" #FOR MATCHING WITH SF/LEAFLET EXPECTATIONS FOR NAMING

    #STASH INPUTTED POINT IN A DF, ADD PROCEDURAL COLS TO IT
    pt_df = data.frame(
      lng = input$click2calc_lake_click$lng,
      lat = input$click2calc_lake_click$lat
    )

    pt_bearing_pairs = apply(bearing_pts_asmat, 1, rbind, pt_df) #BIND CURRENT PT. TO EACH POSSIBLE BEARING 1:1.
    
    tmp1 = lake_polys %>% 
      filter(DOW == input$fetch_lake_choice)

    current.lake.poly = tmp1[1, ] #GRAB LAKE POLY
    current.lake.poly = st_transform(current.lake.poly, crs = 4326)

    #CONVERT PT, BEARING PT PAIRS INTO SF, MAKE LINE STRING BETWEEN, SEPARATED BY ANY INTERSECTIONS W/ POLYGON
    all_segs = lapply(pt_bearing_pairs, function(x) {
      as.matrix(x) %>%
        st_linestring() %>%
        st_sfc(crs = 4326) %>% #THE RAW DATA NO LONGER HAVE A CRS W/O THIS LINE
        st_intersection(current.lake.poly) %>%
        st_cast("LINESTRING")
    })

    #FIND LINE STRING SEGMENT CLOSEST TO OUR PT (NOT SORTED, SO TEDIOUS)
    closest_segs = lapply(all_segs, function(x) {
      x[unlist(
        st_intersects(
          st_buffer(
            st_as_sf(pt_df,
                     coords = c("lng", "lat"),
                     crs = 4326),
            1),
          x))[1]]
    })

    #STORE ALL LENGTH DATA FOR EVERY SEGMENT.
    result_df = data.frame(length = unlist(lapply(closest_segs, st_length)),
                           bearing = bearings2try)

    #CALC TOTAL LENGTHS BY ADDING UP OPPOSITE SEGMENTS
    result_df$total.length =
      rep((result_df$length[1:ttl_bearings] +
             result_df$length[(ttl_bearings+1):nrow(result_df)]),
          times = 2)

    result_df$max_length = max(result_df$total.length) #FIND MAX LENGTH
    result_df$is_max =
      ifelse(result_df$total.length ==
               result_df$max_length, TRUE, FALSE) #DETERMINE WHICH SEGs ARE IN MAX SEG PAIR

    #CONVERT SEGS FROM EARLIER INTO SF OBJECTS
    seg_sfg = lapply(closest_segs, st_sfc)
    seg_sfc = do.call(c, seg_sfg)
    seg_sf = st_sf(geometry = seg_sfc)

    result_df = cbind(result_df, seg_sf) #COMBINE SEGS WITH RESULTS TABLE FOR GRAPHING

    fetch_pal = colorFactor(c("#777677", "#5b0013"),
                domain = c(TRUE, FALSE))

    leafletProxy("click2calc_lake") %>%
      clearGroup("fetch_lines") %>%
      addPolylines(data = result_df$geometry,
                   group = "fetch_lines",
                   stroke = T, 
                   opacity = 1, 
                   color = fetch_pal(result_df$is_max),
                   weight = 3,
                   label = lapply(paste0("Segment length: ", round(result_df$length, 2), "m<br>",
                                       "Bearing length: ", round(result_df$total.length, 2), "m<br>",
                                       "Longest bearing? ", result_df$is_max), HTML)
                   )

    output$report_pin_fetch = renderText({

      paste0("Fetch for this location: ",
             round(result_df$max_length[1], 2), " meters")

    })

    pin_map_waiter$hide()

})

# Enter lat-longs to calculate fetch subtab -------------------------------

  ##DRAW INITIAL DT TABLE
  output$enter2calc_table = renderDT({
    
    datatable(enter2calc_df(),
              selection = 'multiple', 
              colnames = c("Latitude", "Longitude", "Lake DOW #"),
              options = list(info = FALSE,
                             ordering = FALSE,
                             searching = FALSE,
                             paging = FALSE, 
                             lengthChange = FALSE))
    
  })

  ##OBSERVER FOR ADDING NEW USER-SUBMITTED PTS TO THE TABLE 
  observeEvent(input$submit_entered_pt, {

    if(isTruthy(input$enter2calc_lat) &&
       isTruthy(input$enter2calc_lng)) { #VALS ARE REAL
      
      pt = st_as_sf(data.frame(lng = input$enter2calc_lng, 
                               lat = input$enter2calc_lat), 
                    coords = c("lng", "lat"),
                    crs = 4326)

    this_lake = lengths(st_intersects(lake_polys[lake_polys$DOW == input$fetch_lake_choice,], pt)) > 0
    
    if(this_lake == FALSE) {
      
      output$invalid_pt_warn = renderText({
        
        "The coordinates you entered do not fall within the lake you've selected. Please check your entries and try again."
        
      })
      
    } else {
      
      output$invalid_pt_warn = renderText({})
    
    enter2calc_df(bind_rows(data.frame(lat = input$enter2calc_lat, 
                             lng = input$enter2calc_lng, 
                             lake = input$fetch_lake_choice), 
                  enter2calc_df()))
      
     }
    } else {
     
      output$invalid_pt_warn = renderText({
        
        "You've entered invalid coordinates (they do not fall within Minnesota). Please check your entries and try again."
        
      })
      
   }
    
  })
  
  #ONCE A NEW PT IS ADDED, REFRESH THE TABLE
  observeEvent(enter2calc_df(), {
    
    dataTableProxy("enter2calc_table") %>% 
      replaceData(enter2calc_df())
    
  })
  
  #REMOVE SELECTED ROWS UPON REQUEST
  observeEvent(input$remove_selected, {
    
    if(isTruthy(input$enter2calc_table_rows_selected)) {
      enter2calc_df(enter2calc_df()[-input$enter2calc_table_rows_selected, ])
    }
    
    dataTableProxy("enter2calc_table") %>% 
      replaceData(enter2calc_df())
    
  })
  
  ##OBSERVER WATCHING CALC FETCH BUTTON
  observeEvent(input$enter2calc_calc, {
    
    if(nrow(enter2calc_df()) > 0) {
      
      #180 SHOULD BE DIVISIBLE BY NUM BEARINGS.
      if(180 %% input$enter_num_bearings == 0) {
        
        entire_enter2calc_waiter$show()

        #CACHE BEARINGS AND DEGREE STEP LENGTHS 
        ttl_bearings = input$enter_num_bearings
        step_size = 180/ttl_bearings
        
        bearings2try = seq(-180, 180, by = step_size)
        bearings2try = bearings2try[-length(bearings2try)]
        
        pt_df = enter2calc_df()
        
        pt_df$uniq_id = seq(1, nrow(pt_df), 1)
        
        #EXPAND THE TABLE ACCORDING TO THE NUMBER OF BEARINGS VIA WITCHCRAFT
        pt_df2 = pt_df %>% 
          rowwise() %>% 
          mutate(id = row_number()) %>% 
          ungroup() %>% 
          slice(rep(row_number(), each = length(bearings2try))) %>% 
          select(-id)
        
        pt_df2$bearing = rep(bearings2try, nrow(pt_df))
        
        #CREATE POINTS IN THE DIRECTIONS OF ALL THE PROPER BEARINGS
        bearing_pts_aslist = lapply(1:nrow(pt_df2), function(x) {
          calculate_bearing_point(start_point = c(pt_df2$lng[x], pt_df2$lat[x]), 
                                  bearing = pt_df2$bearing[x],
                                  distance = 50000 #meters
          )
        })
        
        bearing_pts_asmat = do.call(rbind, bearing_pts_aslist)
        colnames(bearing_pts_asmat) = c("bearing_lng", "bearing_lat")
        
        pt_df3 = cbind(pt_df2, bearing_pts_asmat)

        current.lake.polys = lake_polys %>% filter(DOW %in% unique(pt_df$lake))
        current.lake.polys = st_transform(current.lake.polys, crs = 4326)
        
        #CONVERT PT, BEARING PT PAIRS INTO SF, MAKE LINE STRING BETWEEN, SEPARATED BY ANY INTERSECTIONS W/ POLYGON
        all_segs = lapply(1:nrow(pt_df3), function(x) {
          matrix(c(pt_df3$lng[x], pt_df3$lat[x], pt_df3$bearing_lng[x], pt_df3$bearing_lat[x]), nrow=2, byrow=2) %>%
            st_linestring() %>%
            st_sfc(crs = 4326) %>% #THE RAW DATA NO LONGER HAVE A CRS W/O THIS LINE
            st_intersection(current.lake.polys[current.lake.polys$DOW == pt_df3$lake[x], ]) %>%
            st_cast("LINESTRING")
        })
        
        #FIND LINE STRING SEGMENT CLOSEST TO OUR PT (NOT SORTED, SO TEDIOUS)
        closest_segs = lapply(1:nrow(pt_df3), function(x) {
          segs = all_segs[[x]]
          
          segs[unlist(
            st_intersects(
              st_buffer(
                st_as_sf(pt_df3[x,], 
                         coords = c("lng", "lat"),
                         crs = 4326),
                1),
              segs))[1]]
        })
        
        pt_df3$length = unlist(lapply(closest_segs, st_length))
        
        #CALC TOTAL LENGTHS BY ADDING UP OPPOSITE SEGMENTS
        pt_df3$total.length = NA
        pt_df3$max_length = NA 
        pt_df3$is_max = NA
        
        for(i in 1:nrow(pt_df)) {
        this_pt = pt_df$uniq_id[i]
        
        pt_df3$total.length[pt_df3$uniq_id == this_pt] = 
          rep(
            pt_df3$length[pt_df3$uniq_id == this_pt][1:ttl_bearings] +
          pt_df3$length[pt_df3$uniq_id == this_pt][(ttl_bearings+1):length(
            pt_df3$length[pt_df3$uniq_id == this_pt]
          )],
          2)
        
        pt_df3$max_length[pt_df3$uniq_id == this_pt] = 
          max(pt_df3$total.length[pt_df3$uniq_id == this_pt]) #FIND MAX LENGTH
        
        pt_df3$is_max[pt_df3$uniq_id == this_pt] = 
          ifelse(pt_df3$total.length[pt_df3$uniq_id == this_pt] == 
                   pt_df3$max_length[pt_df3$uniq_id == this_pt], TRUE, FALSE) #DETERMINE WHICH SEGs ARE IN MAX SEG PAIR
        
        }
        
        pt_df3$bearing_lat = round(pt_df3$bearing_lat, 4)
        pt_df3$bearing_lng = round(pt_df3$bearing_lng, 4)
        pt_df3$length = round(pt_df3$length, 1)
        pt_df3$total.length = round(pt_df3$total.length, 1)
        pt_df3$max_length = round(pt_df3$max_length, 2)
        
      
        output$calced_entered_df = renderDT({
          
          pt_df3 %>% rename(
            `Point latitude` = lat,
            `Point longitude` = lng,
            `Lake DOW` = lake,
            `Bearing angle` = bearing,
            `Bearing point latitude` = bearing_lat,
            `Bearing point longitude` = bearing_lng,
            `Segment length (m)` = length,
            `Segment + anti-segment length (m)` = total.length,
            `Point's fetch (m)` = max_length,
            `Fetch segment?` = is_max
          ) %>% 
            select(-uniq_id) %>% 
            datatable() %>% 
            formatStyle(target = "row", 
                        columns = "Fetch segment?",
                        backgroundColor = styleEqual(
                          levels = pt_df3$is_max, 
                          values = ifelse(pt_df3$is_max == TRUE, "darkred", "white")
                        ),
                        color = styleEqual(
                          levels = pt_df3$is_max, 
                          values = ifelse(pt_df3$is_max == TRUE, "white", "black")
                        ),
                        fontWeight = styleEqual(
                          levels = pt_df3$is_max, 
                          values = ifelse(pt_df3$is_max == TRUE, "bold", "normal")
                        ))
          
        })
      
      entire_enter2calc_waiter$hide()
      
      }
    
     }
  })
  

# Submit template to calculate fetch subtab -------------------------------

  output$fetch_template = 
    downloadHandler(filename = "lakeEffects_fetch_template.csv", 
                    function(file) {
    
        write.csv(data.frame(lat = c(45.2538, 46.1899),
                   lng = c(-93.4213, -93.0698), 
                   lake = c("02034200", "01000100")), file, row.names = FALSE)          
                    
  })
  
  ##OBSERVER FOR VALIDATING INPUTTED DATA
  observeEvent(input$submit_template_CSV, {
    
    req(isTruthy(input$submit_template_CSV))

    df = read.csv(input$submit_template_CSV$datapath)
    
    names_check = all(names(df) == c("lat", "lng", "lake"))
    
    if(names_check == TRUE) {
      
      df$lake[nchar(df$lake) == 7] = paste0("0", df$lake[nchar(df$lake) == 7]) #REPAIR DOWS
      
      these_lakes = lake_polys[lake_polys$DOW %in% unique(df$lake),]
      these_lakes = st_transform(these_lakes, crs = 4326)
      
      df = df[-c(2L, 5L, 8L, 17L, 20L, 22L, 33L, 35L, 36L, 40L, 42L, 43L, 44L, 
                     47L, 49L, 62L, 71L, 72L, 79L, 82L, 99L, 101L, 105L, 106L, 107L, 
                     108L, 109L, 113L, 114L, 119L, 127L, 129L, 131L, 132L, 134L, 138L, 
                     143L, 146L), ] #**TEST!!
      
      lake_pt_inter_check = unlist(lapply(1:nrow(df), function(x) {
        
        lengths(st_intersects(these_lakes[these_lakes$DOW == df$lake[x], ], 
                      st_as_sf(df[x, ], coords = c("lng", "lat"), crs = 4326))) > 0
        
      }))
      
      if(any(lake_pt_inter_check == FALSE)) {
        
        allow_fetch_processing(FALSE)
        
      } else {
        
        allow_fetch_processing(TRUE)
        
      }
      
    } else {
      
      allow_fetch_processing(TRUE)
      
    }
    
  })
  
  observeEvent(input$template_calc_fetch, {
    
    browser()
    
    if(allow_fetch_processing() == TRUE) {
      
      df = read.csv(input$submit_template_CSV$datapath)
      df$lake[nchar(df$lake) == 7] = paste0("0", df$lake[nchar(df$lake) == 7]) #REPAIR DOWS
      current.lake.polys = lake_polys[lake_polys$DOW %in% unique(df$lake),]
      current.lake.polys = st_transform(current.lake.polys, crs = 4326)
      
      if(nrow(df) > 0) { ##THIS ONE NEEDS TO HAPPEN SOONER.
        
        #180 SHOULD BE DIVISIBLE BY NUM BEARINGS.
        if(180 %% input$template_num_bearings == 0) {
          
          entire_template2calc_waiter$show()
          
          #CACHE BEARINGS AND DEGREE STEP LENGTHS 
          ttl_bearings = input$template_num_bearings
          step_size = 180/ttl_bearings
          bearings2try = seq(-180, 180, by = step_size)
          bearings2try = bearings2try[-length(bearings2try)]
          
          pt_df = df
          
          pt_df$uniq_id = seq(1, nrow(pt_df), 1)
          
          #EXPAND THE TABLE ACCORDING TO THE NUMBER OF BEARINGS VIA WITCHCRAFT
          pt_df2 = pt_df %>% 
            rowwise() %>% 
            mutate(id = row_number()) %>% 
            ungroup() %>% 
            slice(rep(row_number(), each = length(bearings2try))) %>% 
            select(-id)
          
          pt_df2$bearing = rep(bearings2try, nrow(pt_df))
          
          #CREATE POINTS IN THE DIRECTIONS OF ALL THE PROPER BEARINGS
          bearing_pts_aslist = lapply(1:nrow(pt_df2), function(x) {
            calculate_bearing_point(start_point = c(pt_df2$lng[x], pt_df2$lat[x]), 
                                    bearing = pt_df2$bearing[x],
                                    distance = 50000 #meters
            )
          })
          
          bearing_pts_asmat = do.call(rbind, bearing_pts_aslist)
          colnames(bearing_pts_asmat) = c("bearing_lng", "bearing_lat")
          
          pt_df3 = cbind(pt_df2, bearing_pts_asmat)
          
          #CONVERT PT, BEARING PT PAIRS INTO SF, MAKE LINE STRING BETWEEN, SEPARATED BY ANY INTERSECTIONS W/ POLYGON
          all_segs = lapply(1:nrow(pt_df3), function(x) {
            matrix(c(pt_df3$lng[x], pt_df3$lat[x], pt_df3$bearing_lng[x], pt_df3$bearing_lat[x]), nrow=2, byrow=2) %>%
              st_linestring() %>%
              st_sfc(crs = 4326) %>% #THE RAW DATA NO LONGER HAVE A CRS W/O THIS LINE
              st_intersection(current.lake.polys[current.lake.polys$DOW == pt_df3$lake[x], ]) %>%
              st_cast("LINESTRING")
          })
          
          #FIND LINE STRING SEGMENT CLOSEST TO OUR PT (NOT SORTED, SO TEDIOUS)
          closest_segs = lapply(1:nrow(pt_df3), function(x) {
            segs = all_segs[[x]]
            
            segs[unlist(
              st_intersects(
                st_buffer(
                  st_as_sf(pt_df3[x,], 
                           coords = c("lng", "lat"),
                           crs = 4326),
                  1),
                segs))[1]]
          })
          
          pt_df3$length = unlist(lapply(closest_segs, st_length))
          
          #CALC TOTAL LENGTHS BY ADDING UP OPPOSITE SEGMENTS
          pt_df3$total.length = NA
          pt_df3$max_length = NA 
          pt_df3$is_max = NA
          
          for(i in 1:nrow(pt_df)) {
            this_pt = pt_df$uniq_id[i]
            
            pt_df3$total.length[pt_df3$uniq_id == this_pt] = 
              rep(
                pt_df3$length[pt_df3$uniq_id == this_pt][1:ttl_bearings] +
                  pt_df3$length[pt_df3$uniq_id == this_pt][(ttl_bearings+1):length(
                    pt_df3$length[pt_df3$uniq_id == this_pt]
                  )],
                2)
            
            pt_df3$max_length[pt_df3$uniq_id == this_pt] = 
              max(pt_df3$total.length[pt_df3$uniq_id == this_pt]) #FIND MAX LENGTH
            
            pt_df3$is_max[pt_df3$uniq_id == this_pt] = 
              ifelse(pt_df3$total.length[pt_df3$uniq_id == this_pt] == 
                       pt_df3$max_length[pt_df3$uniq_id == this_pt], TRUE, FALSE) #DETERMINE WHICH SEGs ARE IN MAX SEG PAIR
            
          }
          
          pt_df3$bearing_lat = round(pt_df3$bearing_lat, 4)
          pt_df3$bearing_lng = round(pt_df3$bearing_lng, 4)
          pt_df3$length = round(pt_df3$length, 1)
          pt_df3$total.length = round(pt_df3$total.length, 1)
          pt_df3$max_length = round(pt_df3$max_length, 2)
          
          output$calced_template_df = renderDT({
            
            pt_df3 %>% rename(
              `Point latitude` = lat,
              `Point longitude` = lng,
              `Lake DOW` = lake,
              `Bearing angle` = bearing,
              `Bearing point latitude` = bearing_lat,
              `Bearing point longitude` = bearing_lng,
              `Segment length (m)` = length,
              `Segment + anti-segment length (m)` = total.length,
              `Point's fetch (m)` = max_length,
              `Fetch segment?` = is_max
            ) %>% 
              select(-uniq_id) %>% 
              datatable() %>% 
              formatStyle(target = "row", 
                          columns = "Fetch segment?",
                          backgroundColor = styleEqual(
                            levels = pt_df3$is_max, 
                            values = ifelse(pt_df3$is_max == TRUE, "darkred", "white")
                          ),
                          color = styleEqual(
                            levels = pt_df3$is_max, 
                            values = ifelse(pt_df3$is_max == TRUE, "white", "black")
                          ),
                          fontWeight = styleEqual(
                            levels = pt_df3$is_max, 
                            values = ifelse(pt_df3$is_max == TRUE, "bold", "normal")
                          ))
            
          })
          
          entire_template2calc_waiter$hide()
          
          output$template_outputs = 
            downloadHandler(filename = "lakeEffects_fetch_outputs.csv", 
                            function(file) {
                              
                              write.csv(pt_df3, file, row.names = FALSE)          
                              
                            })
          
          
        }
        
      }
      
    } else {
      
      
      
    }
    
  })
  
}