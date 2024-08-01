server = function(input, output, session) {
  
  #REACTIVES
  pin_df = reactiveVal(lake_polys[1,])
  pin_map_waiter = Waiter$new(id = "pin_map_area", 
                              hide_on_render = T, 
                              html = spin_loaders(7, color = "lightblue"), 
                              color = "darkred", 
                              fadeout = TRUE)
  enter_pt_waiter = Waiter$new(id = "enter_pt_table_area", 
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
  

# Click to calculate fetch subtab -----------------------------------------

  
  ##BASELINE LAKE SEARCHER
  output$pin_lake_find = renderUI({ 
    
    choices_done = as.character(lake_polys$DOW[1])
    pretty = paste0(lake_polys$map_label[1], " (DOW: ", lake_polys$DOW[1], ")")
    names(choices_done) = pretty
    
    selectInput("pin_lake_choice", 
                "A sample lake is shown below. Start typing above to select a different lake.", 
                choices = choices_done,
                selectize = TRUE)
    
    })
  
  ##LAKE SEARCHER FOR CLICK2CALC MAP
  observeEvent(input$pin_lake_search, {

    if(nchar(input$pin_lake_search) > 2) {
      
        choices = lake_polys %>% 
          st_drop_geometry() %>% 
          select(DOW, map_label) %>% 
          filter(grepl(input$pin_lake_search, DOW, ignore.case = T) |
                   grepl(input$pin_lake_search, map_label, ignore.case = T)) %>% 
          head(200) %>% 
          select(DOW, map_label)
        
        if(nrow(choices) > 0) {
        
        pretty = unlist(lapply(1:nrow(choices), function(x) {
          
          paste0(choices$map_label[x], " (DOW: ", choices$DOW[x], ")")
          
        }))
        
        choices_done = pull(choices, DOW)
        names(choices_done) = pretty
        
        updateSelectInput(session, 
                          inputId = "pin_lake_choice",
                          label = "Select a lake.", 
                          choices = choices_done)
      
        }
    }
    
  })
  
  ##OBSERVER WATCHING PIN LAKE SELECTOR
observeEvent(input$pin_lake_choice, {

    tmp1 = lake_polys %>% 
    filter(DOW == input$pin_lake_choice)
    
    pin_df(tmp1)
    
    bounds = unname(st_bbox(tmp1))
  
    leafletProxy("click2calc_lake") %>% 
      clearGroup("lake_polygon") %>% 
      addPolygons(data = tmp1$geometry[1], 
                  stroke = TRUE, 
                  group = "lake_polygon",
                  color = "darkblue", 
                  weight = 4, 
                  opacity = 0.9, 
                  fill = T, 
                  fillColor = "lightblue",
                  fillOpacity = 0.7,
                  label = HTML(paste0("Lake: ", tmp1$map_label[1],
                                      "<br>", "DOW: ", tmp1$DOW[1]))) %>% 
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    
  })
  
  
  ##CLICK2CALC BASE MAP
  output$click2calc_lake = renderLeaflet({
    
    leaflet(options = tileOptions(maxZoom = 20, minZoom = 6)) %>% 
      addTiles() %>% 
      addPolygons(data = lake_polys$geometry[1], 
                  stroke = TRUE, 
                  group = "lake_polygon",
                  color = "darkblue", 
                  weight = 4, 
                  opacity = 0.9, 
                  fill = T, 
                  fillColor = "lightblue",
                  fillOpacity = 0.7,
                  label = HTML(paste0("Lake: ", lake_polys$map_label[1],
                                 "<br>", "DOW: ", lake_polys$DOW[1])))
    
  })
  
  ##CLICK TO DROP PIN ON CLICK2CALC
  observeEvent(input$click2calc_lake_click, {

    df = data.frame(lat = input$click2calc_lake_click$lat, 
               lng = input$click2calc_lake_click$lng)
    
    sfdf = st_as_sf(df,
                    coords = c("lng", "lat"),
                    crs = 4326)
    
    sfdf_T = st_transform(sfdf, crs = 32615)
    lake_T = st_transform(pin_df()[1,], crs = 32615)
    
    if(lengths(st_intersects(lake_T, sfdf_T)) > 0) {

    leafletProxy("click2calc_lake") %>% 
      clearGroup("fetch_lines") %>% 
      clearMarkers() %>% 
      addMarkers(data = sfdf)
    
    output$dropped_pin_coords = renderText({
      
      paste0("Lat. of dropped pin: ", round(input$click2calc_lake_click$lat, 4), "<br>",
             "Long. of dropped pin: ", round(input$click2calc_lake_click$lng, 4))
      
    })
    
    } else {
      
      output$dropped_pin_coords = renderText({
        
        paste0("You clicked outside of the lake's polygon! Don't do that.")
        
      })
      
    }
    
  })
  
  #CALC FETCH FOR PIN DROP
  observeEvent(input$pin_calc, {
    
    #NEEDS TO BE A VALID MAP MARKER.
    if(isTruthy(input$click2calc_lake_click)) {
    
    #180 SHOULD BE DIVISIBLE BY NUM BEARINGS.
    if(180 %% input$pin_num_bearings == 0) {
      
    pin_map_waiter$show()
      
    output$pin_num_bear_warn = renderText({})

    #CACHE BEARINGS AND DEGREE STEP LENGTHS 
    ttl_bearings = input$pin_num_bearings
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
    
    current.lake.poly = pin_df()[1, ] #GRAB LAKE POLY
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
    
    fetch_pal = colorFactor(c("gray50", "darkred"), 
                domain = c(TRUE, FALSE))
    
    leafletProxy("click2calc_lake") %>% 
      clearGroup("fetch_lines") %>% 
      addPolylines(data = result_df$geometry, 
                   group = "fetch_lines",
                   stroke = T,
                   color = fetch_pal(result_df$is_max), 
                   weight = 3.5, 
                   label = lapply(paste0("Segment length: ", round(result_df$length, 2), "m<br>",
                                       "Bearing length: ", round(result_df$total.length, 2), "m<br>",
                                       "Longest bearing? ", result_df$is_max), HTML)
                   )
    
    output$report_pin_fetch = renderText({
      
      paste0("Fetch for this location: ", 
             round(result_df$max_length[1], 2), " meters")
      
    })
    
    pin_map_waiter$hide()
    
    } else {
      
      output$pin_num_bear_warn = renderText({ "180 must be divisible by the number of bearings you want to try (valid options: 1, 2, 3, 4, 5, 6, 9, 10, 12, 15, 18, 20, 30, 36, 45, 60, 90, and 180)"})
      
    }
      
    } else {
      
      output$pin_num_bear_warn = renderText({ "You need to click a location in the lake's polygon to calculate fetch!" })
      
    }

    
  })
  

# Enter lat-longs to calculate fetch subtab -------------------------------

  ##DRAW INITIAL DT TABLE
  output$enter2calc_table = renderDT({
    
    enter2calc_df()
    
  })
  
  ##BASELINE LAKE SEARCHER FOR ENTER2CALC SUBTAB
  output$enter_lake_find = renderUI({ 
    
    choices_done = as.character(lake_polys$DOW[1])
    pretty = paste0(lake_polys$map_label[1], " (DOW: ", lake_polys$DOW[1], ")")
    names(choices_done) = pretty
    
    selectInput("enter_lake_choice", 
                "A sample lake is shown below. Start typing above to select a different lake.", 
                choices = choices_done,
                selectize = TRUE)
    
  })
  
  ##LAKE SEARCHER FOR ENTER2CALC SUBTAB 
  observeEvent(input$enter_lake_search, {
    
    if(nchar(input$enter_lake_search) > 2) {
      
      choices = lake_polys %>% 
        st_drop_geometry() %>% 
        select(DOW, map_label) %>% 
        filter(grepl(input$enter_lake_search, DOW, ignore.case = T) |
                 grepl(input$enter_lake_search, map_label, ignore.case = T)) %>% 
        head(200) %>% 
        select(DOW, map_label)
      
      if(nrow(choices) > 0) {
      
      pretty = unlist(lapply(1:nrow(choices), function(x) {
        
        paste0(choices$map_label[x], " (DOW: ", choices$DOW[x], ")")
        
      }))
      
      choices_done = pull(choices, DOW)
      names(choices_done) = pretty
      
      updateSelectInput(session, 
                        inputId = "enter_lake_choice",
                        label = "Select a lake.", 
                        choices = choices_done)
      
      }
    }
    
  })
  
  ##OBSERVER FOR ADDING NEW USER-SUBMITTED PTS TO THE TABLE 
  observeEvent(input$submit_entered_pt, {
    
    if(isTruthy(input$enter2calc_lat) &&
       isTruthy(input$enter2calc_lng)) { #VALS ARE REAL
      
      enter_pt_waiter$show()
      
      pt = st_as_sf(data.frame(lng = input$enter2calc_lng, 
                               lat = input$enter2calc_lat), 
                    coords = c("lng", "lat"),
                    crs = 4326)

    this_lake = lengths(st_intersects(lake_polys[lake_polys$DOW == input$enter_lake_choice,], pt)) > 0
    
    if(this_lake == FALSE) {
      
      output$invalid_pt_warn = renderText({
        
        "The coordinates you entered do not fall within the lake you've selected. Please check your entries and try again."
        
      })
      
    } else {
      
      output$invalid_pt_warn = renderText({})
    
    enter2calc_df(bind_rows(data.frame(lat = input$enter2calc_lat, 
                             lng = input$enter2calc_lng, 
                             lake = input$enter_lake_choice), 
                  enter2calc_df()))
      
     }
    } else {
     
      output$invalid_pt_warn = renderText({
        
        "You've entered invalid coordinates (they do not fall within Minnesota). Please check your entries and try again."
        
      })
      
   }
    
    enter_pt_waiter$hide()
    
  })
  
  #ONCE A NEW PT IS ADDED, REFRESH THE TABLE
  observeEvent(enter2calc_df(), {
    
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