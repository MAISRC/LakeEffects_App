server = function(input, output, session) {
  

# REACTIVES, TRACKERS, AND FLAGS ------------------------------------------

  most_recent_bearing_num = reactiveVal(314)
  most_recent_lake_choice = reactiveVal("00000000")
  first_map_fourth = reactiveVal(TRUE)
  first_enter_fourth = reactiveVal(TRUE)
  any_non0_bearing = reactiveVal()
  first_logo_move = reactiveVal(TRUE)
  first_move2submit = reactiveVal(TRUE)
  prev_enter_df_nrows = reactiveVal()
  enter2calc_df = reactiveVal(data.frame(lat = numeric(0), 
                                         lng = numeric(0), 
                                         lake = character(0)))
  allow_fetch_processing = reactiveVal(FALSE)
  search_debounce = debounce(reactive({input$fetch_lake_search}),
                            750)
  
  ##BOOT UP THE FETCH_FOURTH WAITER
  pin_map_waiter = Waiter$new(id = "fetch_fourth", 
                              hide_on_render = T, 
                              html = spin_loaders(7, color = "lightblue"), 
                              color = "darkred", 
                              fadeout = TRUE)


# START-UP RENDERING ------------------------------------------------------

  #RENDER THE SUBMETHOD SELECTOR AS SOON AS USERS PICK A METHOD
output$methods = renderUI({
  
  if(input$pick_effect != "--Select from below--") {
    
    removeUI("#startup-header") #GET RID OF THE FIRST ACTION POINTERS
    
    #POPULATE FETCH SUBMETHODS IF METHOD == FETCH
    if(input$pick_effect == "Calculate fetch") {
      
      selectInput("fetch_method", 
                  label = "How'd you like to calculate fetch?", 
                  choices = c("--Select from below--", "By clicking a map", "By entering coordinates", "By uploading a CSV file"), 
                  selected = "--Select from below--",
                  selectize = F,
                  size = 4,
                  width = "100%")
      
    }
    
  } else {
    #OTHERWISE, FLUSH ALL FETCH-SUBMETHOD ENTITIES
    removeUI("#fetch_first"); removeUI("#fetch_second"); removeUI("#fetch_third"); removeUI("#fetch_fourth")
    NULL #MUST RETURN "SOMETHING".
  }
  
})


# Observer -- FETCH SUBMETHOD SELECTOR ------------------------------------
observeEvent(input$fetch_method, {
  
  #NO MATTER WHAT, IF MESSING WITH METHODS, FLUSH THE SUBSEQUENT SUBMETHOD OUTPUTS.
  removeUI("#fetch_first"); removeUI("#fetch_second")
  removeUI("#fetch_third"); removeUI("#fetch_fourth")
  #...AND TOGGLE RELEVANT TRACKERS/FLAGS BACK TO THEIR STARTING STATES
  enter2calc_df = reactiveVal(data.frame(lat = numeric(0), 
                                         lng = numeric(0), 
                                         lake = character(0)))
  first_enter_fourth(TRUE)
  first_map_fourth(TRUE)
  first_move2submit(TRUE)

  ##IF THEY'VE PICKED A SUBMETHOD...
  if(isTruthy(input$fetch_method) &&
     input$fetch_method != "--Select from below--") {

    ##WHICH? ONE OF THE FIRST TWO?
    if(input$fetch_method == "By clicking a map" |
       input$fetch_method == "By entering coordinates") {

      ##INSERT FIRST SIDEBAR WIDGET--THE LAKE SEARCH
      insertUI(selector = "#sidebar", 
               where = "beforeEnd",
               ui = div(id = "fetch_first",
                        textInput("fetch_lake_search", 
                                  div(HTML("Type (part of) a lake name or unique identifying number (DOW) in the box below to search for a specific lake. <a href = 'https://www.dnr.state.mn.us/lakefind/index.html' target = '_blank'>Use the Minnesota DNR's LakeFinder tool for assistance, if needed.</a>")), 
                                  value = "")
                        )
               )
      session$sendCustomMessage(type = 'scrollSidebar', message = list()) #SCROLL TO THE BOTTOM AS NEEDED.
      
    } else {
     
     #ELSE INSERT THE OTHER SUBMETHOD FIRST WIDGET, WHICH IS A DOWNLOAD BUTTON AND A GOT IT BUTTON.
      insertUI(selector = "#sidebar", 
               where = "beforeEnd",
               ui = div(id = "fetch_first",
                        p("If you haven't already, download a template CSV file below to use this sub-method. Hit the \"Got it!\" button to proceed", class = "control-label"),
                        downloadButton("fetch_template",
                                       label = "Download"),
                        br(),
                        actionButton("fetch_first_proceed", "Got it!")
               )
      )
      session$sendCustomMessage(type = 'scrollSidebar', message = list()) #Scroll to bottom of sidebar as needed.
      
    }
  }
  
})


# Observer -- FETCH LAKE SEARCH -------------------------------------------
observeEvent(search_debounce(), 
             priority = 2, { #WE NEED THIS TO RUN BEFORE THE BEARINGS CHOICE OBSERVER BECAUSE WE COULD PROCEED STRAIGHT FROM THIS TO FETCH_FOURTH IN THOSE INSTANCES AND THE OTHER OBSERVER AND THIS ONE WOULD RACE.

  #FLUSH/RESET SOME SUBSEQUENT OUTPUTS REGARDLESS, MOSTLY TO DO WITH RESULTS AND THE MAP SUBMETHOD.
  most_recent_bearing_num("--Select from below--")
  output$dropped_pin_coords = renderText({})
  output$report_pin_fetch = renderText({})
  removeUI("#calced_df_div")
  most_recent_lake_choice("00000000") #***THERE'S A CHANCE THIS IS TOO CONDITIONLESS AND IT NEEDS TO OCCUR ONLY WHEN THE CONDITION BELOW IS NOT TRUE.
               
  #DID USER SEARCH FOR A VALID STRING > 2 CHARACTERS?
  if(isTruthy(input$fetch_lake_search) && 
     nchar(input$fetch_lake_search) > 0) {
    
    #IF SO, FLUSH ALL REMAINING SUBSEQUENT OUTPUTS, CONDITIONALLY FOR THE ENTER SUBMETHOD
    removeUI("#fetch_second"); removeUI("#fetch_third")
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "By entering coordinates") {
      removeUI("#fetch_fourth")
    }
    
    
    #SEARCH THE LAKE POLYGON FILE FOR THAT DOW/LAKE NAME AND FILTER.
    choices = lake_polys %>% 
      st_drop_geometry() %>% 
      select(DOW, map_label) %>% 
      filter(grepl(input$fetch_lake_search, DOW, ignore.case = T) |
               grepl(input$fetch_lake_search, map_label, ignore.case = T)) %>% 
      head(200) #CURTAIL RESULTS TO THE FIRST THIS MANY.
    
    #DID THE SEARCH RETURN RESULTS?
    if(nrow(choices) > 0) {
      
      #UNIFY LAKE NAMES AND DOW #S TOGETHER INTO PRETTY, HUMAN-READABLE STRINGS OF NAME (DOW) FORMAT
      pretty = unlist(lapply(1:nrow(choices), function(x) {
        paste0(choices$map_label[x], " (DOW: ", choices$DOW[x], ")")
      }))
      
      choices_done = pull(choices, DOW) #GET JUST DOWS
      names(choices_done) = pretty #MAKE THE PRETTY STRINGS THE NAMES.
      choices_done = c("--Select from below--" =
                         "--Select from below--", 
                       choices_done) #ADD THE "NON-CHOICE" CHOICE.
      
      #IF ONLY A SINGLE RESULT RETURNS, WE AUTO-SELECT IT.
      selected2use = "--Select from below--"
      if(nrow(choices) == 1) {
        selected2use = choices_done[2]
      }
      
      #INSERT NEXT UI, WHICH IS THE LAKE CHOICE SELECTOR.
      insertUI(selector = "#sidebar", 
               where = "beforeEnd",
               ui = div(id = "fetch_second", 
                        selectInput(inputId = "fetch_lake_choice",
                                    label = "Select a specific lake from the drop-down menu below to proceed. Only the first 200 results are shown.", 
                                    selectize = FALSE,
                                    selected = selected2use,
                                    size = 6,
                                    choices = choices_done)
               ))
      session$sendCustomMessage(type = 'scrollSidebar', message = list()) #SCROLL DOWN AS NEEDED.
      
    } else {
      #IF NO RESULTS RETURN, INTRODUCE A DEAD-END FETCH_SECOND WITH A HEADS-UP. 
      insertUI(selector = "#sidebar", where = "beforeEnd",
               ui = div(id = "fetch_second", 
                        p(id = "no_results_info",
                      "Your search yielded no results. Try searching a different string of characters above.")))
      session$sendCustomMessage(type = 'scrollSidebar', 
                                message = list()) #SCROLL DOWN AS NEEDED.
    }
  } 

})



# Observer -- FETCH LAKE SELECTION ----------------------------------------
observeEvent(input$fetch_lake_choice, priority = 2, {
 
  removeUI("#calced_df_div") #THIS THIS OUTPUT NO MATTER WHAT

  if(isTruthy(input$fetch_lake_choice) &&
     input$fetch_lake_choice == "--Select from below--") { #ONLY FLUSH IF THEY'VE GONE THIS FAR BACKWARD, WHICH SHOULD BE TREATED LIKE STARTING OVER.
    removeUI("#fetch_third")
    removeUI("#fetch_fourth")
    any_non0_bearing(NULL) #LIKE STARTING OVER.
  } else {

    #IF THEY HAVE ALREADY SELECTED A BEARING NUMBER, CARRY THAT THRU FOR THE FIRST TWO SUB-METHODS
      if(input$fetch_method != "By uploading a CSV file" &&
         isTruthy(any_non0_bearing())) {
        selected2use = any_non0_bearing() 
      } else {
        selected2use = "--Select from below--"
      }

    #INSERT THE NEXT INPUT WIDGET (BEARING NUMBER)
    insertUI(selector = "#sidebar", 
             where = "beforeEnd",
             ui = div(id = "fetch_third", 
                      selectInput("fetch_num_bearings", 
                                  "To calculate fetch, the app will draw lines outward from your point in a number of specific directions (bearings) until it hits land. Select a number of bearings using the drop-down menu below. A higher number will mean more accurate results, but it will also take longer to calculate--we recommend using 36 or so.",
                                  selectize = FALSE,
                                  size = 6,
                                   choices = c("--Select from below--", "180", "90", "60", "45", "36", "30", "20", "18", "15", "12", "10", "9", "6", "5", "4"), 
                                  selected = selected2use)
             ))
    session$sendCustomMessage(type = 'scrollSidebar', message = list()) 
      
  }
  
})

  

# Observer -- FETCH PROCEED -----------------------------------------------
observeEvent(input$fetch_first_proceed, {
  
  #FLUSH SOME (POTENTIAL) SUBSEQUENT OUTPUTS
  removeUI("#fetch_third"); removeUI("#fetch_fourth")
  removeUI("#calced_df_div")
  
  #IF THEY'VE HIT THE BUTTON AT ALL...MOVE ON.
  if(isTruthy(input$fetch_first_proceed) &&
      !is.null(input$fetch_first_proceed) &&
       input$fetch_first_proceed > 0) {
    
    #BRING UP THE SAME BEARINGS WIDGET AS ABOVE.
    insertUI(selector = "#sidebar", 
             where = "beforeEnd",
             ui = div(id = "fetch_third", 
                      selectInput("fetch_num_bearings", 
                                  "To calculate fetch, the app will draw lines outward from your point in a number of specific directions (bearings) until it hits land. Select a number of bearings using the drop-down menu below. A higher number will mean more accurate results, but it will also take longer to calculate--we recommend using 36 or so.",
                                  selectize = FALSE,
                                  size = 6,
                                  choices = c("--Select from below--", "180", "90", "60", "45", "36", "30", "20", "18", "15", "12", "10", "9", "6", "5", "4"), 
                                  selected = "--Select from below--")
             ))
    session$sendCustomMessage(type = 'scrollSidebar', message = list()) 
  }
  
  # most_recent_bearing_num("--Select from below--") #RESET TRACKER NOW THAT NEW CHOICES ARE AVAILABLE.
  first_move2submit(TRUE) #THIS IS NEEDED TO ENSURE THAT HITTING THE BUTTON AFTER PICKING THIS SUBMETHOD AND AFTER BRINGING UP FETCH_FOURTH ONCE BUT BEFORE RESELECTING A BEARING NUMBER WILL STILL EVENTUALLY BRING UP FETCH_FOURTH (SPECIFIC CIRCUMSTANCES!)
  
})



# Observer -- RESPOND TO A BEARING NUMBER SELECTION -----------------------
  ##PROBABLY THE MOST COMPLEX OBSERVER OF THEM ALL, SO BE CAREFUL EDITING IT.
observeEvent(list(input$fetch_lake_choice,
                  input$fetch_num_bearings), {
                    
          #         browser()

  #FLUSH SOME POTENTIAL SUBSEQUENT OUTPUTS
  removeUI("#calced_df_div")
  removeUI("#submitted_results_div")

  #ON MAP SUBMETHOD, REMOVE RESULTS IF THEY CHANGE EITHER LAKE OR BEARING NUMBER.
  if(isTruthy(input$fetch_method) &&
     input$fetch_method == "By clicking a map") {
     output$report_pin_fetch = renderText({})
  }
                    
  #HAS A BEARING NUMBER BEEN CHOSEN?
  if(isTruthy(input$fetch_num_bearings) &&
     input$fetch_num_bearings != "--Select from below--") {
    

    ## Largely map submethod stuff ---------------------------------------------

    ##THESE TWO CHECKS WILL ONLY FIRE IN SEQUENCE TO HAVE ANY EFFECT WHEN A USER GETS TO FETCH_FOURTH, THEN SELECTS NO LAKE, THEN SELECTS A LAKE AGAIN. THEY ENSURE FETCH_FOURTH CAN BE REMADE ASSUMING ALL OTHER REQUIREMENTS ARE MET.
    if(input$fetch_method == "By clicking a map" &&
       most_recent_lake_choice() == "--Select from below--") { first_map_fourth(TRUE) }
    if(input$fetch_method == "By clicking a map" &&
       first_map_fourth() == TRUE) { most_recent_bearing_num("--Select from below--") }
    
    if(isTruthy(input$fetch_method) &&
       input$fetch_method == "By clicking a map" &&
       most_recent_bearing_num() == "--Select from below--" &&
       input$fetch_lake_choice != "--Select from below--" &&
       first_map_fourth() == TRUE) {

      #OTHERWISE, WE NEED TO BUILD FETCH_FOURTH
        if(first_logo_move() == TRUE) {
          removeUI("#startup_main") #REMOVE STARTUP LOGOS TEXT
          insertUI(selector = ("#sidebar"), where = "afterBegin", 
                   ui = tags$img(src = "logo1resized.png",
                                 alt = "The LakeEffects app logo",
                                 class = "logo_pics"))
          session$sendCustomMessage(type = 'scrollSidebar', message = list())
          first_logo_move(FALSE) #NO LONGER FIRST TIME. 
        }

        #INSERT FETCH_FOURTH UI.
     insertUI(selector = "#main-panel",
               where = "beforeEnd",
               ui = div(id = "fetch_fourth",
                        p("Click a location within your chosen lake's polygon (blue line) below to calculate fetch for that location.", id = "pin_instructions"),
                        leafletOutput("click2calc_lake", width = "500px"),
                        htmlOutput("dropped_pin_coords")
               ))

     #DRAW THE PIN-DROPPING MAP AS PART OF THAT UI.
      output$click2calc_lake = renderLeaflet({
        
        #FILTER LAKE POLYS TO ONLY THIS LAKE
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
      
      first_map_fourth(FALSE)

    }

    # Largely enter points submethod stuff ---------------------------------------------

    ##IF WE'RE ON THE ENTER POINTS SUBMETHOD AND WE HAVE CHOSEN A LAKE AND BEARING NUMBER FOR THE FIRST TIME, WE MUST BUILD FETCH_FOURTH
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "--Select from below--" &&
       input$fetch_method == "By entering coordinates" &&
       input$fetch_lake_choice != "--Select from below--" &&
       input$fetch_num_bearings != "--Select from below--" &&
       first_enter_fourth() == TRUE) { 
      
      #LARGELY AS ABOVE.
      if(first_logo_move() == TRUE) {
        removeUI("#startup_main")
        insertUI(selector = ("#sidebar"), where = "afterBegin", 
                 ui = tags$img(src = "logo1resized.png",
                               alt = "The LakeEffects app logo",
                               class = "logo_pics"))
        session$sendCustomMessage(type = 'scrollSidebar', message = list())
        first_logo_move(FALSE)
      }
      first_enter_fourth(FALSE)
      
      #FILTER TO JUST CURRENT LAKE, AS ABOVE
      tmp1 = lake_polys %>% 
        filter(DOW == input$fetch_lake_choice)
      bounds = round(unname(st_bbox(tmp1)), 4)
      
      #INSERT FETCH FOURTH UI, W/ NUMERIC WIDGETS AND BASIC DT TABLE, PLUS REMOVE PT BUTTON.
      insertUI(selector = "#main-panel",
               where = "beforeEnd",
               ui = div(id = "fetch_fourth",
                        p(id = "enter_pt_info", "For this sub-method, you will first enter points at which to calculate fetch by selecting latitude and longitude values for a given point using the selectors below, then adding that point to your list of points using the button below. If you add a point to your list by mistake, select that point in your list and hit the \"remove\" button to remove it."),
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
                                     "Add point to list"),
                        htmlOutput("invalid_pt_warn"),
                        br(),
                        dataTableOutput("enter2calc_table"),
                        actionButton("remove_selected", 
                                     "Remove selected rows")
                        ))
      
    } 
    
    #IF USER HAS SELECTED A DIFFERENT LAKE AND FETCH_FOURTH IS ALREADY UP, MAKE SURE TO GIVE THEM RELEVANT VALUES FOR THE NUMERIC WIDGETS.
    if(first_enter_fourth() == FALSE) {
      
      tmp1 = lake_polys %>% 
        filter(DOW == input$fetch_lake_choice)
      bounds = round(unname(st_bbox(tmp1)), 4)
      
      updateNumericInput(session, 
                   "enter2calc_lat", 
                   min = bounds[2], 
                   value = round(mean(c(bounds[4],bounds[2])), 4),
                   max = bounds[4])
      updateNumericInput(session, 
                  "enter2calc_lng", 
                   min = bounds[1], 
                   value = round(mean(c(bounds[1],bounds[3])), 4),
                   max = bounds[3])
      
    }
    
    #IF A USER ADDS POINTS, MOVES THE BEARINGS SELECTOR TO NO SELECTION, THEN RESTORES IT TO A VALUE, WE HAVE TO PUT THE GO BUTTON BACK (SPECIFIC CIRCUMSTANCES)
      if(isTruthy(input$fetch_method) &&
         isTruthy(input$fetch_num_bearings) &&
         input$fetch_method == "By entering coordinates" &&
         input$fetch_num_bearings != "--Select from below--" &&
         nrow(enter2calc_df()) > 0) {
        removeUI("#enter2calc_godiv") #YOU CAN ALSO END UP HERE WITH 1 GO BUTTON ALREADY, SO ENSURE WE STAY AT 1 THEN.
        insertUI(selector = "#fetch_fourth", 
                 ui = div(id = "enter2calc_godiv",
                          actionButton("enter2calc_go", "Calculate fetch!")))
      }
      
    # Largely submit template points submethod stuff ---------------------------------------------
    
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "--Select from below--" &&
       input$fetch_method == "By uploading a CSV file") {
      
      removeUI("#submitted_results_div") #WIPE RESULTS, IF ANY, IF THEY ARE SCREWING AROUND WITH THE NUMBER OF BEARINGS. 
      
      #IF FIRST FETCH FOURTH, REMOVE STARTING LOGO CONTENT.
      if(first_logo_move() == TRUE) {
        removeUI("#startup_main")
      insertUI(selector = ("#sidebar"), where = "afterBegin", 
               ui = tags$img(src = "logo1resized.png",
                             alt = "The LakeEffects app logo",
                             class = "logo_pics"))
      session$sendCustomMessage(type = 'scrollSidebar', message = list())
      first_logo_move(FALSE)
      }

      #INSERT FETCH_FOURTH DIV
      if(first_move2submit() == TRUE) {
      insertUI(selector = "#main-panel", 
               where = "beforeEnd",
               ui = div(id = "fetch_fourth", 
                        fileInput(inputId = "template_submit", 
                                  "Upload your file of points at which to calculate fetch using this file selector. Make sure your file is a .csv file and that it contains only columns called \"lat\", \"lng\", \"lake\", just as in the template file.",
                                   accept = ".csv")
               ))
        first_move2submit(FALSE)
      }
    }
    
  } else {
    
    #IF A USER HAS ADVANCED PAST BEARING # ON THE CSV OR MAP SUBMETHODS, THEN RETURNS TO NO BEARING #, THAT'S WEIRD BEHAVIOR! IT SHOULD BE SORTA LIKE STARTING OVER.
    if(isTruthy(input$fetch_method) &&
       input$fetch_method != "By uploading a CSV file") {
      removeUI("#fetch_fourth")
      first_move2submit(TRUE)
      first_map_fourth(TRUE)
      enter2calc_df = reactiveVal(data.frame(lat = numeric(0), 
                                             lng = numeric(0), 
                                             lake = character(0)))
    }
    
  }
  
  #UPDATE TRACKERS
  most_recent_bearing_num(input$fetch_num_bearings)
  most_recent_lake_choice(input$fetch_lake_choice)
  
  #FOR MEMORY, LET'S REMEMBER ANY NON-ZERO BEARING NUMBER THEY HAVE ALREADY CHOSEN.
  if(isTruthy(input$fetch_method) &&
     isTruthy(input$fetch_num_bearings) &&
     input$fetch_method != "By uploading a CSV file" && 
     input$fetch_num_bearings != "--Select from below--") {
    any_non0_bearing(input$fetch_num_bearings)
  }
  
})


# Observer -- DROP PIN FOR FETCH ------------------------------------------

observeEvent(input$click2calc_lake_click, {

  req(input$click2calc_lake_click) #PREVENTS ERRORS ON STARTUP
  
  #REMOVE ANY SUBSEQUENT OUTPUTS THAT MAY BE AROUND
  removeUI("#pin_instructions")
  removeUI("#fetch_action")
  output$dropped_pin_coords = renderText({})
  output$report_pin_fetch = renderText({})

  #TURN CLICKED LOCATION INTO SPATIAL POINT.
  df = data.frame(lat = input$click2calc_lake_click$lat,
             lng = input$click2calc_lake_click$lng)
  sfdf = st_as_sf(df,
                  coords = c("lng", "lat"),
                  crs = 4326)

  #GET CURRENT LAKE, TRANSFORM ALL TO A UTM PROJECTION
  tmp1 = lake_polys %>% 
    filter(DOW == input$fetch_lake_choice)
  sfdf_T = st_transform(sfdf, crs = 32615)
  lake_T = st_transform(tmp1[1,], crs = 32615)

  #SO LONG AS PT INTERSECTS LAKE...
  if(lengths(st_intersects(lake_T, sfdf_T)) > 0) {

    #DRAW PIN ON MAP
  leafletProxy("click2calc_lake") %>%
    clearGroup("fetch_lines") %>%
    clearMarkers() %>%
    addMarkers(data = sfdf)

    #RENDER ITS COORDS
  output$dropped_pin_coords = renderText({

    paste0("Pin latitude: ", round(input$click2calc_lake_click$lat, 4), "<br>",
           "Pin longitude: ", round(input$click2calc_lake_click$lng, 4))

  })

  #AND INSERT THE NEXT UI (GO BUTTON)
  insertUI("#fetch_fourth", 
           ui = div(id = "fetch_action", 
                    actionButton("calc_fetch_button", 
                             "Calculate fetch!"),
                    htmlOutput("report_pin_fetch")))

  } else {

    #OTHERWISE, GIVE WARNING
    output$dropped_pin_coords = renderText({
      
      leafletProxy("click2calc_lake") %>%
        clearGroup("fetch_lines") %>%
        clearMarkers()

      paste0("You clicked outside of the lake's polygon! Click within the polygon to proceed.")

    })

  }

})


# Observer -- CALC FETCH (MAP) --------------------------------------------

observeEvent(input$calc_fetch_button, {

    pin_map_waiter$show() #TURN ON WAITER

    #CACHE BEARINGS AND TURN ANGLE
    ttl_bearings = as.numeric(input$fetch_num_bearings)
    step_size = 180/ttl_bearings

    #CREATE SEQUENCE OF TURN ANGLE/BEARINGS TO TRY
    bearings2try = seq(-180, 180, by = step_size)
    bearings2try = bearings2try[-length(bearings2try)] #THIS ONE WOULD BE A DUPLICATE, SO REMOVE.

    #CREATE POINTS WAY FAR OUT IN THE DIRECTIONS OF ALL THE PROPER BEARINGS
    bearing_pts_aslist = lapply(bearings2try, function(x) {
      calculate_bearing_point(start_point = 
                                c(input$click2calc_lake_click$lng,
                                input$click2calc_lake_click$lat),
                              bearing = x,
                              distance = 50000 #meters
                              )
    })
    bearing_pts_asmat = do.call(rbind, bearing_pts_aslist) #CONVERT FROM LIST TO MATRIX
    colnames(bearing_pts_asmat)[1] = "lng" #MATCH SF/LEAFLET NAMING EXPECTATIONS

    #STASH USER-PROVIDED POINT IN A DF, ADD PROCEDURAL COLS TO IT
    pt_df = data.frame(
      lng = input$click2calc_lake_click$lng,
      lat = input$click2calc_lake_click$lat
    )

    pt_bearing_pairs = apply(bearing_pts_asmat, 1, rbind, pt_df) #BIND CURRENT PT. TO EACH POSSIBLE BEARING PT 1:1.
    
    #GET CURRENT LAKE POLY, TRANSFORM TO LAT/LONG
    tmp1 = lake_polys %>% 
      filter(DOW == input$fetch_lake_choice)
    current.lake.poly = tmp1[1, ]
    current.lake.poly = st_transform(current.lake.poly, crs = 4326)

    #CONVERT PT, BEARING PT PAIRS INTO SF OBJECTS, MAKE LINE STRING BETWEEN, SEPARATED BY ANY INTERSECTIONS W/ LAKE POLYGON
    all_segs = lapply(pt_bearing_pairs, function(x) {
      as.matrix(x) %>%
        st_linestring() %>%
        st_sfc(crs = 4326) %>% #THE RAW DATA NO LONGER HAVE A CRS W/O THIS LINE
        st_intersection(current.lake.poly) %>%
        st_cast("LINESTRING")
    })

    #FIND LINE STRING SEGMENT CLOSEST TO OUR PT
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

    #CREATE STORAGE OBJ FOR RESULTS
    result_df = data.frame(length = unlist(lapply(closest_segs, st_length)),
                           bearing = bearings2try)

    #CALC TOTAL FETCH LENGTHS BY ADDING UP OPPOSING SEGMENTS
    result_df$total.length =
      rep((result_df$length[1:ttl_bearings] +
             result_df$length[(ttl_bearings+1):nrow(result_df)]),
          times = 2)

    #FIND MAX FETCH LENGTH
    result_df$max_length = max(result_df$total.length) 
    #DETERMINE WHICH 2 SEGs ARE IN MAX SEG PAIRING
    result_df$is_max =
      ifelse(result_df$total.length ==
               result_df$max_length, TRUE, FALSE) 

    #CONVERT SEGS FROM EARLIER INTO SF OBJECTS
    seg_sfg = lapply(closest_segs, st_sfc)
    seg_sfc = do.call(c, seg_sfg)
    seg_sf = st_sf(geometry = seg_sfc)

    result_df = cbind(result_df, seg_sf) #COMBINE SEGS WITH RESULTS TABLE FOR GRAPHING

    #CREATE PALETTE FOR SEGMENTS
    fetch_pal = colorFactor(c("#777677", "#5b0013"),
                domain = c(TRUE, FALSE))

    #MAKE THE BEARING ANGLES AND DIRECTIONS HUMAN READABLE.
    result_df = result_df %>% 
      rowwise() %>% 
      mutate(dir = bearing_funct(bearing)) %>% 
      mutate(bearing_dir = paste0(bearing, "° (", dir, ")", collapse = "")) %>% 
      ungroup()

    #GRAPH ALL SEGMENTS, WITH CUSTOM HOVERS, AND COLOR CODING.
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
                                       "Bearing angle (dir): ", result_df$bearing_dir, "<br>",
                                       "Longest bearing? ", result_df$is_max), HTML)
                   )

    #REPORT RESULTS FOR MAX FETCH SEGMENT.
    output$report_pin_fetch = renderText({

      paste0("<span id = 'pin_result_info'>The fetch data you requested are below.</span><br>",
        "Fetch for this location: ",
             round(result_df$max_length[1], 2), " meters<br>",
             "Fetch bearing angles (dirs): ", paste0(result_df$bearing_dir[result_df$is_max == TRUE], collapse = ", "))

    })

    pin_map_waiter$hide() #HIDE WAITER

})

  # Draw INITIAL DT table for added fetch points (2nd submethod) --------------------
  output$enter2calc_table = renderDT({

    datatable(data.frame(lat = numeric(0), 
                         lng = numeric(0), 
                         lake = character(0)),
              selection = 'multiple', 
              colnames = c("Latitude", "Longitude", "Lake DOW #"),
              options = list(info = FALSE,
                             ordering = FALSE,
                             searching = FALSE,
                             paging = FALSE, 
                             lengthChange = FALSE))
    
})

  # Observer -- Add new points to fetch DT table (2nd submethod) ------------
  observeEvent(input$submit_entered_pt, {

    if(isTruthy(input$enter2calc_lat) &&
       isTruthy(input$enter2calc_lng)) { #VALS ARE REAL

      #TURN PT INTO SPATIAL OBJ
      pt = st_as_sf(data.frame(lng = input$enter2calc_lng, 
                               lat = input$enter2calc_lat), 
                    coords = c("lng", "lat"),
                    crs = 4326)

      #CONFIRM IT INTERSECTS SELECTED LAKE.
    this_lake = lengths(st_intersects(lake_polys[lake_polys$DOW == input$fetch_lake_choice,], pt)) > 0
    
    #WARN IF NOT
    if(this_lake == FALSE) {
      output$invalid_pt_warn = renderText({
        "The coordinates you entered do not fall within the lake you've selected. Please check your entries and try again."
      })
    } else {
      
      #ELSE, REMOVE WARN..
      output$invalid_pt_warn = renderText({})
    
    #UPDATE THE DF TRACKER, WHICH'LL AUTO-TRIGGER THE OBSERVER BELOW.
    enter2calc_df(bind_rows(data.frame(lat = input$enter2calc_lat, 
                             lng = input$enter2calc_lng, 
                             lake = input$fetch_lake_choice), 
                  enter2calc_df()))
    
    prev_enter_df_nrows(nrow(enter2calc_df())) #UPDATE THIS FLAG W/ NEW ROW #
      
     }
    } 
    
  })
  

# Observer -- Refresh table when new pt is added --------------------------
  observeEvent(enter2calc_df(), {
    
    removeUI("#calced_df_div") #REMOVE ANY RESULTS
    
    dataTableProxy("enter2calc_table") %>% 
      replaceData(enter2calc_df())

    #IF A USER STARTS W/ > 1 ROW AND ENDS WITH == 1 BY DELETING ROWS, WE'D END UP WITH TWO GO BUTTONS. THIS CHECK PREVENTS
    dupe_flag = isTruthy(prev_enter_df_nrows()) && 
      prev_enter_df_nrows() > 1 &&
      nrow(enter2calc_df()) == 1
    
    #PUT IN THE GO BUTTON SO USER CAN RUN THE FETCH CALCULATION
    if(nrow(enter2calc_df()) == 1 &&
       dupe_flag == FALSE) {

      insertUI(selector = "#fetch_fourth", 
               ui = div(id = "enter2calc_godiv",
                        actionButton("enter2calc_go", "Calculate fetch!")))
      
    } else {
      
      if(nrow(enter2calc_df()) == 0) { #MAKE SURE IT GOES AWAY IF THERE'S NOTHING TO SUBMIT.
      removeUI("#enter2calc_godiv")
      }
      
    }
    
  })
  

# Observer -- Remove pts from DT fetch table (2nd sub-method) -------------

  observeEvent(input$remove_selected, {
    
    if(isTruthy(input$enter2calc_table_rows_selected)) { #IF SELECTED ROWS...
      
      prev_enter_df_nrows(nrow(enter2calc_df())) #STASH THE NUMBER OF ROWS THE DF USED TO HOLD IN A FLAG. SEE NOTE ABOVE FOR WHY.
      
      #REMOVE ROWS
      enter2calc_df(enter2calc_df()[-input$enter2calc_table_rows_selected, ])
    
      #UPDATE TABLE
    dataTableProxy("enter2calc_table") %>% 
      replaceData(enter2calc_df())
    
    }
  })
  

# Observer -- CALC FETCH (ENTER PTS SUBMETHOD) -------------------------------
  observeEvent(input$enter2calc_go, {

        pin_map_waiter$show() #TURN ON WAITER

        #SEE ANNOTATIONS IN CALC FETCH OBSERVER
        ttl_bearings = as.numeric(input$fetch_num_bearings)
        step_size = 180/ttl_bearings
        bearings2try = seq(-180, 180, by = step_size)
        bearings2try = bearings2try[-length(bearings2try)]
        
        #GRAB USER-ENTERED TABLE OF PT DATA
        pt_df = enter2calc_df()
        #GIVE ALL PTS UNIQ ID
        pt_df$uniq_id = seq(1, nrow(pt_df), 1)
        
        #EXPAND THE TABLE ACCORDING TO THE NUMBER OF BEARINGS TO TRY VIA CHATGPT WITCHCRAFT
        pt_df2 = pt_df %>% 
          rowwise() %>% 
          mutate(id = row_number()) %>% 
          ungroup() %>% 
          slice(rep(row_number(), each = length(bearings2try))) %>% 
          select(-id)
        
        pt_df2$bearing = rep(bearings2try, nrow(pt_df))
        
        #SEE ANNOTATIONS ELSEWHERE
        bearing_pts_aslist = lapply(1:nrow(pt_df2), function(x) {
          calculate_bearing_point(start_point = c(pt_df2$lng[x], pt_df2$lat[x]), 
                                  bearing = pt_df2$bearing[x],
                                  distance = 50000 #meters
          )
        })
        bearing_pts_asmat = do.call(rbind, bearing_pts_aslist)
        colnames(bearing_pts_asmat) = c("bearing_lng", "bearing_lat")
        pt_df3 = cbind(pt_df2, bearing_pts_asmat)
        #MUST GET ALL RELEVANT LAKES
        current.lake.polys = lake_polys %>% filter(DOW %in% unique(pt_df$lake))
        current.lake.polys = st_transform(current.lake.polys, crs = 4326)
        
        #CONVERT PT, BEARING PT PAIRS INTO SF, MAKE LINE STRING BETWEEN, SEPARATED BY ANY INTERSECTIONS W/ RELEVANT POLYGON
        all_segs = lapply(1:nrow(pt_df3), function(x) {
          matrix(c(pt_df3$lng[x], pt_df3$lat[x], pt_df3$bearing_lng[x], pt_df3$bearing_lat[x]), nrow=2, byrow=2) %>%
            st_linestring() %>%
            st_sfc(crs = 4326) %>%
            #DO ALL THIS LAKE BY LAKE IN THIS CASE
            st_intersection(current.lake.polys[current.lake.polys$DOW == pt_df3$lake[x], ]) %>%
            st_cast("LINESTRING")
        })
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
        #FOR EACH PT, GENERATE ALL ITS RESULTS DATA
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
          2) #ADD LENGTHS OF OPPOSING SEGMENTS USING LAGGED INDICES
        
        pt_df3$max_length[pt_df3$uniq_id == this_pt] = 
          max(pt_df3$total.length[pt_df3$uniq_id == this_pt]) #FIND MAX LENGTH
        
        pt_df3$is_max[pt_df3$uniq_id == this_pt] = 
          ifelse(pt_df3$total.length[pt_df3$uniq_id == this_pt] == 
                   pt_df3$max_length[pt_df3$uniq_id == this_pt], TRUE, FALSE) #DETERMINE WHICH 2 SEGs ARE IN MAX SEG PAIR
        }
        
        #ROUND OFF ALL RESULTS
        pt_df3$bearing_lat = round(pt_df3$bearing_lat, 4)
        pt_df3$bearing_lng = round(pt_df3$bearing_lng, 4)
        pt_df3$length = round(pt_df3$length, 1)
        pt_df3$total.length = round(pt_df3$total.length, 1)
        pt_df3$max_length = round(pt_df3$max_length, 2)
        
        #MAKE RESULTS HUMAN READABLE AND ADD BEARING DIRECTION INFO
        pt_df3 = pt_df3 %>% 
          rowwise() %>% 
          mutate(dir = bearing_funct(bearing)) %>% 
          mutate(bearing_dir = paste0(bearing, "° (", dir, ")", collapse = "")) %>% 
          ungroup()
        
        #INSERT RESULTS UI
        insertUI(selector = "#fetch_fourth", 
                 ui = div(id = "calced_df_div",
                          p(id = "calced_entered_info", 
                            "The fetch data you requested are below."),
                          dataTableOutput("calced_entered_df"),
                          downloadButton("enter_outputs", 
                                         label = "Download results!")))
 
        #MANIPULATE RESULTS FOR CONCISE PRESENTATION (THROWS OUT A LOT OF METADATA THAT COULD INSTEAD BE RETAINED SOMEHOW)
       pt_df4 = pt_df3 %>% 
          select(-uniq_id, -bearing_lat, -bearing_lng, 
                 -length, -total.length, -dir, -bearing) %>% 
          filter(is_max == TRUE) %>% 
          select(-is_max) %>% 
          distinct(lat, lng, lake, bearing_dir, max_length) %>% 
          group_by(lat, lng, lake) %>% 
          mutate(bearing_dirs = paste0(bearing_dir, collapse = ",<br>")) %>% 
          ungroup() %>% 
          distinct(lat, lng, lake, bearing_dirs, max_length) %>% 
          rename(
            `Point<br>latitude` = lat,
            `Point<br>longitude` = lng,
            `Lake<br>DOW #` = lake,
            `Bearing<br>angles (dirs)` = bearing_dirs,
            `Point's<br>fetch (m)` = max_length,
          )
        
       #RENDER RESULTS TABLE 
        output$calced_entered_df = renderDT({
          
          pt_df4 %>% 
            datatable(escape = FALSE, 
                      selection = "none",
                      options = list(info = FALSE,
                                     ordering = FALSE,
                                     searching = FALSE,
                                     paging = FALSE, 
                                     lengthChange = FALSE))
          
        })
        
        #RENDER DOWNLOADABLE VERSION OF RESULTS
        output$enter_outputs = 
          downloadHandler(filename = "lakeEffects_fetch_outputs.csv", 
                          function(file) {
                            
                            write.csv(pt_df4, file, row.names = FALSE)          
                            
                          })
        
        pin_map_waiter$hide() #HIDE WAITER
    
  })
  


# Download handler for fetch template CSV file ----------------------------

  output$fetch_template = 
    downloadHandler(filename = "lakeEffects_fetch_template.csv", 
                    function(file) {
    
        write.csv(data.frame(lat = c(45.2538, 46.1899),
                   lng = c(-93.4213, -93.0698), 
                   lake = c("02034200", "01000100")), file, row.names = FALSE)          
  })
  

# Observer -- Validate submitted fetch csv file --------------------------

  observeEvent(input$template_submit, {
    
    removeUI("#submitted_results_div") #REMOVE ANY RESULTS
    removeUI("#submitted_DT_div")
    
    #FILE MUST EXIST AND BE A CSV FILE
    req(input$template_submit)
    req(grepl(".csv", input$template_submit$datapath))

    df = read.csv(input$template_submit$datapath) #READ AS CSV
    
    rows_check = nrow(df) > 0 #ENSURE > 0 ROWS.

    names_check = all(names(df) == c("lat", "lng", "lake")) #CONFIRM CONTAINS ONLY EXPECTED COLS
    
    if(names_check == TRUE && rows_check == TRUE) { #IF YES, PROCEED.
      
      df$lake[nchar(df$lake) == 7] = 
        paste0("0", df$lake[nchar(df$lake) == 7]) #REPAIR ANY DOWS
      
      #GO GET RELEVANT LAKES.
      these_lakes = lake_polys[lake_polys$DOW %in% unique(df$lake),]
      these_lakes = st_transform(these_lakes, crs = 4326)

      #CHECK THAT ALL PTS INTERCEPT THE RESPECTIVE LAKES INDICATED
      lake_pt_inter_check = unlist(lapply(1:nrow(df), function(x) {
        
        lengths(st_intersects(these_lakes[these_lakes$DOW == df$lake[x], ], 
                      st_as_sf(df[x, ], coords = c("lng", "lat"), crs = 4326))) > 0
        
      }))
      
      #IF ANY FAIL,
      if(any(lake_pt_inter_check == FALSE)) {
        
        #INSERT TABLE OF THE POINTS, BUT INDICATE WHICH FAIL THIS CHECK.
        insertUI(selector = "#fetch_fourth", 
                 where = "beforeEnd",
                 ui = div(id = "submitted_DT_div",
                          p("The data you uploaded are shown below. However, some of the points (those marked in pink) do not lie within their specified lake's polygon. Please correct these points to proceed.", id = "upload_info"),
                          dataTableOutput("submitted_DT")))
        
        #RENDER DEAD-END DT INDICATING FAILURE PTS.
        output$submitted_DT = renderDT({
          
          df$valid_coords = lake_pt_inter_check
          
          datatable(df, 
                    selection = 'none', 
                    escape = FALSE,
                    colnames = c("Latitude", "Longitude", "Lake<br>DOW #", "Valid<br>coordinates?"),
                    options = list(info = FALSE,
                                   ordering = FALSE,
                                   searching = FALSE,
                                   lengthChange = FALSE)) %>% 
            formatStyle(target = "row", 
                        columns = "valid_coords",
                        backgroundColor = styleEqual(
                          levels = df$valid_coords, 
                          values = ifelse(df$valid_coords == FALSE, "pink", "white")
                        ))
          
        })
        
        allow_fetch_processing(FALSE) #DON'T ALLOW.
        
      } else {
        
        #OTHERWISE, INSERT DT WITH SUBMITTED PTS, PLUS GO BUTTON
        insertUI(selector = "#fetch_fourth", 
                 where = "beforeEnd",
                 ui = div(id = "submitted_DT_div",
                          p("The data you uploaded are shown below. If you're satisfied, hit the button below to calculate fetch! Note: This process takes about 1 minute per 15 points submitted for 36 bearings, so you may need to limit the number of points or reduce the numbers of bearings to keep runtimes managable!", id = "upload_info"),
                          dataTableOutput("submitted_DT"),
                          actionButton("template_go", 
                                       "Calculate fetch!")))
        
        output$submitted_DT = renderDT({
          
        datatable(df, 
                  selection = 'none', 
                  colnames = c("Latitude", "Longitude", "Lake DOW #"),
                  options = list(info = FALSE,
                                 ordering = FALSE,
                                 searching = FALSE,
                                 lengthChange = FALSE))
          
        })
        
        allow_fetch_processing(TRUE) #ALLOW
        
      }
      
    } else {
      
     #WARN THAT SUBMITTED DATA ARE TOO SCREWY
      insertUI(selector = "#fetch_fourth", 
               where = "beforeEnd",
               ui = div(id = "submitted_DT_div",
                        p("The data you uploaded are lacking one or more columns the app is looking for! Please consult the template file, available in the sidebar, for details. Alternatively, you accidentally submitted a file with no data in it.", id = "upload_info")))
      
      allow_fetch_processing(FALSE) #DON'T ALLOW.
      
    }
    
  })
  

# Observer -- CALC FETCH (submit template submethod) ----------------------
  observeEvent(input$template_go, {
    
    removeUI("#submitted_results_div") #REMOVE PAST RESULTS

    #SEE ANNOTATIONS ELSEWHERE
      pin_map_waiter$show() 
      pt_df = read.csv(input$template_submit$datapath)
      pt_df$lake[nchar(pt_df$lake) == 7] = paste0("0", pt_df$lake[nchar(pt_df$lake) == 7]) 
      current.lake.polys = lake_polys[lake_polys$DOW %in% unique(pt_df$lake),]
      current.lake.polys = st_transform(current.lake.polys, crs = 4326)
      ttl_bearings = as.numeric(input$fetch_num_bearings)
      step_size = 180/ttl_bearings
      bearings2try = seq(-180, 180, by = step_size)
      bearings2try = bearings2try[-length(bearings2try)]
      pt_df$uniq_id = seq(1, nrow(pt_df), 1)
      pt_df2 = pt_df %>% 
            rowwise() %>% 
            mutate(id = row_number()) %>% 
            ungroup() %>% 
            slice(rep(row_number(), each = length(bearings2try))) %>% 
            select(-id)
      pt_df2$bearing = rep(bearings2try, nrow(pt_df))
      bearing_pts_aslist = lapply(1:nrow(pt_df2), function(x) {
            calculate_bearing_point(start_point = c(pt_df2$lng[x], pt_df2$lat[x]), 
                                    bearing = pt_df2$bearing[x],
                                    distance = 50000
      )
      })
      bearing_pts_asmat = do.call(rbind, bearing_pts_aslist)
      colnames(bearing_pts_asmat) = c("bearing_lng", "bearing_lat")
      pt_df3 = cbind(pt_df2, bearing_pts_asmat)
      all_segs = lapply(1:nrow(pt_df3), function(x) {
            matrix(c(pt_df3$lng[x], pt_df3$lat[x], pt_df3$bearing_lng[x], pt_df3$bearing_lat[x]), nrow=2, byrow=2) %>%
              st_linestring() %>%
              st_sfc(crs = 4326) %>% 
              st_intersection(current.lake.polys[current.lake.polys$DOW == 
                                                   pt_df3$lake[x], ]) %>%
              st_cast("LINESTRING")
      })
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
              max(pt_df3$total.length[pt_df3$uniq_id == this_pt])
          pt_df3$is_max[pt_df3$uniq_id == this_pt] = 
              ifelse(pt_df3$total.length[pt_df3$uniq_id == this_pt] == 
                       pt_df3$max_length[pt_df3$uniq_id == this_pt], TRUE, FALSE)
          }
      pt_df3$bearing_lat = round(pt_df3$bearing_lat, 4)
      pt_df3$bearing_lng = round(pt_df3$bearing_lng, 4)
      pt_df3$length = round(pt_df3$length, 1)
      pt_df3$total.length = round(pt_df3$total.length, 1)
      pt_df3$max_length = round(pt_df3$max_length, 2)
      pt_df3 = pt_df3 %>% 
            rowwise() %>% 
            mutate(dir = bearing_funct(bearing)) %>% 
            mutate(bearing_dir = paste0(bearing, "° (", dir, ")", collapse = "")) %>% 
            ungroup()
          
      #INSERT RESULTS
          insertUI(selector = "#fetch_fourth", 
                   where = "beforeEnd",
                   ui = div(id = "submitted_results_div",
                            p("Your calculated fetch data are below.", id = "upload_info"),
                            dataTableOutput("calced_template_df"),
                            downloadButton("template_outputs", 
                                           label = "Download results!")))
      #MANIPULATE RESULTS FOR CONCISE PRESENTATION
          pt_df4 = pt_df3 %>% 
            select(-uniq_id, -bearing_lat, -bearing_lng, 
                   -length, -total.length, -dir, -bearing) %>% 
            filter(is_max == TRUE) %>% 
            select(-is_max) %>% 
            distinct(lat, lng, lake, bearing_dir, max_length) %>% 
            group_by(lat, lng, lake) %>% 
            mutate(bearing_dirs = paste0(bearing_dir, collapse = ",<br>")) %>% 
            ungroup() %>% 
            distinct(lat, lng, lake, bearing_dirs, max_length) %>% 
            rename(
              `Point<br>latitude` = lat,
              `Point<br>longitude` = lng,
              `Lake<br>DOW` = lake,
              `Bearing<br>angles (dirs)` = bearing_dirs,
              `Point's<br>fetch (m)` = max_length,
            )
          
          #RENDER RESULTS IN DT
          output$calced_template_df = renderDT({
            
            datatable(pt_df4, escape = FALSE, 
                        selection = "none",
                        options = list(info = FALSE,
                                       lengthChange = FALSE))
            
          })
          
          #CONSOLIDATE RESULTS AND FEED TO DOWNLOAD HANDLER
          pt_df5 = pt_df4
          colnames(pt_df5) = gsub("<br>", "", colnames(pt_df4))
          
          output$template_outputs = 
            downloadHandler(filename = "lakeEffects_fetch_outputs.csv", 
                            function(file) {
                              
                              write.csv(pt_df5, file, row.names = FALSE)          
                              
                            })
          
          pin_map_waiter$hide()

    
  })
  
}