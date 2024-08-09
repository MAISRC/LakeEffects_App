ui = fluidPage(
  
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet")
  ),
  
  useWaiter(),
  
  tabsetPanel(id = "effect_tabset", 
              selected = "1",
              type = "pill", 
              
    tabPanel(title = "Fetch calculator",
             value = "1",
             
      tabsetPanel(id = "fetch_tabset",
                  selected = "1",
                  type = "pill",
                  
          tabPanel(title = "Click to calculate", 
                   value = "1",
                   
                   textInput("pin_lake_search", "Start typing a lake name or DOW here to search.", value = ""),
                   uiOutput("pin_lake_find"),
                   div(id = "pin_map_area",
                   leafletOutput("click2calc_lake")),
                   htmlOutput("dropped_pin_coords"),
                   numericInput("pin_num_bearings", "Number of bearings to try.",
                                value = 36,
                                min = 1,
                                max = 180),
                   htmlOutput("pin_num_bear_warn"),
                   actionButton("pin_calc", "Calculate fetch"),
                   htmlOutput("report_pin_fetch")
                   
                   ),
          
          tabPanel(title = "Enter lat/longs to calculate", 
                   value = "2",
                   
                   div(id = "entire_enter2calc", 
                   textInput("enter_lake_search", 
                             "Start typing a lake name or DOW here to search.",
                             value = ""),
                   uiOutput("enter_lake_find"),
                   numericInput("enter2calc_lat", 
                                "Enter latitude value",
                                min = 43.49966, 
                                value = 46.1899,
                                max = 49.38442,
                                step = 0.00001),
                   numericInput("enter2calc_lng", 
                                "Enter longitude value",
                                min = -97.22108, 
                                value = -93.0698,
                                max = -89.48321,
                                step = 0.00001),
                   htmlOutput("invalid_pt_warn"),
                   numericInput("enter_num_bearings", "Number of bearings to try.",
                                value = 36,
                                min = 1,
                                max = 180),
                   actionButton("submit_entered_pt", 
                                "Add point to table"),
                   actionButton("enter2calc_calc", 
                                "Calculate fetches"),
                   div(id = "enter_pt_table_area", 
                   dataTableOutput("enter2calc_table"))
                   ),
                   dataTableOutput("calced_entered_df")
                   
          ),
          
          tabPanel(title = "Submit table to calculate", 
                   value = "3",
                   div(id = "entire_template2calc", 
                       downloadButton("fetch_template",
                                  label = "Download a template CSV file"),
                   numericInput("template_num_bearings", "Number of bearings to try.",
                                value = 36,
                                min = 1,
                                max = 180),
                   fileInput("submit_template_CSV", 
                             label = "Upload a template-like CSV file of point data", 
                             accept = ".csv"),
                   actionButton("template_calc_fetch",
                                "Calculate fetches"),
                   dataTableOutput("calced_template_df"),
                   downloadButton("template_outputs",
                                  "Download a copy of the outputs once they are ready."))
                   
          ),
                  
                  )
             
             ),
    
    tabPanel(title = "Within-lake distances calculator",
             value = "2",
             
             
    ),
              )
  
)