ui = fluidPage(id = "whole_page",
  

# Head content and hooplah ------------------------------------------------

  
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet"),
    tags$script(src = "behaviors.js")
  ),
  
  useWaiter(),
  
  disconnectMessage(text = "Hmm...something has gone wrong. Either you have been idle for too long and the app has timed out or an error has been triggered in the R code of the application. To try again, refresh the page. If, after doing so, you encounter this page again after taking the same actions as before, please file a bug report with Alex at bajcz003@umn.edu. We appreciate your cooperation!", 
                    refresh = "Refresh the page", 
                    width = "full", 
                    top = "center", 
                    size = 22, 
                    background = "#7a0019", 
                    colour = "white", 
                    refreshColour = "#ffb71e", 
                    css = "z-index: 100000 !important;"),
  

# Actual content ----------------------------------------------------------

  
  
  div(id = "both_panels",
    
    div(id = "sidebar",
        div(id = "startup-header", 
            h2("👇     Start here     👇"),
            br()),
      selectInput("pick_effect",
                  "Pick your effect--what type of spatial operation do you want to perform?", 
                  choices = c("--Select from below--", "Calculate fetch"), 
                  selected = "--Select from below--", 
                  size = 2,
                  selectize = FALSE,
                  width = "100%"),
      uiOutput("methods")
    ),
    
    div(id = "main-panel",
      div(id = "startup_main",
      tags$img(src = "logo1resized.png", alt = "The LakeEffects app logo", class = "logo_pics"),
      h2("A Shiny app for geospatial operations on Minnesota lakes", class = "logo_display"),
      br(),
      h4("Powered by the Minnesota Aquatic Invasive Species Research Center at the University of Minnesota", class = "logo_display"),
      img(src = "jointlogoresized.png", class = "logo_pics"),
      br(),
      br(),
      HTML("<i><h5>App Version 0.0.5. Last updated 8/6/24. Developed by Dr. Alex Bajcz, Quantitative Ecologist for MAISRC. Funding for this work was provided by the Minnesota Environment and Natural Resources Trust Fund as recommended by the Minnesota Aquatic Invasive Species Research Center (MAISRC) and the Legislative-Citizen Commission on Minnesota Resources (LCCMR) and also the State of Minnesota.</i></h5>")
      )
      
    )
    
  )
  
)