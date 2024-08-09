ui = fluidPage(id = "whole_page",
  

# Head content and hooplah ------------------------------------------------

  ##LINK TO STYLES AND BEHAVIORS FILES
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet"),
    tags$script(src = "behaviors.js")
  ),
  
  useWaiter(), #TURN ON WAITER PACKAGE FEATURES
  
 #CREATE CUSTOM DISCONNECT MESSAGE.
  disconnectMessage(text = "Hmm...something has gone haywire! Either you have been idle for too long and the app has timed out (this occurs after about 10 minutes), or an error has occurred in our R code, triggering a shutdown. To proceed, refresh the page. If, after doing so, you encounter this message again after taking the same actions as before, please file a bug report with Alex at bajcz003@umn.edu. Thanks!", 
                    refresh = "Refresh the page", 
                    width = "full", 
                    top = "center", 
                    size = 22, 
                    background = "#7a0019", 
                    colour = "white", 
                    refreshColour = "#ffb71e", 
                    css = "z-index: 100000 !important;"),
  

# Actual content ----------------------------------------------------------

#DIVIDE PAGE INTO TWO PANELS
  div(id = "both_panels",
    
      #SIDEBAR PANEL WILL HOLD METHOD/SUBMETHOD INPUT WIDGETS
    div(id = "sidebar",
        #START WITH AN INDICATOR POINTING USERS TO START HERE.
        div(id = "startup-header", 
            h2("👇     Start here     👇"),
            br()),
        #FIRST INPUT WIDGET--WHAT "EFFECT" DOES THE USER WANT?
      selectInput("pick_effect",
                  "Pick your effect--what type of spatial operation do you want to perform?", 
                  choices = c("--Select from below--", "Calculate fetch"), 
                  selected = "--Select from below--", 
                  size = 2,
                  selectize = FALSE,
                  width = "100%"),
      uiOutput("methods")
    ),
    
    #MAIN PANEL BEGINS WITH LOGOS, VERSION INFO, FUNDING INFO, ETC.
    div(id = "main-panel",
      div(id = "startup_main",
      tags$img(src = "logo1resized.png", alt = "The LakeEffects app logo", class = "logo_pics"),
      h2("A Shiny app for geospatial operations on Minnesota lakes", class = "logo_display"),
      br(),
      h4("Powered by the Minnesota Aquatic Invasive Species Research Center at the University of Minnesota", class = "logo_display"),
      img(src = "jointlogoresized.png", class = "logo_pics"),
      br(),
      br(),
      HTML("<i><h5>App Version 0.0.6. Last updated 8/9/24. Developed by Dr. Alex Bajcz, Quantitative Ecologist for MAISRC. Funding for this work was provided by the Minnesota Environment and Natural Resources Trust Fund as recommended by the Minnesota Aquatic Invasive Species Research Center (MAISRC) and the Legislative-Citizen Commission on Minnesota Resources (LCCMR) and also the State of Minnesota.</i></h5>")
      )
      
    )
    
  )
  
)