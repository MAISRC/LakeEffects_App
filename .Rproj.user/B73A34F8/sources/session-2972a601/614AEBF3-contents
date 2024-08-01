ui = fluidPage(
  
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet")
  ),
  
  useWaiter(),
  
  sidebarLayout(
    
    sidebarPanel(
      h2("Start here!"),
      selectInput("pick_effect",
                  "Pick your effect", 
                  choices = c("No selection", "Calculate fetch"), 
                  selected = "No selection")
    ),
    
    mainPanel(
      tags$img(src = "logo1.png", alt = "The LakeEffects app logo"),
      h3("A Shiny app for geospatial operations on Minnesota lakes"),
      h4("Powered by the Minnesota Aquatic Invasive Species Research Center at the University of Minnesota"),
      p("App Version 0.0.0")
      
    )
    
  )
  
)