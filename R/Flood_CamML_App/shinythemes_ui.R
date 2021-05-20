


# Porting material shiny ui over to shinydashboard ui


## ui.R ##
library(shinythemes)

# bootstrap shinythemes theme
ui <- fluidPage(
  
  # Themes
  theme = shinytheme("lumen"),
  
  # Application title - color details
  title = "NCDOT FloodCamML", 
  
  
  
  # ## Header Image ----------------------------------
  # # Image in folder 'www' at same level as app.R
  # material_parallax(
  #     # image source: https://obxstuff.com/pages/things-to-do
  #     image_source = "outer_banks_aerial.jpeg"
  # ),
  
  
  ## App Description ----------------------------------
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h2("Outer Banks Nuisance Flooding Detection"),
        p("This is where I imagine an introduction to the project or instructions for the app might go:"),
        h5("Detecting Flood Events with Machine Learning:"),
        p("This application uses live camera feeds from the North Carolina Department of Transportation to and machine-learning 
                  models to determine when NC motorways are experiencing flooding conditions. The windows below
                  will display real-time camera images and the determination (flooding/not flooding) from both
                  a supervised and un-supervised machine learning model.")
      )
    )
  ),
  
  
  # Flooding Description
  fluidRow(
    
    column(
      width = 12,
      # Content Card
      wellPanel(
        h3("What is Nuisance and High Tide Flooding?"),
        p("During extremely high tides, the sea literally spills onto land in some locations, inundating low-lying areas
                   with seawater until high tide has passed. Because this flooding causes public inconveniences such as road closures
                   and overwhelmed storm drains, the events were initially called nuisance flooding.
                   To help people understand the cause of these events, they are now referred to as high-tide floods."),
        a(href = "https://toolkit.climate.gov/topics/coastal-flood-risk/shallow-coastal-flooding-nuisance-flooding",
          "Source")
      )
    )
  ),
  
  
  ####  Set up Tab Pages for Different Content
  tabsetPanel(
    
    ####  Tab 1 Weather Conditions  ####
    tabPanel(
      strong("Current Weather Conditions"),
      style = 'padding-bottom: 20px;',
      
      ####  Fluid Row for contents
      fluidRow(
        column(width = 12,
               h3("Is it the lack of content here?"),
               p("That is making the app break?"))
      )
    ),
    
    
    
    #### Tab 2 - Model 1 Results  ####
    tabPanel(
      strong("Model 1 Results"),
      style = 'padding-bottom: 20px;',
      
      ###_ Row 1 ----------------------------------
      #testing row
      fluidRow(
        
        column(
          width = 6,
          wellPanel(
            strong("Ocracoke"),
            plotOutput("cam1_img"),
            actionButton(inputId = "m1_cam1_flood", label = "Flooding"),
            actionButton(inputId = "m1_cam1_dry", label = "Not-Flooding")
          )
        ),
        
        column(
          width = 6,
          wellPanel(
            strong("Mirlo"),
            plotOutput("cam2_img"),
            actionButton(inputId = "m1_cam2_flood", label = "Flooding"),
            actionButton(inputId = "m1_cam2_dry", label = "Not-Flooding")
          )
        )
      ),
      
      ###_ Row 2 ----------------------------------
      fluidRow(
        column(
          width = 6,
          wellPanel(
            strong("Hatteras"),
            plotOutput("cam3_img"),
            actionButton(inputId = "m1_cam3_flood", label = "Flooding"),
            actionButton(inputId = "m1_cam3_dry", label = "Not-Flooding")
          )
        ),
        column(
          width = 6,
          wellPanel(
            strong("Buxton"),
            plotOutput("cam4_img"),
            actionButton(inputId = "m1_cam4_flood", label = "Flooding"),
            actionButton(inputId = "m1_cam4_dry", label = "Not-Flooding")
          )
        )
      ),
      
      ###_ Row 3 ----------------------------------
      fluidRow(
        column(
          width = 6,
          wellPanel(
            strong("Canal"),
            plotOutput("cam5_img"),
            actionButton(inputId = "m1_cam5_flood", label = "Flooding"),
            actionButton(inputId = "m1_cam5_dry", label = "Not-Flooding")
          )
        ),
        column(
          width = 6,
          wellPanel(
            strong("Avon"),
            plotOutput("cam6_img"),
            actionButton(inputId = "m1_cam6_flood", label = "Flooding"),
            actionButton(inputId = "m1_cam6_dry", label = "Not-Flooding")
          )
        )
      )
    ),
    
    
    #### Tab 3 - Model 1 Results  ####
    
    tabPanel(
      strong("Model 2 Results"),
      style = 'padding-bottom: 20px;',
      
      ###_ Row 1 ----------------------------------
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            strong("Ocracoke"),
            plotOutput("cam1_img"),
            actionButton(inputId = "m2_cam1_flood", label = "Flooding"),
            actionButton(inputId = "m2_cam1_dry", label = "Not-Flooding")
          )
        ),
        column(
          width = 6,
          wellPanel(
            strong("Mirlo"),
            plotOutput("cam2_img"),
            actionButton(inputId = "m2_cam2_flood", label = "Flooding"),
            actionButton(inputId = "m2_cam2_dry",   label = "Not-Flooding")
          )
        )
      ),
      
      ###_ Row 2 ----------------------------------
      fluidRow(
        column(
          width = 6,
          wellPanel(
            strong("Hatteras"),
            plotOutput("cam3_img"),
            actionButton(inputId = "m2_cam3_flood", label = "Flooding"),
            actionButton(inputId = "m2_cam3_dry", label = "Not-Flooding")
          )
        ),
        column(
          width = 6,
          wellPanel(
            strong("Buxton"),
            plotOutput("cam4_img"),
            actionButton(inputId = "m2_cam4_flood", label = "Flooding"),
            actionButton(inputId = "m2_cam4_dry", label = "Not-Flooding")
          )
        )
      ),
      
      
      ###_ Row 3 ----------------------------------
      fluidRow(
        column(
          width = 6,
          wellPanel(
            strong("Canal"),
            plotOutput("cam5_img"),
            actionButton(inputId = "m2_cam5_flood", label = "Flooding"),
            actionButton(inputId = "m2_cam5_dry", label = "Not-Flooding")
          )
        ),
        column(
          width = 6,
          wellPanel(
            strong("Avon"),
            plotOutput("cam6_img"),
            actionButton(inputId = "m2_cam6_flood", label = "Flooding"),
            actionButton(inputId = "m2_cam6_dry",   label = "Not-Flooding")
          )
        )
      )
    ) # Close Tab content 3
    
    
    
  ) # Close tabs
  
  
  
  
  
  
)