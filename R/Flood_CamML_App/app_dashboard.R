# Packages to load
library(dplyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(waiter)
library(magick)
library(shinyalert)
library(stringr)
library(shinydisconnect)
library(tippy)
library(httr)
library(shinyWidgets)
library(googledrive)
library(googlesheets4)


# Google auth
source("C:/Users/Adam Gold/Downloads/google_keys.R")

sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")
API_KEY <- Sys.getenv("GOOGLE_API_KEY")

googledrive::drive_auth_configure(api_key = API_KEY)
googlesheets4::gs4_auth(token = googledrive::drive_token())

## 1. Load Models ---------------------------------------------------------------------


## 2. Functions to load NCDOT Images ---------------------------------------------------------------------

get_traffic_cam <- function(camera_name){
  
  # from   https://www.drivenc.gov/
  if (camera_name == 'Mirlo'){
    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_MirloBeach.jpg'
  }
  if (camera_name == 'NorthDock'){
    URL <- 'https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_North_Dock.jpg'
  }
  if (camera_name == 'SouthDock'){
    URL <- 'https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_South_Dock.jpg'
  }
  if (camera_name == 'SouthOcracoke'){
    URL <- 'https://tims.ncdot.gov/tims/cameras/viewimage.ashx?id=Ocracoke_South.jpg'
  }
  #if (camera_name == 'Ocracoke'){
  #    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_OcracokeNorth.jpg'
  #}
  #if (camera_name == 'Hatteras'){
  #    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NorthHatterasVillage.jpg'
  #}
  #if (camera_name == 'Buxton'){
  #    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_Buxton.jpg'
  #}
  #if (camera_name == 'NewInlet'){
  #    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NewInlet.jpg'
  #}
  #if (camera_name == 'Canal'){
  #    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_CanalZone.jpg'
  #}
  #if (camera_name == 'RBNurl'){
  #    URL <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=RodantheBridgeNorth.jpg'
  #}
  
  tmpfile <- tempfile(fileext = ".jpg")  # this file will need to be sent to GoogleDrive eventually: do in Shiny?
  
  # retrieve the image
  pic <- magick::image_read(URL)
  time <-  Sys.time() %>% lubridate::with_tz("UTC")
  
  # write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile".
  # magick::image_write(pic, path = tmpfile, format = "jpg")
  magick::image_write(pic, path = paste0(camera_name,'.jpg'), format = "jpg")
  
  return(time)
}

## 3. Functions to classify Images ---------------------------------------------------------------------


#------------------------ Define UI ---------------------------------------
ui <- dashboardPage(
  title = "NC12 Flood CamML", 
  skin = "black",
  
  header = dashboardHeader(
    title =  HTML('
                <div width="300px">
                      <i class="fas fa-camera-retro" role="presentation" aria-label="camera icon" style="color:#ffffff;position:absolute;left:25px;top:15px"></i><p style="color:white">NC12 Flood CamML</p>
                </div>
                      '),
    titleWidth = 350
  ),
  sidebar = dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "nav",
      
      menuItem("Model 1", tabName = "Model1", icon = icon("robot")),
      
      conditionalPanel(
        condition = "input.nav === 'Model1'",
        div(style="border-left-style: solid; border-left-width: medium; border-left-color: white;",
            p("Directions", style = "color:white;font-size:12pt;width:250px;white-space: break-spaces;margin-left: auto;margin-right: auto; font-style:italic"),
            br(),
            p("More directions", style = "color:white;font-size:12pt;width:250px;white-space: break-spaces;margin-left: auto;margin-right: auto; font-style:italic"),
            
            div(align="center",
              actionButton(inputId = "submit", label = "SUBMIT ASSESSMENT", class = "btn btn-success", style="color:white;font-size:12pt,font-weight:bold")
            )
        )
      ), 
      menuItem("Model 2", tabName = "Model2", icon = icon("robot")),
      
      conditionalPanel(
        condition = "input.nav === 'Model2'",
        div(style="border-left-style: solid; border-left-width: medium; border-left-color: white;",
            p("Directions", style = "color:white;font-size:12pt;width:250px;white-space: break-spaces;margin-left: auto;margin-right: auto; font-style:italic"),
            br(),
            p("More directions", style = "color:white;font-size:12pt;width:250px;white-space: break-spaces;margin-left: auto;margin-right: auto; font-style:italic"),
            
            div(align="center",
                actionButton(inputId = "submit2", label = "SUBMIT ASSESSMENT", class = "btn btn-success", style="color:white;font-size:12pt,font-weight:bold")
            )
        )
      ), 
      
      menuItem("About", tabName = "About", icon = icon("info-circle")),
      br()
    )
  ),
  dashboardBody(
    fluidPage(
      disconnectMessage(
        text = "Your session has timed out! Try refreshing the page.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#000000",##000000
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.25,
        width = 450,
        top = "center",
        size = 24,
        css = ""
      ),
      shinyjs::useShinyjs(),
      useShinyalert(),
      use_waiter(),
      # waiter::waiter_show_on_load(html = spin_3k(),
      #                             color = transparent(0)),
      tags$head(
                tags$style(HTML('
        .skin-black .main-header .logo {
          background-color: #000000;
          border-right: 1px solid #000000;
        }
        .skin-black .main-header .logo:hover {
          background-color: #000000;
        }
        
        .skin-black .main-header .navbar {
          background-color: #000000;
        }
        
        .skin-black .main-header .navbar>.sidebar-toggle {
          color: #FFFFFF;
          border-right: 1px solid #000000;
        }
        
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          color: #fff;
          background: #000;
        }
        
        # .main-header .sidebar-toggle {
        #   font-weight: 200; 
        # }
                                
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: black;
        }
          
      '))),
      tabItems(
        
        tabItem(tabName = "Model1",
                fluidRow(
                  box(solidHeader = T,
                      column(width=6,
                        h2("Model predictions"),
                        h4(icon("circle",style="color:#dd4b39;font-size:16;border-color:black;"), "Flooding"),
                        h4(icon("circle",style="color:#f39c12;font-size:16;border-color:black;"), "Unsure"),
                        h4(icon("circle",style="color:#00a65a;font-size:16;border-color:black;"), "No Flooding")
                      ),
                      column(width=6,
                             h3("Help us validate, click 'Flooding' or 'No Flooding'")
                             )
                  ),
                  box(solidHeader = T,
                    h1("Latest conditions")
                  )
                ),
                fluidRow(
                  column(width=6,
                        uiOutput(outputId = "mirlo_selection")
                    ),
                  column(width=6,
                         box(width="100%",
                             height=300,
                             title = paste0("Test"),
                             solidHeader = T,
                             status = "warning",
                           h1("Test")
                         )
                  )
                  ),
                fluidRow(
                  column(width=6,
                         box(width="100%",
                             height=300,
                             title = "Test2",
                             solidHeader = T,
                             status = "danger",
                             h1("Test2")
                         )
                  ),
                  column(width=6,
                         box(width="100%",
                             height=300,
                             h1("Test3")
                         )
                  )
                  ),
                fluidRow(
                  column(width=6,
                         box(width="100%",
                             height=300,
                             h1("Test4")
                         )
                  ),
                  column(width=6,
                         box(width="100%",
                             height=300,
                             h1("Test6")
                         )
                  )
                )
        ), 
        
        # Tab showing selected data and time series graphs
        tabItem(tabName = "Model2",
                fluidRow(
                  column(width=6,
                         uiOutput(outputId = "mirlo_selection_unsupervised")
                  ),
                  column(width=6,
                         box(width="100%",
                             height=300,
                             title = paste0("Test"),
                             solidHeader = T,
                             status = "warning",
                             h1("Test")
                         )
                  )
                ),
                fluidRow(
                  column(width=6,
                         box(width="100%",
                             height=300,
                             title = "Test2",
                             solidHeader = T,
                             status = "danger",
                             h1("Test2")
                         )
                  ),
                  column(width=6,
                         box(width="100%",
                             height=300,
                             h1("Test3")
                         )
                  )
                ),
                fluidRow(
                  column(width=6,
                         box(width="100%",
                             height=300,
                             h1("Test4")
                         )
                  ),
                  column(width=6,
                         box(width="100%",
                             height=300,
                             h1("Test6")
                         )
                  )
                )
        ),
        tabItem(tabName = "About",
                fluidRow(
                  h1("NC12 Flood CamML"),
                  p("About...")
                )
        )
        )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Popup on load to display info
  shinyalert(title = "Welcome to the NC12 Flood CamML!",
             text = "View real-time images of NC12 to check for flooding.... \n\n Images and information are preliminary and for informational purposes only",
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonText = "OK",
             type = "info",
             animation=F,
             size = "s",
             inputId = "splash_page")
  #-------------- Set up Reactive Values to hold user input -------------
  button_info_model1 <- reactiveValues(mirlo_button_info = NULL)
  button_info_model2 <- reactiveValues(mirlo_button_info = NULL)
  
  
  #--------------- Full test for one square ----------------------
  mirlo_time_reactive <- reactive({
    invalidateLater(millis = 5*60*1000, session = session)
    get_traffic_cam("Mirlo")
  })

  mirlo_predict <- reactive({

    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  
  # Initialize wait screen for individual pics. Need one of these for each site
  w <- Waiter$new(id="mirlo_selection",
                  html = spin_3k(),
                  color = transparent(.75))
  
  output$mirlo_picture <- renderImage({
    outfile <- "Mirlo.jpg"
    
    list(src= outfile,
         alt = "Mirlo Beach",
         width = "100%"
         # height="180px"
         )
  }, deleteFile=F)
  
  output$mirlo_selection <- renderUI({
    w$show()

    box(width="100%",
        # height=300,
        solidHeader = T,
        status = "success", #ifelse(mirlo_predict() >= 0.6, "danger", ifelse(mirlo_predict() <0.6 & mirlo_predict() >=0.4, "warning", "success"))
        title = "Mirlo",
        align="center",
        imageOutput("mirlo_picture",height="100%"),
        p(paste0(mirlo_time_reactive() %>% lubridate::with_tz("America/New_York"), " EDT/EST")),
        div(style="display:inline-block",
          shinyWidgets::radioGroupButtons(inputId = "mirlo_button_select", choiceNames = c("Flooding","No Flooding"), choiceValues = c("Flooding","No Flooding"), justified = F, selected = character(0), checkIcon = list(yes = icon("ok",lib="glyphicon")))
          ),
        div(style="display:inline-block",
         actionButton(inputId = "mirlo_clear", label = "Clear", class = "btn btn-primary", style="font-size:10pt;color:white")
        )
         )
  })
  
  #------------------ Code for one unsupervised square ----------------
  mirlo_time_reactive_unsupervised <- reactive({
    invalidateLater(millis = 5*60*1000, session = session)
    get_traffic_cam("Mirlo")
  })
  
  mirlo_predict_unsupervised <- reactive({
    
    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  
  # Initialize wait screen for individual pics. Need one of these for each site
  w <- Waiter$new(id="mirlo_selection_unsupervised",
                  html = spin_3k(),
                  color = transparent(.75))
  
  output$mirlo_picture_unsupervised <- renderImage({
    outfile <- "Mirlo.jpg"
    
    list(src= outfile,
         alt = "Mirlo Beach",
         width = "100%"
         # height="180px"
    )
  }, deleteFile=F)
  
  output$mirlo_selection_unsupervised <- renderUI({
    w$show()
    
    box(width="100%",
        # height=300,
        solidHeader = T,
        status = "success", #ifelse(mirlo_predict_unsupervised() >= 0.6, "danger", ifelse(mirlo_predict_unsupervised() <0.6 & mirlo_predict_unsupervised() >=0.4, "warning", "success"))
        title = "Mirlo",
        align="center",
        imageOutput("mirlo_picture_unsupervised",height="100%"),
        p(paste0(mirlo_time_reactive() %>% lubridate::with_tz("America/New_York"), " EDT/EST")),
        div(style="display:inline-block",
            shinyWidgets::radioGroupButtons(inputId = "mirlo_button_select_unsupervised", choiceNames = c("Flooding","No Flooding"), choiceValues = c("Flooding","No Flooding"), justified = F, selected = character(0), checkIcon = list(yes = icon("ok",lib="glyphicon")))
        ),
        div(style="display:inline-block",
            actionButton(inputId = "mirlo_clear_unsupervised", label = "Clear", class = "btn btn-primary", style="font-size:10pt;color:white")
        )
    )
  })
  
  
  ########### Need reactive code to update button values for both supervised and unsupervised #######################
  ########### I made a reactive value list above to store the value: ################################################
  ########### button_info_model1$mirlo_button_info ##################################################################
  ########### button_info_model2$mirlo_button_info ##################################################################
  
  observeEvent(input$mirlo_clear ,{
    updateRadioGroupButtons(session = session,
                            inputId = "mirlo_button_select",
                            choiceNames = c("Flooding","No Flooding"), 
                            choiceValues = c("Flooding","No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok",lib="glyphicon")))
  })
  
  observeEvent(input$mirlo_clear_unsupervised ,{
    updateRadioGroupButtons(session = session,
                            inputId = "mirlo_button_select_unsupervised",
                            choiceNames = c("Flooding","No Flooding"), 
                            choiceValues = c("Flooding","No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok",lib="glyphicon")))
  })
  
  
  #------------------- Submit button for model 1 -------------------
  observeEvent(input$submit,{
    
    shinyalert(
      inputId = "shinyalert",
      title = "Submit?",
      text = "Are you ready to submit your answers?",
      size = "s",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "No",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  # Final submission for model 1 (tab 1)
  observeEvent(input$shinyalert ==T,{
    req(input$shinyalert)

    # Change button to "submitted"
    updateActionButton(session = session,
                       inputId = "submit",
                       label = "SUBMITTED!", 
                       icon = icon("ok",lib="glyphicon"))
    
    # disables submit button
    shinyjs::disable("submit")
    
    data <- tibble(
      "date" = c(mirlo_time_reactive()),
      "location" = c("Mirlo"),
      "filename" = c("mirlo_filename.jpg"), #change to whatever it ends up being
      "model_type" = "supervised",
      "model_score" = 0.3, # mirlo_predict()
      "model_class" = "No Flooding",#ifelse(mirlo_predict() >= 0.5, "Flooding","No Flooding"),
      "user_response" = button_info_model1$mirlo_button_info
    )

    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = data))
    
  })
  
  #------------------- Submit button for model 2 -------------------
  
  observeEvent(input$submit2,{
    
    shinyalert(
      inputId = "shinyalert2",
      title = "Submit?",
      text = "Are you ready to submit your answers?",
      size = "s",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "No",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  # Final submission for model 2 (tab 2)
  observeEvent(input$shinyalert2 ==T,{
    req(input$shinyalert2)
    
    updateActionButton(session = session,
                       inputId = "submit2",
                       label = "SUBMITTED!", 
                       icon = icon("ok",lib="glyphicon"))
    
    shinyjs::disable("submit2")
    
    data <- tibble(
      "date" = c(mirlo_time_reactive_unsupervised()),
      "location" = c("Mirlo"),
      "filename" = c("mirlo_filename.jpg"), #change to whatever it ends up being
      "model_type" = "unsupervised",
      "model_score" = 0.3, # mirlo_predict_unsupervised()
      "model_class" = "No Flooding", #ifelse(mirlo_predict_unsupervised() >= 0.5, "Flooding","No Flooding"),
      "user_response" = button_info_model2$mirlo_button_info
    )
    
    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = data))
    
  })
    
}

# Run the application
shinyApp(ui = ui, server = server)