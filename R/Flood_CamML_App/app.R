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
source("./google_keys.R")

sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")
API_KEY   <- Sys.getenv("GOOGLE_API_KEY")

googledrive::drive_auth_configure(api_key = API_KEY)
googlesheets4::gs4_auth(token = googledrive::drive_token())


## 1. Load Models ---------------------------------------------------------------------

#model <- keras::load_model_tf("C:/GitHub/FloodCamMLShiny/R/Flood_CamML_App/ml/supervised")



## 2. Functions to load NCDOT Images ---------------------------------------------------------------------

get_traffic_cam <- function(camera_name){
  
  # from   https://www.drivenc.gov/
  URL <- switch(
    EXPR = camera_name,
    'Mirlo'         = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_MirloBeach.jpg',
    "NorthDock"     = 'https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_North_Dock.jpg',
    "SouthDock"     = 'https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_South_Dock.jpg',
    "SouthOcracoke" = 'https://tims.ncdot.gov/tims/cameras/viewimage.ashx?id=Ocracoke_South.jpg',
    "Ocracoke"      = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_OcracokeNorth.jpg',
    "Hatteras"      = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NorthHatterasVillage.jpg',
    "Buxton"        = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_Buxton.jpg',
    "NewInlet"      = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NewInlet.jpg',
    "Canal"         = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_CanalZone.jpg',
    "RBNurl"        = 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=RodantheBridgeNorth.jpg')
  
  # Load tmp file
  # tmpfile <- tempfile(fileext = ".jpg")  # this file will need to be sent to GoogleDrive eventually: do in Shiny?
  
  # retrieve the image
  pic <- magick::image_read(URL)
  time <-  Sys.time() %>% lubridate::with_tz("UTC")
  
  # write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile".
  # magick::image_write(pic, path = tmpfile, format = "jpg")
  magick::image_write(pic, path = paste0(camera_name,'.jpg'), format = "jpg")
  
  return(time)
}



## 3. Functions to classify Images ---------------------------------------------------------------------












####____________________________________####
#------------------------ Define UI ---------------------------------------
ui <- dashboardPage(
  title = "NC12 Flood CamML", 
  skin = "black",
  
  
  #####  Header  ####
  header = dashboardHeader(
    title =  HTML('
                <div width="300px">
                      <i class="fas fa-camera-retro" role="presentation" aria-label="camera icon" style="color:#ffffff;position:absolute;left:25px;top:15px"></i><p style="color:white">NC12 Flood CamML</p>
                </div>
                      '),
    titleWidth = 350),
  
  
  #####  Sidebar  ####
  sidebar = dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "nav",
      
      #####_ Model 1  ####
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
      
      #####_ Model 2 ####
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
  
  #####  Dashboard Body  ####
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
        css = ""),
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
      
      ##### Tab Items  ####
      tabItems(
        
        
        ###### Model 1 ####
        tabItem(tabName = "Model1",
                fluidRow(
                  
                  ######_ Prediction Key  ####
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
                  
                  ######_ Latest Conditions  ####
                  box(solidHeader = T,
                    h1("Latest conditions")
                  )
                ),
                
                ######_ Mod 1 Cams  ####
                fluidRow(
                  
                  # Mirlo
                  column(width=6,
                         uiOutput(outputId = "mirlo_selection")),
                  
                  # NorthDock
                  column(width=6,
                         uiOutput(outputId = "northdock_selection"))
                  ),
                
                # Second Row
                fluidRow(
                  
                  # Tester for "Danger"
                  column(width=6,
                         uiOutput(outputId = "southdock_selection")),
                  
                  # Test Box 
                  column(width=6,
                         uiOutput(outputId = "southocracoke_selection"))
                  )
                
        ), 
        
        
        ###### Model 2 ####
        # Tab showing selected data and time series graphs
        tabItem(tabName = "Model2",
                
                #####_ Mod 2 Cams  ####
                fluidRow(
                  column(width=6,
                         uiOutput(outputId = "mirlo_selection_unsupervised")
                  ),
                  
                  column(width=6,
                         uiOutput(outputId = "northdock_selection_unsupervised")
                  )
                ),
                
                fluidRow(
                  column(width=6,
                         uiOutput(outputId = "southdock_selection_unsupervised")
                  ),
                  column(width=6,
                         uiOutput(outputId = "southocracoke_selection_unsupervised")
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









####_______________________________####
####  Server  ####

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
  
  
  
  
  
  
  
  #-------------- Reactive Value Holders -------------
  # These capture user inputs for later
  
  # feedback on model 1
  button_info_model1 <- reactiveValues(mirlo_button_info = NULL, 
                                       northdock_button_info = NULL,
                                       southdock_button_info = NULL,
                                       southocracoke_button_info = NULL)
  
  # feedback on model 2
  button_info_model2 <- reactiveValues(mirlo_button_info = NULL, 
                                       northdock_button_info = NULL,
                                       southdock_button_info = NULL,
                                       southocracoke_button_info = NULL)
  
  
  # Print values as error check
  observe({
    print(button_info_model1$mirlo_button_info)
    print(button_info_model1$northdock_button_info)
    print(button_info_model1$southdock_button_info)
    print(button_info_model1$southocracoke_button_info)
    
  })
  
  
  
  ####____________________________####
  ####__  Supervised Model Displays __####

  #--------------- Get Cam Images ----------------------
  
  
  # Get Traffic Cam Images
  
  # Function to Apply to Each Camera
  get_cam <- function(cam_name){
    reactive({
      invalidateLater(millis = 5*60*1000, session = session)
      get_traffic_cam(cam_name)
    })
  }
  
  # Run Each Camera
  mirlo_time_reactive <- get_cam("Mirlo")
  northdock_time_reactive <- get_cam("NorthDock")
  southdock_time_reactive <- get_cam("SouthDock")
  southocracoke_time_reactive <- get_cam("SouthOcracoke")
  
  
  
  
  
  
  #--------------- Model 1. Results ----------------------
  
  # Get Predictions
  
  mirlo_predict <- reactive({
    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  
  northdock_predict <- reactive({
    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  
  southdock_predict <- reactive({
    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  
  southocracoke_predict <- reactive({
    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  
  
  #--------------- Display Camera Feeds ----------------------
  
  
  # Initialize wait screen for individual pics. Need one of these for each site
  w <- Waiter$new(id = "mirlo_selection",
                  html = spin_3k(),
                  color = transparent(.75))
  
  
  # 1. Build UI for Camera Image Displays
  
  # Function to apply to each
  render_cam_image <- function(cam_name, alt_name){
    out_image <- renderImage({
      outfile <- str_c(cam_name, ".jpg")
      list(src = outfile,
           alt = alt_name,
           width = "100%"#, height="180px"
      )
    }, deleteFile=F)
    
    return(out_image)
  }
  
  # Run Each Camera
  output$mirlo_picture <- render_cam_image(cam_name = "Mirlo", alt_name = "Mirlo Beach")
  output$northdock_picture <- render_cam_image(cam_name = "Northdock", alt_name = "NorthDock")
  output$southdock_picture <- render_cam_image(cam_name = "SouthDock", alt_name = "SouthDock")
  output$southocracoke_picture <- render_cam_image(cam_name = "SouthOcracoke", alt_name = "SouthOcracoke")
  
  
  
  
  
  #--------------- Camera Feedback UI ----------------------
  
  # 2. Display for image box / model classification
  
  # Function to apply to each
  # takes the camera name, the reactive time, and the model predictions
  render_camera_ui <- function(cam_name, cam_time, model_prediction, id_suffix = ""){
    
    # string prep for naming patterns for UI elements
    # option to add suffix for "_unsupervised" ui elements
    name_lcase <- tolower(cam_name)
    img_output_id <- str_c(name_lcase, "_picture", id_suffix)
    radio_button_id <- str_c(name_lcase, "_button_select", id_suffix)
    button_clear <- str_c(name_lcase, "_clear", id_suffix)
    
    # UI generation for radio buttons
    camera_button_ui <- renderUI({
      w$show()
      box(width="100%",
          # height=300,
          solidHeader = T,
          status = "success", #ifelse(model_prediction >= 0.6, "danger", ifelse(model_prediction <0.6 & model_prediction >=0.4, "warning", "success"))
          title  = cam_name,
          align  = "center",
          # Display Cam Image
          imageOutput(img_output_id,
                      height="100%"),
          # Datetime for image
          p(paste0(cam_time %>% lubridate::with_tz("America/New_York"), " EDT/EST")),
          # Inline boxes for user feedback
          div(style="display:inline-block",
              shinyWidgets::radioGroupButtons(inputId = radio_button_id, 
                                              choiceNames = c("Flooding", "Not Sure", "No Flooding"), 
                                              choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                                              justified = F, 
                                              selected = character(0), 
                                              checkIcon = list(yes = icon("ok",lib="glyphicon")))
          ),
          
          # clear selection button
          div(style="display:inline-block",
              actionButton(inputId = button_clear, 
                           label = "Clear", 
                           class = "btn btn-primary", 
                           style = "font-size:10pt;color:white")
          )
      )
    })
    
    #return the UI
    return(camera_button_ui)
    
    
  }
  
  # Apply to each camera
  output$mirlo_selection <- render_camera_ui(cam_name = "Mirlo", 
                                             cam_time = mirlo_time_reactive(),
                                             model_prediction = mirlo_predict())
  output$northdock_selection <- render_camera_ui(cam_name = "NorthDock", 
                                                 cam_time = northdock_time_reactive(),
                                                 model_prediction = northdock_predict())
  output$southdock_selection <- render_camera_ui(cam_name = "SouthDock", 
                                                 cam_time = southdock_time_reactive(),
                                                 model_prediction = southdock_predict())
  output$southocracoke_selection <- render_camera_ui(cam_name = "SouthOcracoke", 
                                                     cam_time = southocracoke_time_reactive(),
                                                     model_prediction = southocracoke_predict())
 
  
  
  
  ####____________________________####
  ####__  Un-Supervised Model Displays __####
  #------------------ Get Cam Images  ----------------
  
  # 1. reset timer for images
  mirlo_time_reactive_unsupervised         <- get_cam("Mirlo")
  northdock_time_reactive_unsupervised     <- get_cam("NorthDock")
  southdock_time_reactive_unsupervised     <- get_cam("SouthDock")
  southocracoke_time_reactive_unsupervised <- get_cam("SouthOcracoke")
  
 
  
  #--------------- Model 2. Results ----------------------
  
  # 2. Code to predict flood/not flood from model
  mirlo_predict_unsupervised <- reactive({
    # code to predict. Can use "Mirlo.jpg" as path for keras code
  })
  northdock_predict_unsupervised <- reactive({
    # code to predict. 
  })
  southdock_predict_unsupervised <- reactive({
    # code to predict. 
  })
  southocracoke_predict_unsupervised <- reactive({
    # code to predict. 
  })
  
  
  #--------------- Display Camera Feeds ----------------------
  
  
  # Initialize wait screen for individual pics. Need one of these for each site
  w <- Waiter$new(id = "mirlo_selection_unsupervised",
                  html = spin_3k(),
                  color = transparent(.75))
  
  
  # Render camera images
  output$mirlo_picture_unsupervised <- render_cam_image(cam_name = "Mirlo", alt_name = "Mirlo Beach")
  output$northdock_picture_unsupervised <- render_cam_image(cam_name = "NorthDock", alt_name = "NorthDock")
  output$southdock_picture_unsupervised <- render_cam_image(cam_name = "SouthDock", alt_name = "SouthDock")
  output$southocracoke_picture_unsupervised <- render_cam_image(cam_name = "SouthOcracoke", alt_name = "SouthOcracoke")

  
  
  #--------------- Camera Feedback UI ----------------------
  
  # Display model classification around pictures
  # Apply model prediction and radio buttons to each camera
  output$mirlo_selection_unsupervised <- render_camera_ui(cam_name = "Mirlo", 
                                                          cam_time = mirlo_time_reactive_unsupervised(),
                                                          model_prediction = mirlo_predict_unsupervised(),
                                                          id_suffix = "_unsupervised")
  output$northdock_selection_unsupervised <- render_camera_ui(cam_name = "NorthDock", 
                                                              cam_time = northdock_time_reactive_unsupervised(),
                                                              model_prediction = northdock_predict_unsupervised(),
                                                              id_suffix = "_unsupervised")
  output$southdock_selection_unsupervised <- render_camera_ui(cam_name = "SouthDock", 
                                                              cam_time = southdock_time_reactive_unsupervised(),
                                                              model_prediction = southdock_predict_unsupervised(),
                                                              id_suffix = "_unsupervised")
  output$southocracoke_selection_unsupervised <- render_camera_ui(cam_name = "SouthOcracoke", 
                                                                  cam_time = southocracoke_time_reactive_unsupervised(),
                                                                  model_prediction = southocracoke_predict_unsupervised(),
                                                                  id_suffix = "_unsupervised")
  
  
  
  ####____________________________####
  ####__  User Data Collection  __####
  
  
  #------------------ Reactive reset buttons ----------------
  
  #####__ 1. Reset supervised buttons  ####
  
  # Mirlo
  observeEvent(input$mirlo_clear ,{
    updateRadioGroupButtons(session = session,
                            inputId = "mirlo_button_select",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  
  
  # NorthDock
  observeEvent(input$northdock_clear ,{
    updateRadioGroupButtons(session = session,
                            inputId = "northdock_button_select",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  
  
  # SouthDock
  observeEvent(input$southdock_clear ,{
    updateRadioGroupButtons(session = session,
                            inputId = "southdock_button_select",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  
  # SouthOcracoke
  observeEvent(input$southocracoke_clear ,{
    updateRadioGroupButtons(session = session,
                            inputId = "southocracoke_button_select",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  
  
  
  
  #####__ 2. Reset unsupervised buttons  ####
  
  # Mirlo
  observeEvent(input$mirlo_clear_unsupervised ,{
    updateRadioGroupButtons(session = session,
                            inputId = "mirlo_button_select_unsupervised",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  # NorthDock
  observeEvent(input$northdock_clear_unsupervised ,{
    updateRadioGroupButtons(session = session,
                            inputId = "northdock_button_select_unsupervised",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  
  # SouthDock
  observeEvent(input$southdock_clear_unsupervised ,{
    updateRadioGroupButtons(session = session,
                            inputId = "southdock_button_select_unsupervised",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  
  # SouthOcracoke
  observeEvent(input$southocracoke_clear_unsupervised ,{
    updateRadioGroupButtons(session = session,
                            inputId = "southocracoke_button_select_unsupervised",
                            choiceNames  = c("Flooding", "Not Sure", "No Flooding"), 
                            choiceValues = c("Flooding", "Not Sure", "No Flooding"), 
                            selected = character(0), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  ###########  Reactive Button Info #######################
  
  # Need reactive code to update button values for both supervised and unsupervised
  # I made a reactive value list above to store the value: 
  # button_info_model1$mirlo_button_info 
  # button_info_model2$mirlo_button_info 
  
  # That code looked like this
  # button_info_model1 <- reactiveValues(mirlo_button_info = NULL)
  # button_info_model2 <- reactiveValues(mirlo_button_info = NULL)
  
  # Input ID's are:
  # inputId = "mirlo_button_select",
  # inputId = "mirlo_button_select_unsupervised",
  
  observeEvent(c(input$mirlo_button_select, input$mirlo_clear), {
    button_info_model1$mirlo_button_info <- input$mirlo_button_select
  })
  
  observeEvent(c(input$northdock_button_select, input$northdock_clear), {
    button_info_model1$northdock_button_info <- input$northdock_button_select
  })
  
  observeEvent(c(input$southdock_button_select, input$southdock_clear), {
    button_info_model1$southdock_button_info <- input$southdock_button_select
  })
  
  observeEvent(c(input$southocracoke_button_select, input$southocracoke_clear), {
    button_info_model1$southocracoke_button_info <- input$southocracoke_button_select
  })
  
  
  
  #------------------- Submit button for model 1 -------------------
  
  # 1. Observe the user submission
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
  
  
  # 2. Put user data into table, push to google sheets:
  # Final submission for model 1 (tab 1)
  observeEvent(input$shinyalert ==T,{
    req(input$shinyalert)

    # Change button to "submitted"
    updateActionButton(session = session,
                       inputId = "submit",
                       label = "SUBMITTED!", 
                       icon = icon("ok", lib = "glyphicon"))
    
    # disables submit button
    shinyjs::disable("submit")
    
    ######  Supervised Model Feedback  ####
    
    
    # Function to pull relevant camera data from models and feedback
    store_cam_data <- function(cam_name, cam_time, model_prediction, button_response){
      cam_data <- tibble(
        "date"          = c(cam_time),
        "location"      = c(cam_name),
        "filename"      = str_c(cam_name, ".jpg"), #change to whatever it ends up being
        "model_type"    = "supervised",
        "model_score"   = 0.3, # model_prediction
        "model_class"   = "No Flooding",#ifelse(model_prediction >= 0.5, "Flooding","No Flooding"),
        "user_response" = button_response
      )
    }
    
    # Grab the data from the different cams
    mirlo_data <- store_cam_data(cam_name = "Mirlo", 
                                 model_prediction = mirlo_predict(), 
                                 button_response = button_info_model1$mirlo_button_info)
    northdock_data <- store_cam_data(cam_name = "NorthDock", 
                                 model_prediction = northdock_predict(), 
                                 button_response = button_info_model1$northdock_button_info)
    southdock_data <- store_cam_data(cam_name = "SouthDock", 
                                 model_prediction = southdock_predict(), 
                                 button_response = button_info_model1$southdock_button_info)
    southocracoke_data <- store_cam_data(cam_name = "SouthOcracoke", 
                                 model_prediction = southocracoke_predict(), 
                                 button_response = button_info_model1$southocracoke_button_info)
    
    
    # Join them into one table
    data <- bind_rows(mirlo_data, northdock_data, southdock_data, southocracoke_data)
    
    # Append data to google sheet
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
                       icon = icon("ok",lib = "glyphicon"))
    
    shinyjs::disable("submit2")
    
    # Data for unsupervised model submissions
    data <- tibble(
      "date" = c(mirlo_time_reactive_unsupervised()),
      "location" = c("Mirlo"),
      "filename" = c("mirlo_filename.jpg"), #change to whatever it ends up being
      "model_type" = "unsupervised",
      "model_score" = 0.3, # mirlo_predict_unsupervised()
      "model_class" = "No Flooding", #ifelse(mirlo_predict_unsupervised() >= 0.5, "Flooding","No Flooding"),
      "user_response" = button_info_model2$mirlo_button_info
    )
    
    # Append data to google sheet
    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = data))
    
  })
    
}

# Run the application
shinyApp(ui = ui, server = server)