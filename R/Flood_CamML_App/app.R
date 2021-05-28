


####  Packages  ####
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
library(keras)
library(purrr)
library(noaaoceans)


####  Python Paths  ####

# Python Path for Publishing to shinyapps.io
# Sys.setenv(RETICULATE_PYTHON = '/usr/local/bin/python')

# Adam G's python path
# Sys.setenv(RETICULATE_PYTHON = 'C:/python39')

# Adam K's python path
reticulate::use_condaenv(condaenv = "py36")


####  Google Auth  ####

# Keys for Google Auth
# source(here::here("R/Flood_CamML_App/google_keys.R")) # testing
source("./google_keys.R") # publishing

# load google authentications
folder_ID <- Sys.getenv("GOOGLE_FOLDER_ID")
sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")
google_json_path <- Sys.getenv("GOOGLE_JSON_PATH")

# Set configurations
googledrive::drive_auth(path = google_json_path)
googlesheets4::gs4_auth(token = googledrive::drive_token())


## 1. Load Models ---------------------------------------------------------------------

# Path to model within Github folder

# Best model
model1 <- keras::load_model_tf("./ml/Rmodel_5_27_2021")

# Old from scratch model for testing. 
# Can uncomment for local testing, but memory runs out of shinyapps.io
# If uncommented, change the selectInput choices for "input$model_number"

# model2 <- keras::load_model_tf("./ml/Rmodel_5_25_2021")


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

# Added picture downloads here so the files already exist before server code executes
get_traffic_cam("Mirlo")
get_traffic_cam("NorthDock")
get_traffic_cam("SouthDock")
get_traffic_cam("SouthOcracoke")


write_traffic_cam <- function(camera_name, cam_time) {
  suppressMessages(googledrive::drive_upload(
    media =  paste0(camera_name,'.jpg'),
    path = as_id(folder_ID),
    name =  paste0(camera_name, "_", cam_time, ".jpg")
  ))
}

get_tides <- function(location) {

  station_id <- switch(
        EXPR = location,
        "Oregon Inlet Marina" = '8652587',
        "USCG Hatteras"       = '8654467')
    

  df <- noaaoceans::query_coops_data(
    station_id,
    start_date = format(Sys.Date(), "%Y%m%d"),
    end_date   = format(Sys.Date()+1, "%Y%m%d"),
    'predictions',
    units      = "english",  # feet
    time_zone  = "lst_ldt",
    interval   = 'hilo',
    datum      = 'MLLW')  # alternatively, 'MHW'

  df <- df %>%
    mutate(t = lubridate::ymd_hm(t)) %>%
    dplyr::select(-station)
    
  colnames(df) <- c("Time","Predicted tide (ft MLLW)", "Type")
    
  return(df)
}


## 3. Functions to classify Images ---------------------------------------------------------------------

rescale <- function(dat, mn, mx){
  m = min(dat)
  M = max(dat)
  
  z <- ((mx-mn)*(dat-m))/((M-m)+mn)
  return(z)
}

standardize <- function(img) {
  s = sd(img)
  m = mean(img)
  img = (img - m) / s
  
  img =rescale(img, 0, 1)
  
  rm(s, m)
  
  return(img)
}

predict_flooding <- function(camera_name, model_number){
  
  # Reshape to correct dimensions (1, 224, 224, 3)
  img_array <- keras::image_load(paste0(camera_name,'.jpg'),
                                 target_size = c(224,224)) %>% 
    keras::image_to_array() %>% 
    standardize() %>%
    keras::array_reshape(., c(1, dim(.)))
  
  # Model prediction. I think it outputs it as a list, so could convert with a simple "as.numeric()" or "c()"
  prediction <- eval(parse(text = paste0("model",model_number))) %>% 
    predict(x = img_array) 
  
  as.numeric(prediction)
}

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
      
      #####_ Models  ####
      menuItem("Model", tabName = "Model", icon = icon("robot")),
      
      conditionalPanel(
        condition = "input.nav === 'Model'",
        div(style="border-left-style: solid; border-left-width: medium; border-left-color: white;",
            p(strong("Directions: "),"Directions", style = "color:white;font-size:12pt;width:250px;margin-left:20px;"),
            br(),
            p(strong("Model 1: "),br(),"Trained from scratch", style = "color:white;font-size:12pt;width:250px;;margin-left:20px;"),
            br(),
            p(strong("Model 2: "),br(),"Trained from scratch - old version (for testing)",style = "color:white;font-size:12pt;width:250px;;margin-left:20px;"),
            br(),
            selectInput(inputId = "model_number",
                        label = "Choose a model",
                        choices = c(1),
                        selected = 1),
            br(),
            div(align="center",
              actionButton(inputId = "submit", label = "SUBMIT ASSESSMENT", class = "btn btn-success", style="color:white;font-size:12pt,font-weight:bold")
            )
        )
      ), 
      

      
      menuItem("About", tabName = "About", icon = icon("info-circle")),
      br()
    )
  ),
  
  ####  Dashboard Body  ####
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
      waiter::waiter_preloader(html = spin_wave(), color = "#222d32"),
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
        
        
        ###### Model ####
        tabItem(tabName = "Model",
                fluidRow(
                  
                  ######_ Prediction Key  ####
                  box(solidHeader = T,
                      height = "200px",
                      column(width=6,
                             h2("Model predictions"),
                             tippy::tippy(span(class="badge","Flooding",style="background-color:#dd4b39;"),h4("This means that the model is more than ", strong("60%")," sure that there is water on the road")),
                             tippy::tippy(span(class="badge","Unsure",style="background-color:#f39c12;"),h4("This means that the model is between ", strong("40 - 60%")," sure that there is water on the road")),
                             tippy::tippy(span(class="badge","No Flooding",style="background-color:#00a65a;"),h4("This means that the model is less than ", strong("40%")," sure that there is water on the road"))
                      ),
                      column(width=6,
                             h4('Help us validate our machine learning models, click "Flooding" or "No Flooding" under each image. For more detailed instructions, check out "About Flood CamML"')
                      )
                  ),
                  
                  ######_ Latest Conditions  ####
                  box(solidHeader = T,
                      height = "200px",
                    # h2("Latest conditions"),
                    div(style="display: flex;justify-content: space-around;align-items: center;flex-direction: row;",
                    uiOutput("next_tide_label"),
                    tableOutput("latest_tides")
                    )
                  )
                ),
                
                ######_ Cams  ####
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
                  
                  # SouthDock
                  column(width=6,
                         uiOutput(outputId = "southdock_selection")),
                  
                  # South Ocracoke 
                  column(width=6,
                         uiOutput(outputId = "southocracoke_selection"))
                  )
                
        ), 
        
        tabItem(tabName = "About Flood CamML",

                fluidRow(
                  h1("NC12 Flood CamML"),
                  p('Flood CamML is an open source project funded by the NSF Coastlines and People program,
                     and was completed over ~72 hours by a group of awesome scientists from across the country. /n/n

                     Our mission: Develop a machine learning (ML) algorithm that can detect from a single image whether or not
                     a roadway is flooded.

                     Why did we make Flood CamML?: As scientists, we are interested in *how often* coastal roadways -- and
                     the people that depend on these roadways -- are impacted by shallow (nuisance) flooding or ponding.
                     It is *easy* for a human to look at a traffic camera and recognize whether a roadway is flooded, but
                     who has all day to look at webcameras? Our question: can we train a machine to detect flooding?
                     Or can the machine train itself to detect flooding given enough images?

                     Why NC12?: North Carolina Highway 12 (NC12) provides access to the Outer Banks, a chain of low-lying barrier
                     islands. Segments of NC12 are highly vulnerable to both storm and high-tide impacts, and when flooded,
                     isolate communities from the mainland. The NC Department of Transportation maintains a series of webcams
                     along NC12, which we utilize here!

                     Instructions: Please help us validate our ML models to identify flooded roadways!
                     - What do we mean by flooded? -- Images should be labeled *"flooded"* if several
                     inches or more of water is on the roadway (typically recognizable by a sheen). Wet roadways should
                     be classified as "not flooded".
                     - What if you are not sure if the roadway is flooded, or if the image is blurred? -- If you cannot
                     see the roadway in an image, or if you are not sure if flood waters are in the roadway or off to the
                     side, classify the image as "not sure"')
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
             text = 'View real-time NCDOT traffic camera images along North Carolina Highway 12 \n\n
             Help our machine learning model better detect flooded roadways \n\n
             Images and model predictions are preliminary and for informational purposes only \n\n
             (pronunced "flood camel"!)',
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonText = "OK",
             type = "info",
             animation=F,
             size = "s",
             inputId = "splash_page")
  
  #-------------------- Get local data ---------------
  tides <- get_tides("Oregon Inlet Marina")
  
  todays_tides <- tides %>% 
    filter(as.Date(Time) == Sys.Date())
  
  next_tide <- tides %>% 
    filter(Time > Sys.time()) %>% 
    arrange(Time) %>% 
    slice(1) 
  
  output$latest_tides <- renderTable({
     todays_tides %>% 
      mutate(Time = str_remove(format(Time, "%I:%M %p"), "^0+"))
  })
  
  output$next_tide_label <- renderUI({
    tide_label <- next_tide %>% 
      mutate(Time = ifelse(as.Date(Time) == Sys.Date(), paste0("Today at ", format(Time, "%I:%M %p")), paste0("Tomorrow at ", format(Time, "%I:%M %p")))) %>% 
      mutate(Type = ifelse(Type == "H", "High", "Low"))
    
    h4("Next tide:",br(),strong(tide_label$Time),br(),tide_label$Type)
    
  })

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
  # observe({
  #   print(button_info_model1$mirlo_button_info)
  #   print(button_info_model1$northdock_button_info)
  #   print(button_info_model1$southdock_button_info)
  #   print(button_info_model1$southocracoke_button_info)
  #   
  # })
  
  
  
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
  
  #--------------- Model Results ----------------------
  
  # Get Predictions
  mirlo_predict <- reactive({
    predict_flooding("Mirlo", input$model_number)
  })
  
  northdock_predict <- reactive({
    predict_flooding("NorthDock", input$model_number)
  })
  
  southdock_predict <- reactive({
    predict_flooding("SouthDock", input$model_number)
  })
  
  southocracoke_predict <- reactive({
    predict_flooding("SouthOcracoke", input$model_number)
  })
  
  
  #--------------- Display Camera Feeds ----------------------
  
  # Initialize wait screen for individual pics. Need one of these for each site
  w <- Waiter$new(id = "mirlo_selection",
                  html = spin_3k(),
                  color = transparent(.75))
  
  w2 <- Waiter$new(id = "northdock_selection",
                   html = spin_3k(),
                   color = transparent(.75))
  
  w3 <- Waiter$new(id = "southdock_selection",
                   html = spin_3k(),
                   color = transparent(.75))
  
  w4 <- Waiter$new(id = "southocracoke_selection",
                   html = spin_3k(),
                   color = transparent(.75))
  
  
  # 1. Build UI for Camera Image Displays
  
  # Function to apply to each
  render_cam_image <- function(cam_name, alt_name){
    out_image <- renderImage({
      outfile <- paste0(cam_name,'.jpg')
      list(src = outfile,
           alt = alt_name,
           width = "100%"#, height="180px"
      )
    }, deleteFile=F)
    
    return(out_image)
  }
  
  # Run Each Camera
  output$mirlo_picture <- render_cam_image(cam_name = "Mirlo", alt_name = "Mirlo Beach")
  output$northdock_picture <- render_cam_image(cam_name = "NorthDock", alt_name = "NorthDock")
  output$southdock_picture <- render_cam_image(cam_name = "SouthDock", alt_name = "SouthDock")
  output$southocracoke_picture <- render_cam_image(cam_name = "SouthOcracoke", alt_name = "SouthOcracoke")
  
  
  #--------------- Camera Feedback UI ----------------------
  
  # 2. Display for image box / model classification
  
  # Function to apply to each
  # takes the camera name, the reactive time, and the model predictions
  render_camera_ui <- function(cam_name, cam_time, waiter_number, model_prediction, id_suffix = ""){
    
    model_prediction_val <- model_prediction
    cam_time_val <- cam_time
    lst_time <- cam_time_val %>% lubridate::with_tz("America/New_York")
    
    # string prep for naming patterns for UI elements
    # option to add suffix for "_unsupervised" ui elements
    name_lcase <- tolower(cam_name)
    img_output_id <- str_c(name_lcase, "_picture", id_suffix)
    radio_button_id <- str_c(name_lcase, "_button_select", id_suffix)
    button_clear <- str_c(name_lcase, "_clear", id_suffix)
    
    camera_button_ui <- renderUI({
      eval(parse(text = paste0("w",waiter_number,"$show()")))
        div(width="100%",
            style="background-color: #ffffff;
            padding: 10px;
            border-radius: 10px;
            margin: 10px 0;",
            # height=300,
            align  = "center",
            div(style="display:inline-block",
                h2(gsub("([a-z])([A-Z])", "\\1 \\2", cam_name))),
            div(style="display:inline-block",
                if(model_prediction_val > 0.6){
                  span(class="badge","Flooding",style="background-color:#dd4b39;
             position: relative;
             bottom: 5px;
             color:white;")
                }
                
                else if(model_prediction_val > 0.4 & model_prediction_val <= 0.6){
                  span(class="badge","Unsure",style="background-color:#f39c12;
             position: relative;
             bottom: 5px;
             color:white;")
                }
                
                else if(model_prediction_val <= 0.4 ){
                  span(class="badge","No Flooding",style="background-color:#00a65a;
             position: relative;
             bottom: 5px;
             color:white;")
                }),
            
            # Display Cam Image
            imageOutput(img_output_id,
                        height="100%"),
            
            # Datetime for image
            p(paste0(lst_time, " EDT/EST")),
            
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
  observe({
    output$mirlo_selection <- render_camera_ui(cam_name = "Mirlo", 
                                               cam_time = isolate(mirlo_time_reactive()),
                                               waiter_number = "",
                                               model_prediction = mirlo_predict())
  })

  observe({
    output$northdock_selection <- render_camera_ui(cam_name = "NorthDock", 
                                                   cam_time = isolate(northdock_time_reactive()),
                                                   waiter_number = "2",
                                                   model_prediction = northdock_predict())
  })
  
  observe({
    output$southdock_selection <- render_camera_ui(cam_name = "SouthDock", 
                                                 cam_time = isolate(southdock_time_reactive()),
                                                 waiter_number = "3",
                                                 model_prediction = southdock_predict())
  })
  
  observe({
    output$southocracoke_selection <- render_camera_ui(cam_name = "SouthOcracoke", 
                                                       cam_time = isolate(southocracoke_time_reactive()),
                                                       waiter_number = "4",
                                                       model_prediction = southocracoke_predict())
  })
 
  
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
  
  
  
  
  
  
  ###########  Reactive Button Info #######################
  
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
  # This reactiveValue is to keep track of what model users have submitted
  submissions <- reactiveValues("model1" = F,
                                "model2" = F)
  
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
  
  observeEvent(input$model_number,{
    if(submissions$model1 == F & submissions$model2 == T){
      # Change button to "submitted"
      updateActionButton(session = session,
                         inputId = "submit",
                         label = "SUBMIT ASSESSMENT", 
                         icon = icon("ok", lib = "glyphicon"))
      
      # disables submit button
      shinyjs::toggleState("submit")
    }
    
    # if(submissions$model1 == T){
    #   # Change button to "submitted"
    #   updateActionButton(session = session,
    #                      inputId = "submit",
    #                      label = "SUBMIT ASSESSMENT", 
    #                      icon = icon("ok", lib = "glyphicon"))
    #   
    #   # disables submit button
    #   shinyjs::disable("submit")
    # }
    
    if(submissions$model2 == F & submissions$model1 == T){
      # Change button to "submitted"
      updateActionButton(session = session,
                         inputId = "submit",
                         label = "SUBMIT ASSESSMENT", 
                         icon = icon("ok", lib = "glyphicon"))
      
      # disables submit button
      shinyjs::toggleState("submit")
    }
    
  })
  
  # 2. Put user data into table, push to google sheets:
  # Final submission for model 1 (tab 1)
  observeEvent(input$shinyalert == T,{
    req(input$shinyalert)

    updateActionButton(session = session,
                       inputId = "submit",
                       label = "SUBMITTED!", 
                       icon = icon("ok", lib = "glyphicon"))
    
    # disables submit button
    shinyjs::disable("submit")
    
    if(input$model_number == 1){
      submissions$model1 <- T
    }
    
    if(input$model_number == 2){
      submissions$model2 <- T
    }
    ######  Supervised Model Feedback  ####
    
    
    # Function to pull relevant camera data from models and feedback
    store_cam_data <- function(cam_name, cam_time, model_prediction, button_response){
      cam_data <- tibble(
        "date"          = c(cam_time),
        "location"      = c(cam_name),
        "filename"      = str_c(cam_name,"_",cam_time,".jpg"), 
        "model_type"    = input$model_number, 
        "model_score"   = model_prediction,
        "model_class"   = ifelse(model_prediction > 0.6, "Flooding",
                                 ifelse(model_prediction <= 0.6 & model_prediction > 0.4, "Unsure","No Flooding")),
        "user_response" = ifelse(is.null(button_response), NA, button_response)
      )
    }
    
    # Grab the data from the different cams
    # observe({
      mirlo_data <- store_cam_data(cam_name = "Mirlo", 
                                   cam_time = isolate(mirlo_time_reactive()),
                                   model_prediction = mirlo_predict(), 
                                   button_response = button_info_model1$mirlo_button_info)
    # })
    
    # observe({
      northdock_data <- store_cam_data(cam_name = "NorthDock", 
                                       cam_time = isolate(northdock_time_reactive()),
                                   model_prediction = northdock_predict(), 
                                   button_response = button_info_model1$northdock_button_info)
    # })
      
    # observe({
      southdock_data <- store_cam_data(cam_name = "SouthDock", 
                                     cam_time = isolate(southdock_time_reactive()),
                                 model_prediction = southdock_predict(), 
                                 button_response = button_info_model1$southdock_button_info)
    # })
      
    # observe({
      southocracoke_data <- store_cam_data(cam_name = "SouthOcracoke", 
                                         cam_time = isolate(southocracoke_time_reactive()),
                                 model_prediction = southocracoke_predict(), 
                                 button_response = button_info_model1$southocracoke_button_info)
    # })
    
    # Join them into one table
    data <- bind_rows(mirlo_data, northdock_data, southdock_data, southocracoke_data)
    
    # Append data to google sheet
    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = data))
    
    purrr::map2(data$location, data$date, write_traffic_cam)

    
  })
    
}

# Run the application
shinyApp(ui = ui, server = server)
