####  Packages  ####
library(dplyr)
library(lubridate)
library(shiny)
library(markdown)
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
library(jsonlite)
library(readr)


####  Google Auth  ####

# Keys for Google Auth
source("./keys/google_keys.R") # publishing

# load google authentications
folder_ID <- Sys.getenv("GOOGLE_FOLDER_ID")
sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")

googledrive::drive_auth(path = "./keys/google_key.json")
# googledrive::drive_auth(path = google_key_path)
googlesheets4::gs4_auth(token = googledrive::drive_token())

# Create temp directory for storing pictures
tmp_dir <- tempdir()

#------- camera list --------------------

# Lat and Long aren't currently in use but exist in the csv for later mapping
# Filter by 'use' column so users can include other sites later
camera_info <- readr::read_csv("./camera_info.csv") %>% 
  filter(use == T)

# Create layout info for UI
panel_data <- tibble("panels" = 1:length(camera_info$camera_name)) %>% 
  mutate("rows" = ceiling(panels/2),
         "position" = c(0, abs(diff(rows)-1)))

## 1. Load Model ---------------------------------------------------------------------

# Path to model within Github folder

# Best model. 4 class classification model
model <- keras::load_model_tf("./models/Rmodel_6_23_2021")

## 2. Functions to load NCDOT Images ---------------------------------------------------------------------

get_traffic_cam <- function(camera_name){
  
  URL <- camera_info$url[camera_info$camera_name == camera_name] 
    
  # retrieve the image
  pic <- magick::image_read(URL)
  time <-  Sys.time() %>% lubridate::with_tz("UTC")
  
  # write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile".
  magick::image_write(pic, path = paste0(tmp_dir,"/",camera_name,'.jpg'), format = "jpg")
  
  return(time)
}

# Download pictures on initilization
walk(.x = camera_info$camera_name, .f = get_traffic_cam)


write_traffic_cam <- function(camera_name, cam_time) {
  suppressMessages(googledrive::drive_upload(
    media =  paste0(tmp_dir,"/",camera_name,'.jpg'),
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
    station_id = station_id,
    start_date = format(lubridate::with_tz(Sys.time(), "America/New_York") %>% as.Date()-1, "%Y%m%d"),
    end_date = format(lubridate::with_tz(Sys.time(), "America/New_York") %>% as.Date()+1, "%Y%m%d"),
    data_product = 'predictions',
    units = "english",  # feet
    time_zone = "lst_ldt",
    interval = 'hilo',
    datum = 'MLLW')  # alternatively, 'MHW'
  
  df <- df %>%
    mutate(t = lubridate::ymd_hm(t) %>% lubridate::force_tz(tzone="America/New_York"),
           v = round(as.numeric(v), digits = 2)) %>%
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


predict_flooding <- function(camera_name){
  
  
  # Reshape to correct dimensions (1, 224, 224, 3)
  img_array <- keras::image_load(paste0(tmp_dir,"/",camera_name,'.jpg'),
                                 target_size = c(224,224)) %>% 
    keras::image_to_array() %>% 
    standardize() %>%
    keras::array_reshape(., c(1, dim(.)))
  
  # Model prediction. I think it outputs it as a list, so could convert with a simple "as.numeric()" or "c()"
  prediction <- model %>% 
    predict(x = img_array) %>% 
    t()
  
  colnames(prediction) <- "prob"
  
  prediction <- prediction %>% 
    as_tibble() %>% 
    transmute(prob = round(prob, 2),
           label = c("Bad Image","No Flooding", "Not Sure", "Flooding")) %>% 
    filter(prob == max(prob, na.rm=T)) %>% 
    slice(1)
    
  prediction
}

waiting_screen <- tagList(
  spin_wave(),
  h4("Loading Flood CamML")
)

####____________________________________####
#------------------------ Define UI ---------------------------------------
ui <- dashboardPage(
  title = "NC12 Flood CamML", 
  skin = "black",
  
  
  #####  Header  ####
  header = dashboardHeader(
    title =  p("NC12 Flood CamML", style="color:white;"),
    titleWidth = 350,
    tags$li(class = "dropdown", 
            actionButton(inputId = "submit", label = "SUBMIT ASSESSMENT", class = "btn btn-success", style="color:white;font-size:12pt,font-weight:bold;"),
            style="margin:8px 20px 8px 0px;"
            )
),
  
  
  #####  Sidebar  ####
  sidebar = dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "nav",
      
      #####_ Models  ####
      menuItem("Cameras", tabName = "Cameras", icon = icon("camera-retro")),
      
      conditionalPanel(
        condition = "input.nav === 'Cameras'",
        div(style= "border-left-style: solid; 
                    border-left-width: medium; 
                    border-left-color: white;
                    overflow-wrap: anywhere;
                    padding: 1px 20px;",
            includeMarkdown("./text/cameras.md"),
            br()
        )
      ), 
      
      
      # ------------ _About Flood CamML -----------
      menuItem("About the Project", tabName = "About", icon = icon("info-circle")),
      menuItem("The Model", tabName = "Model", icon = icon("robot")),
      menuItem("Contact Us", tabName = "Contact", icon = icon("envelope"))
    )
  ),
  
  #####  Dashboard Body  ####
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
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
      waiter::waiter_preloader(html = waiting_screen, color = "#222d32"),
      tags$head(
        tags$link(rel = "shortcut icon", href = "https://raw.githubusercontent.com/FloodCamML/FloodCamMLShiny/cloud-run/pics/camel.png"),
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
        
        .main-sidebar .user-panel, .sidebar-menu, .sidebar-menu>li.header {
          white-space: normal;
          overflow: hidden;
        }
        
        .content {
          padding: 5px;
        }
        
        body.skin-black.fixed {
          overflow-y: auto;
        }

        a {
        color: #5dbeff;
          font-weight: bold;
        }

        a:hover {
        color: #5dbeff;
            text-decoration: underline;
        }

        .skin-black .sidebar a {
          font-weight: bold;
          color:#5dbeff;
        }

        .skin-black .sidebar a:hover {
          font-weight: bold;
          color:#5dbeff;
          text-decoration: underline;

        }

        .skin-black .sidebar-menu>li>a {
            color: white;
            border-left: 3px solid transparent;
        }

      '))),
      
      ##### Tab Items  ####
      tabItems(
        
        
        ###### Model ####
        tabItem(tabName = "Cameras",
                fluidRow(
                  ######_ Prediction Key  ####
                  column(width=6,
                         div(
                           style="background-color: #ffffff;
                      padding: 10px;
                      height: 225px;
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                           # height=300,
                           align  = "center",
                           h3("Flood detection using machine learning"),
                           p("Our model makes the following predictions for each image:",
                             style="text-align:center;"),
                           p(tippy::tippy(span(class="badge","Flooding",style="background-color:#dd4b39;"),h5("Road appears to be flooded")),
                             ", ",
                             tippy::tippy(span(class="badge","Not Sure",style="background-color:#f39c12;"),h5("Can't tell if the road is flooded or not")),
                             ", ",
                             tippy::tippy(span(class="badge","No Flooding",style="background-color:#00a65a;"),h5("Road appears to not be flooded")),
                             ", or ",
                             tippy::tippy(span(class="badge","Bad Image",style="background-color:#787878;"),h5("The image is dark, bad weather, camera is not working, rain on the camera lens, etc.")),
                             style="text-align:center;"),
                          p("But are these roadway predictions correct? Help us improve our model by telling us what you see using the buttons below each image: then click", strong("submit"), " in the upper right. See ",actionLink("to_about_section", "About the Project"), " for more details or hover over the classifications above.", style="text-align:center;")                         )
                  ),
                  column(width=6,
                         ######_ Latest Conditions  ####
                         div(style="background-color: #ffffff;
                      padding: 10px;
                      border-radius: 10px;
                      height: 225px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                             align  = "left",
                             span(h3("Local Tides"),align="center"),
                             uiOutput("tide_label"),
                             br(),
                             radioButtons(inputId =  "latest_tides_location",
                                          label = "Gauge Location",
                                          inline = T,
                                          choices = c("Oregon Inlet Marina",
                                                      "USCG Hatteras"),
                                          selected = "Oregon Inlet Marina")

                         )
                  )
                ),
                
                
                ######_ Cams  ####
                uiOutput(outputId = "picture_panel")
        ),
        
        # ------------- About --------------
        tabItem(tabName = "About",
                
                fluidRow(column(
                  width = 12,
                  div(
                    style = "background-color: #ffffff;
                      padding: 10px;
                      
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                    # height=300,
                    align  = "left",
                    includeMarkdown("./text/about_project.md")
                  )
                ))
        ),
        tabItem(tabName = "Model",

                fluidRow(column(
                  width = 12,
                  div(
                    style = "background-color: #ffffff;
                      padding: 10px;
                      
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                    # height=300,
                    align  = "left",
                    includeMarkdown("text/about_ML.md")
                  )
                ))
    ),
    tabItem(tabName = "Contact",

                fluidRow(column(
                  width = 12,
                  div(
                    style = "background-color: #ffffff;
                      padding: 10px;
                      
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                    align  = "left",
                    includeMarkdown("text/contact_us.md")
                  )
                ))
    )
  )
)))









####_______________________________####
####  Server  ####

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Popup on load to display info
  shinyalert(title = "",
             html = T,
             text = includeMarkdown("./text/landing_text.md"),
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonText = "OK",
             animation=F,
             size = "s",
             inputId = "splash_page", 
             closeOnEsc = T)
  
  #---------------- picture panel render ---------------
  output$picture_panel <- renderUI({
    ui_pieces <- c()
    
    for(i in 1:length(unique(panel_data$rows))){
      numbers <- panel_data %>% 
        filter(rows == i) %>% 
        pull(panels)
      
      if(nrow(panel_data %>% filter(rows == i)) == 2){
        ui_pieces[[i]] <- fluidRow(
          column(width=6,
                 uiOutput(outputId = paste0(tolower(camera_info$camera_name)[numbers[1]],"_selection"))),
          column(width=6,
                 uiOutput(outputId = paste0(tolower(camera_info$camera_name)[numbers[2]],"_selection")))
        )
      }
      
      if(nrow(panel_data %>% filter(rows == i)) == 1){
        ui_pieces[[i]] <- fluidRow(
          column(width=6,
                 uiOutput(outputId = paste0(tolower(camera_info$camera_name)[numbers[1]],"_selection")))
        )
      }
    }
    
    ui_pieces
    
  })
  
  #-------------- Link to About section --------------
  observeEvent(input$to_about_section, {
    
    updateTabItems(session = session, 
                   inputId = "nav", 
                   selected = "About")
    
  })
  
  
  #-------------------- Get local data ---------------
  w_latest_conditions <- Waiter$new(id = "tide_label",
                                    html = spin_3k(),
                                    color = transparent(.75))
  
  
  
  tides <- reactive({
   get_tides(input$latest_tides_location)
  }) %>% 
    bindCache(input$latest_tides_location)
  
  observeEvent(input$latest_tides_location,{
    w_latest_conditions$show()
    
    last_tide <- tides() %>% 
      # filter(as.Date(Time) == lubridate::with_tz(Sys.time(), "America/New_York") %>% as.Date()) %>% 
      filter(Time <= lubridate::with_tz(Sys.time(), "America/New_York")) %>% 
      arrange(rev(Time)) %>% 
      slice(1) 
    
    next_tide <- tides() %>% 
      # filter(as.Date(Time) == lubridate::with_tz(Sys.time(), "America/New_York") %>% as.Date()) %>% 
      filter(Time > lubridate::with_tz(Sys.time(), "America/New_York")) %>% 
      arrange(Time) %>% 
      slice(1) 
    
    
    output$tide_label <- renderUI({
      last_tide_label <-last_tide %>% 
        mutate(date = format(Time, "%m/%d/%Y"),
               sys_date = lubridate::with_tz(Sys.time(), "America/New_York") %>% format(., "%m/%d/%Y")) %>% 
        mutate(Time = ifelse(date == sys_date, paste0("Today at ", format(Time, "%I:%M %p")), ifelse(date > sys_date, paste0("Tomorrow at ", format(Time, "%I:%M %p")), paste0("Yesterday at ", format(Time, "%I:%M %p"))))) %>% 
        mutate(Type = ifelse(Type == "H", "High", "Low")) %>% 
        dplyr::select(-c(date, sys_date))
      
      next_tide_label <- next_tide %>% 
        mutate(date = format(Time, "%m/%d/%Y"),
               sys_date = lubridate::with_tz(Sys.time(), "America/New_York") %>% format(., "%m/%d/%Y")) %>% 
        mutate(Time = ifelse(date == sys_date, paste0("Today at ", format(Time, "%I:%M %p")), ifelse(date > sys_date, paste0("Tomorrow at ", format(Time, "%I:%M %p")), paste0("Yesterday at ", format(Time, "%I:%M %p"))))) %>% 
        mutate(Type = ifelse(Type == "H", "High", "Low")) %>% 
        dplyr::select(-c(date, sys_date))
      
      div(
        span(h5("Last tide:",strong(last_tide_label$Time),paste0("(",last_tide_label$Type,": ",last_tide_label$`Predicted tide (ft MLLW)`," ft MLLW",")"))),
        span(h5("Next tide:",strong(next_tide_label$Time),paste0("(",next_tide_label$Type,": ",next_tide_label$`Predicted tide (ft MLLW)`," ft MLLW",")")))   
      )
    })
  })
  
  #-------------- Reactive Value Holders -------------
  # These capture user inputs for later
  
  # feedback on model 1
  button_info_model1 <- reactiveValues(
    # mirlo_button_info = NULL, 
    #                                    northdock_button_info = NULL,
    #                                    southdock_button_info = NULL,
    #                                    southocracoke_button_info = NULL
    )
  
  
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
  time_reactive_list <- reactiveValues()
  
  walk(.x = camera_info$camera_name, .f = function(.x){
    time_reactive_list[[paste0(tolower(.x),"_time_reactive")]] <- get_cam(.x)
  })
  
  
  #--------------- Model Results ----------------------
  get_prediction <-  function(cam_name){
    reactive({
      invalidateLater(millis = 5*60*1000, session = session)
      predict_flooding(cam_name)
    })
  }
  
  predict_reactive_list <- reactiveValues()
  
  walk(.x = camera_info$camera_name, .f = function(.x){
    predict_reactive_list[[paste0(tolower(.x),"_predict_reactive")]] <- get_prediction(.x)
  })

  
  #--------------- Display Camera Feeds ----------------------
  
  # 1. Build UI for Camera Image Displays
  
  # Function to apply to each
  render_cam_image <- function(cam_name, alt_name){
    out_image <- renderImage({
      outfile <- paste0(tmp_dir,"/",cam_name,'.jpg')
      list(src = outfile,
           alt = alt_name,
           width = "100%"#, height="180px"
      )
    }, deleteFile=F)
    
    return(out_image)
  }
  
  # Run Each Camera
  
  walk(.x = camera_info$camera_name, .f = function(.x){
    output[[paste0(tolower(.x),"_picture")]] <- render_cam_image(cam_name = .x,
                                                                 alt_name = .x)
  })
  
  #--------------- Camera Feedback UI ----------------------
  
  # 2. Display for image box / model classification
  
  # Function to apply to each
  # takes the camera name, the reactive time, and the model predictions
  render_camera_ui <- function(cam_name, cam_time, model_prediction, id_suffix = ""){
    model_predict_info <- model_prediction()
    
    model_prediction_val <- model_predict_info$prob * 100
    model_prediction_class <- model_predict_info$label
    cam_time_val <- cam_time()
    lst_time <- format(cam_time_val %>% lubridate::with_tz("America/New_York"), "%m/%d/%Y %H:%M")
    
    # string prep for naming patterns for UI elements
    # option to add suffix for "_unsupervised" ui elements
    name_lcase <- tolower(cam_name)
    img_output_id <- str_c(name_lcase, "_picture", id_suffix)
    radio_button_id <- str_c(name_lcase, "_button_select", id_suffix)
    button_clear <- str_c(name_lcase, "_clear", id_suffix)
    
    camera_button_ui <- renderUI({
      div(width="100%",
          style="background-color: #ffffff;
            padding: 10px;
            border-radius: 10px;
            margin: 10px 0px;",
          # height=300,
          align  = "center",
          div(style="display:inline-block",
              h2(gsub("([a-z])([A-Z])", "\\1 \\2", cam_name))),
          div(style="display:inline-block",
              
              switch(
                EXPR = model_prediction_class,
                "Flooding" = span(class="badge","Flooding",style="background-color:#dd4b39;position: relative; bottom: 5px; color:white;"),
                "Not Sure" = span(class="badge","Not Sure",style="background-color:#f39c12;position: relative; bottom: 5px; color:white;"),
                "No Flooding" = span(class="badge","No Flooding",style="background-color:#00a65a;position: relative;bottom: 5px;color:white;"),
                "Bad Image" = span(class="badge","Bad Image",style="background-color:#787878;position: relative;bottom: 5px;color:white;")
              )
                
      ),
          
          # Display Cam Image
          imageOutput(img_output_id,
                      height="100%"),
          
          # Datetime for image
          p(paste0("ML probability of ", model_prediction_class,": ", model_prediction_val,"%")),
          p(paste0("Time: ", lst_time, " EDT/EST")),
          
          # Inline boxes for user feedback
          div(style="display:inline-block",

              shinyWidgets::radioGroupButtons(inputId = radio_button_id,
                                              choiceNames = c("Flood", "Not Sure", "No Flood","Bad Image"),
                                              choiceValues = c("Flood", "Not Sure", "No Flood","Bad Image"),
                                              direction = "horizontal",
                                              width = '100%' ,
                                              individual = F,
                                              selected = character(0)
                                              # checkIcon = list(yes = icon("ok",lib="glyphicon"))
                                              )
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
  
  
  observe({
    walk(.x = camera_info$camera_name, .f = function(.x){
      output[[paste0(tolower(.x), "_selection")]] <- render_camera_ui(
        cam_name = .x,
        cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
        model_prediction = predict_reactive_list[[paste0(tolower(.x), "_predict_reactive")]]
      )
    })
  })
  
  
  ####____________________________####
  ####__  User Data Collection  __####
  
  
  #------------------ Reactive reset buttons ----------------
  
  #####__ 1. Reset supervised buttons  ####
  walk(.x = camera_info$camera_name, .f = function(.x){
    observeEvent(input[[paste0(tolower(.x),"_clear")]],{
      updateRadioGroupButtons(session = session,
                              inputId = paste0(tolower(.x),"_button_select"),
                              choiceNames  = c("Flood", "Not Sure", "No Flood","Bad Image"), 
                              choiceValues = c("Flood", "Not Sure", "No Flood","Bad Image"), 
                              selected = character(0), 
                              # checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                              )
    })
  })
  
  
  ###########  Reactive Button Info #######################
  walk(.x = camera_info$camera_name, .f = function(.x){
    observeEvent(c(input[[paste0(tolower(.x),"_button_select")]], input[[paste0(tolower(.x),"_clear")]]),{
      button_info_model1[[paste0(tolower(.x),"_button_info")]] <- input[[paste0(tolower(.x),"_button_select")]]
    })
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
  observeEvent(input$shinyalert == T,{
    req(input$shinyalert)
    
    shinyalert(
        title = "Submitting responses...",
        inputId = "submitting_alert",
        showConfirmButton =F,
        showCancelButton = F,
        closeOnEsc = F,
        animation = T
      )

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
        "filename"      = str_c(cam_name,"_",cam_time,".jpg"), 
        "model_score"   = model_prediction$prob,
        "model_class"   = model_prediction$label,
        "user_response" = ifelse(is.null(button_response), NA, button_response)
      )
    }
    
    # Create reactive list to hold all of user and model data
    data_reactive_list <- reactiveValues()
    
    walk(
      .x = camera_info$camera_name,
      .f = function(.x) {
        data_reactive_list[[paste0(tolower(.x), "_data")]] <-
          store_cam_data(
            cam_name = .x,
            cam_time = isolate(time_reactive_list[[paste0(tolower(.x), "_time_reactive")]]()),
            model_prediction = isolate(predict_reactive_list[[paste0(tolower(.x), "_predict_reactive")]]()),
            button_response = button_info_model1[[paste0(tolower(.x), "_button_info")]]
          )
        
      }
    )
    

    # Join tibbles of user and model data into one tibble
    data <- map_dfr(reactiveValuesToList(data_reactive_list), bind_rows)
    
    # Append data to google sheet
    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = data))

    # Write pictures to Google Drive
    purrr::map2(data$location, data$date, write_traffic_cam)

      shinyalert(
        inputId = "submitted_alert",
        title = "Submitted!",
        type = "success",
        immediate = T,
        animation = T,
        text = "Thanks for helping make the NC12 Flood CamML smarter!"
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
