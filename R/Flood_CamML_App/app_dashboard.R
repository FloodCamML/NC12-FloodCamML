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

# python path for shinyapps.io
Sys.setenv(RETICULATE_PYTHON = '/usr/local/bin/python')

# Adam G's python path
# Sys.setenv(RETICULATE_PYTHON = 'C:/python39')

# Keys for Google Auth
source("./R/Flood_CamML_App/google_keys.R")

sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")
folder_ID <- Sys.getenv("GOOGLE_FOLDER_ID")
google_json_path <- Sys.getenv("GOOGLE_JSON_PATH")

googledrive::drive_auth(path = google_json_path)
googlesheets4::gs4_auth(token = googledrive::drive_token())

## 1. Load Models ---------------------------------------------------------------------

# Path to model within Github folder
model <- keras::load_model_tf("./ml/supervised")

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
  img_array <- keras::image_load(paste0(camera_name,".jpg"),
                                 target_size = c(224,224)) %>% 
    keras::image_to_array() %>% 
    standardize() %>%
    keras::array_reshape(., c(1, dim(.)))
  
  # Model prediction. I think it outputs it as a list, so could convert with a simple "as.numeric()" or "c()"
  prediction <- model %>% 
    predict(x = img_array) 
  
  as.numeric(prediction)
}

# rendered_UI <- function(id, value){
#   img_output_name <- paste0(id,"_picture")
#   button_ID <- paste0(id,"_button_select")
#   clear_button_ID <-  paste0(id,"_clear")
#   
#   return(
#     div(width="100%",
#         style="background-color: #ffffff;
#         padding: 10px;
#         border-radius: 10px;
#         margin: 10px;",
#         # height=300,
#         align  = "center",
#         div(style="display:inline-block",
#             h2(value)),
#         div(style="display:inline-block",
#             uiOutput(outputId = paste0(id,"_selection"))),
#             
#         # Display Cam Image
#         imageOutput(img_output_name,
#                     height="100%"),
#       
#         # Inline boxes for user feedback
#         div(style="display:inline-block",
#             shinyWidgets::radioGroupButtons(inputId = button_ID,
#                                             choiceNames = c("Flooding", "Not Sure", "No Flooding"),
#                                             choiceValues = c("Flooding", "Not Sure", "No Flooding"),
#                                             justified = F,
#                                             selected = character(0),
#                                             checkIcon = list(yes = icon("ok",lib="glyphicon")))
#         ),
#         
#         # clear selection button
#         div(style="display:inline-block",
#             actionButton(inputId = clear_button_ID,
#                          label = "Clear",
#                          class = "btn btn-primary",
#                          style = "font-size:10pt;color:white")
#         )
#     )
#   )
# }

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
        css = ""
      ),
      shinyjs::useShinyjs(),
      useShinyalert(),
      use_waiter(),
      
      ####### CSS ############
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
                             tippy::tippy(span(class="badge","Flooding",style="background-color:#dd4b39;"),h4("This means that the model is more than ", strong("60%")," sure that there is water on the road")),
                             tippy::tippy(span(class="badge","Unsure",style="background-color:#f39c12;"),h4("This means that the model is between ", strong("40 - 60%")," sure that there is water on the road")),
                             tippy::tippy(span(class="badge","No Flooding",style="background-color:#00a65a;"),h4("This means that the model is less than ", strong("40%")," sure that there is water on the road"))
                      ),
                      column(width=6,
                             h4("Help us validate, click 'Flooding' or 'No Flooding'")
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
                         uiOutput(outputId = "mirlo_selection")
                  ),
                  
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
        tabItem(tabName = "Model2",
                
                #####_ Mod 2 Cams  ####
                fluidRow(
                  column(width=6,
                         box(width="100%",
                             height=300,
                             title = paste0("Test"),
                             solidHeader = T,
                             status = "warning",
                             h1("Test")
                         )
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


####_______________________________####
####  Server  ####

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #### Splash screen ####
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
  
  
  ####____________________________####
  
  #--------------- Get Cam Images ----------------------
  # These reactives get the camera images and are on a timer of 5 minutes
  
  # Mirlo Cam
  mirlo_time_reactive <- reactive({
    invalidateLater(millis = 5*60*1000, session = session)
    get_traffic_cam("Mirlo")
  })
  
  # Northdock cam
  northdock_time_reactive <- reactive({
    invalidateLater(millis = 5*60*1000, session = session)
    get_traffic_cam("NorthDock")
  })
  
  # Southdock cam
  southdock_time_reactive <- reactive({
    invalidateLater(millis = 5*60*1000, session = session)
    get_traffic_cam("SouthDock")
  })
  
  # SouthOcracoke cam
  southocracoke_time_reactive <- reactive({
    invalidateLater(millis = 5*60*1000, session = session)
    get_traffic_cam("SouthOcracoke")
  })
  
  #----------- Get model predictions -------------
  # Get from model using pre-defined function above the UI
  
  mirlo_predict <- reactive({
    predict_flooding("Mirlo")
  })
  
  northdock_predict <- reactive({
    predict_flooding("NorthDock")
  })
  
  southdock_predict <- reactive({
    predict_flooding("SouthDock")
  })
  
  southocracoke_predict <- reactive({
    predict_flooding("SouthOcracoke")
  })
  
  
  #--------------- Wait screen spinners ----------------------
  # Create a wait screen for each picture. Need one of these for each site
  
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
  
  
  #--------- Render images ----------
  # Mirlo
  output$mirlo_picture <- renderImage({
    outfile <- "Mirlo.jpg"
    list(src= outfile,
         alt = "Mirlo Beach",
         width = "100%"#, height="180px"
    )
  }, deleteFile=F)
  
  # Northdock
  output$northdock_picture <- renderImage({
    outfile <- "NorthDock.jpg"
    
    list(src= outfile,
         alt = "NorthDock",
         width = "100%" #, height="180px"
    )
  }, deleteFile=F)
  
  # SouthDock
  output$southdock_picture <- renderImage({
    outfile <- "SouthDock.jpg"
    
    list(src= outfile,
         alt = "SouthDock",
         width = "100%" #, height="180px"
    )
  }, deleteFile=F)
  
  # SouthOcracoke
  output$southocracoke_picture <- renderImage({
    outfile <- "SouthOcracoke.jpg"
    
    list(src= outfile,
         alt = "SouthOcracoke",
         width = "100%" #, height="180px"
    )
  }, deleteFile=F)
  
  
  #--------------- Reactive UI ----------------------
  # Function to render UI
  rendered_UI <- function(id, value, predict_reactive, time_reactive){
    predict_reactive_val <- predict_reactive
    time_reactive_val <- time_reactive
    
    img_output_name <- paste0(id,"_picture")
    lst_time <- time_reactive_val %>% lubridate::with_tz("America/New_York")
    button_ID <- paste0(id,"_button_select")
    clear_button_ID <-  paste0(id,"_clear")
    
    return(
      div(width="100%",
          style="background-color: #ffffff;
            padding: 10px;
            border-radius: 10px;
            margin: 10px 0;",
          # height=300,
          align  = "center",
          div(style="display:inline-block",
              h2(gsub("([a-z])([A-Z])", "\\1 \\2", value))),
          div(style="display:inline-block",
              if(predict_reactive_val > 0.6){
                span(class="badge","Flooding",style="background-color:#dd4b39;
             position: relative;
             bottom: 5px;
             color:white;")
              }
              
              else if(predict_reactive_val > 0.4 & predict_reactive_val <= 0.6){
                span(class="badge","Unsure",style="background-color:#f39c12;
             position: relative;
             bottom: 5px;
             color:white;")
              }
              
              else if(predict_reactive_val <= 0.4 ){
                span(class="badge","No Flooding",style="background-color:#00a65a;
             position: relative;
             bottom: 5px;
             color:white;")
              }),
          
          # Display Cam Image
          imageOutput(img_output_name,
                      height="100%"),
          
          # Datetime for image
          p(paste0(lst_time, " EDT/EST")),
          
          # Inline boxes for user feedback
          div(style="display:inline-block",
              shinyWidgets::radioGroupButtons(inputId = button_ID,
                                              choiceNames = c("Flooding", "Not Sure", "No Flooding"),
                                              choiceValues = c("Flooding", "Not Sure", "No Flooding"),
                                              justified = F,
                                              selected = character(0),
                                              checkIcon = list(yes = icon("ok",lib="glyphicon")))
          ),
          
          # clear selection button
          div(style="display:inline-block",
              actionButton(inputId = clear_button_ID,
                           label = "Clear",
                           class = "btn btn-primary",
                           style = "font-size:10pt;color:white")
          )
      )
    )
  }
  
  # Render for each site
  # mirlo
  output$mirlo_selection <- renderUI({
    w$show()
    rendered_UI(id = "mirlo",
                value = "Mirlo",
                predict_reactive = isolate(mirlo_predict()),
                time_reactive = isolate(mirlo_time_reactive()))
  })
  
  # # Northdock
  output$northdock_selection <- renderUI({
    w2$show()
    rendered_UI(id = "northdock",
                value = "NorthDock",
                predict_reactive = isolate(northdock_predict()),
                time_reactive = isolate(northdock_time_reactive()))
  })
  
  # SouthDock
  output$southdock_selection <- renderUI({
    w3$show()
    rendered_UI(id = "southdock",
                value = "SouthDock",
                predict_reactive = isolate(southdock_predict()),
                time_reactive = isolate(southdock_time_reactive()))
  })
  
  
  # SouthOcracoke
  output$southocracoke_selection <- renderUI({
    w4$show()
    rendered_UI(id = "southocracoke",
                value = "SouthOcracoke",
                predict_reactive = isolate(southocracoke_predict()),
                time_reactive = isolate(southocracoke_time_reactive()))
  })
  
  
  #------------------ Clear selection buttons ----------------
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
  
  
  #----------- Tracking user selection ----------------
  
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
  
  # 1. mirlo
  mirlo_data <- reactive({
    req(mirlo_time_reactive(), mirlo_predict())
    
    tibble(
      "date"          = isolate(mirlo_time_reactive()),
      "location"      = c("Mirlo"),
      "filename"      = c("Mirlo.jpg"),
      "model_type"    = "supervised",
      "model_score"   = isolate(mirlo_predict()),
      "model_class"   = isolate(mirlo_predict()),
      "user_response" = button_info_model1$mirlo_button_info
    )
  })
  
  # 2.northdock
  northdock_data <- reactive({
    req(northdock_time_reactive(), northdock_predict())
    
    tibble(
      "date"          = isolate(northdock_time_reactive()),
      "location"      = c("NorthDock"),
      "filename"      = c("NorthDock.jpg"),
      "model_type"    = "supervised",
      "model_score"   = isolate(northdock_predict()),
      "model_class"   = isolate(northdock_predict()),
      "user_response" = button_info_model1$northdock_button_info
    )
  })
  
  # 3. southdock
  southdock_data <- reactive({
    req(southdock_time_reactive(), southdock_predict())
    
    tibble(
      "date"          = isolate(southdock_time_reactive()),
      "location"      = c("SouthDock"),
      "filename"      = c("SouthDock.jpg"),
      "model_type"    = "supervised",
      "model_score"   = isolate(southdock_predict()),
      "model_class"   = isolate(southdock_predict()),
      "user_response" = button_info_model1$southdock_button_info
    )
    
  })
  
  # 4. southocracoke
  southocracoke_data  <- reactive({
    req(southocracoke_time_reactive(), southocracoke_predict())
    
    tibble(
      "date"          = isolate(southocracoke_time_reactive()),
      "location"      = c("SouthOcracoke"),
      "filename"      = c("SouthOcracoke.jpg"),
      "model_type"    = "supervised",
      "model_score"   = isolate(southocracoke_predict()),
      "model_class"   = isolate(southocracoke_predict()),
      "user_response" = button_info_model1$southocracoke_button_info
    )
  })
  
  
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
    
    final_data <- rbind(mirlo_data(),
                        northdock_data(),
                        southdock_data(),
                        southocracoke_data())
    
    # Append data to google sheet
    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = final_data))
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)