####  Packages  ####
library(dplyr)
library(lubridate)
library(shiny)
library(markdown)
library(bs4Dash)
library(shinyjs)
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
library(purrr)
library(jsonlite)
library(readr)

#------------- Read in info ----------------
project_info <- readr::read_csv("./ui/project_info.csv") 
camera_info <- readr::read_csv("./ui/camera_info.csv") %>% 
  filter(use == T)
badges_info = NULL


####  Google Auth  ####

# Keys for Google Auth
source("keys/google_keys.R")

# load google authentications
folder_ID <- Sys.getenv("GOOGLE_FOLDER_ID")
sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")

googledrive::drive_auth(path = "keys/google_key.json")
googlesheets4::gs4_auth(token = googledrive::drive_token())

# Create temp directory for storing pictures
tmp_dir <- tempdir()

#------- camera list --------------------

# Lat and Long aren't currently in use but exist in the csv for later mapping
# Create layout info for UI

panel_data <- tibble("panels" = 1:length(camera_info$camera_name)) %>% 
  mutate("rows" = ceiling(panels/2),
         "position" = c(0, abs(diff(rows)-1)))

button_classes <- project_info %>% filter(variable == "button_classes") %>% pull(value) %>% stringr::str_split(.,pattern=",") %>% unlist()

## 1. Functions to load NCDOT Images ---------------------------------------------------------------------

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

# Function to Apply to Each Camera
get_cam <- function(cam_name){
  get_traffic_cam(cam_name)
}

time_reactive_list <- reactiveValues()

walk(.x = camera_info$camera_name, .f = function(.x){
  time_reactive_list[[paste0(tolower(.x),"_time_reactive")]] <- get_cam(.x)
})


waiting_screen <- tagList(
  spin_wave(),
  h4(paste0("Loading ", project_info %>% filter(variable == "title") %>% pull(value)))
)

# some javascript to make sidebar automatically go away after hitting tab name while on mobile

jsCode <- "shinyjs.init = function() {
  $(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia('only screen and (max-width: 768px)').matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});
}"

#------------------ UI definition ---------------------
ui <- bs4Dash::dashboardPage(
  
  # Title within browser tab
  title = project_info %>% filter(variable == "title") %>% pull(value),
  
  # Title within top navbar
  header = bs4Dash::dashboardHeader(
    border = F,
    fixed = T,
    .list = list(
      span(p(project_info %>% filter(variable == "title") %>% pull(value), style = "color:white;display:inline;font-size:1rem;"))
    )
  ),
  
  # Submit button in a fixed footer
  footer = dashboardFooter(fixed = T, 
                           left = actionButton(inputId = "submit", label = "SUBMIT", status = "success", style = "width:250px")),
  
  #####  Sidebar  ####
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 2,
    fixed = T,
    sidebarMenu(
      id = "nav",
      
      # Menu tabs within sidebar. Model tab is shown if use_model = T as declared in project_info.csv
      menuItem("Cameras", tabName = "Cameras", icon = icon("camera-retro")),
      menuItem("About the Project", tabName = "About", icon = icon("info-circle")),
      menuItem("Contact Us", tabName = "Contact", icon = icon("envelope"))
    )
  ),
  
  #####  Dashboard Body  ####
  dashboardBody(
    fluidPage(
      
      # Message dispalyed on screen when app times out (or errors)
      disconnectMessage(
        text = "Your session has timed out! Try refreshing the page.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#000000",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.25,
        width = 450,
        top = "center",
        size = 24,
        css = ""),
      shinyjs::useShinyjs(),
      extendShinyjs(text = jsCode, functions = c()),
      useShinyalert(),
      use_waiter(),
      waiter::waiter_preloader(html = waiting_screen, color = "#222d32"),
      
      # Adds logo (via the url in project_info.csv) to the browser tab. Loads style rules from .css
      tags$head(
        tags$link(rel = "shortcut icon", href = project_info %>% filter(variable == "logo_url") %>% pull(value)),
        includeCSS("flood-camml.css")
      ),
      
      ##### Tab Items  ####
      tabItems(
        
        #-------------- Camera tab ---------------------------
        tabItem(tabName = "Cameras",
                
                # First row shows subtitle, subtitle_description, and (if using a model) the badges. Info in project_info.csv and badges.csv
                fluidRow(
                  column(width=12,style="padding-left:7.5px;padding-right:7.5px;",
                         div(class="card",
                             div(class = "card-body",
                                 fluidRow(style= "align-content: center; align-items: center;",
                                          column(width=6,
                                                 h3(project_info %>% filter(variable == "subtitle") %>% pull(value)),
                                                 # h5(project_info %>% filter(variable == "subtitle_description") %>% pull(value)),
                                                 uiOutput(outputId = "badges")
                                          ),
                                          column(width=6,
                                                 uiOutput(outputId = "description")
                                          )
                                 )
                             )
                         )
                  )
                ),
                
                # Longform site info from text/site_info.md
                uiOutput(outputId = "site_info"),
                
                # Pictures (customize with camera_info.csv) with label buttons underneath
                uiOutput(outputId = "picture_panel")
        ),
        
        # -------------------- About tab ---------------------
        tabItem(tabName = "About",
                column(
                  width = 12,
                  div(class="card",
                      div(class = "card-body",
                          includeMarkdown("./text/about_project.md")
                      )
                  )
                )
        ),
        
        # ----------- Contact tab -----------------
        tabItem(tabName = "Contact",
                column(
                  width = 12,
                  div(class="card",
                      div(class = "card-body",
                          includeMarkdown("text/contact_us.md"),
                          a(href = "mailto:floodcamml@gmail.com", class = "pretty-link", "floodcamml@gmail.com")
                      )
                  )
                )
        )
      ),
      
      # Content at end of page that has copyright and link to CamML
      div(class = "footer-div-body",
          style = "text-align:center;",
          span(style="text-align:center;",p(paste0("Copyright © ",format(Sys.Date(), "%Y")," ",project_info %>% filter(variable == "organization") %>% pull(value),". Built with"), style= "display:inline;"),a("CamML", href = "https://floodcamml.github.io", class="pretty-link"))
      )
    )
  )
)


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
             inputId = "splash_page",
             closeOnEsc = T)
  
  
  #---------------- description render ---------------
  output$description <- renderUI({
    includeMarkdown("./text/description.md")
  })
  
  observeEvent(input$is_mobile_device, ignoreNULL = T, {
    output$site_info <- renderUI({
      box(width=12,
          id = "site_info_box",
          title= "Site Info",
          icon = icon("info-circle"),
          collapsible = T,
          collapsed = input$is_mobile_device,
          includeMarkdown("./text/site_info.md"))
    })
  })
  
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
  
  
  # Make sidebar disappear if viewing on mobile and menu item is clicked
  observeEvent(input$nav,{
    req(input$is_mobile_device == T)
    addClass(selector = "body", class = "sidebar-collapse")
    removeClass(selector = "body", class = "sidebar-open")
  })
  
  #-------------- Reactive Value Holders -------------
  
  # Create a reactive values object to hold label button values
  button_info <- reactiveValues()
  
  
  # Get Traffic Cam Images
  
  walk(.x = camera_info$camera_name, .f = function(.x){
    time_reactive_list[[paste0(tolower(.x),"_time_reactive")]] <- get_cam(.x)
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
  # takes the camera name and the reactive time
  render_camera_ui_no_model <- function(cam_name, cam_time, tzone = project_info %>% filter(variable == "tzone") %>% pull(value), tzone_alias = project_info %>% filter(variable == "tzone_alias") %>% pull(value),id_suffix = ""){
    
    cam_time_val <- cam_time
    lst_time <- format(cam_time_val %>% lubridate::with_tz(tzone), "%m/%d/%Y %H:%M")
    
    # string prep for naming patterns for UI elements
    # option to add suffix for "_unsupervised" ui elements
    name_lcase <- tolower(cam_name)
    img_output_id <- str_c(name_lcase, "_picture", id_suffix)
    radio_button_id <- str_c(name_lcase, "_button_select", id_suffix)
    button_clear <- str_c(name_lcase, "_clear", id_suffix)
    
    camera_button_ui <- renderUI({
      
      div(class = "col-sm-12", style="padding:0px;",
          div(class="card",
              div(class = "card-header", style="font-size: 1.25rem; display: flex; flex-direction: row; align-items: center; justify-content: flex-start; align-content: center; flex-wrap: nowrap;",
                  gsub("([a-z])([A-Z])", "\\1 \\2", cam_name)
              ),
              div(class="card-body",
                  div(style="text-align:center;",
                      
                      # Display Cam Image
                      imageOutput(img_output_id,
                                  height="100%"),
                      # Datetime for image
                      p(paste0("Time: ", lst_time," ", tzone_alias)),
                      
                      # Image label buttons
                      div(style="display:inline-block; ",
                          
                          shinyWidgets::radioGroupButtons(inputId = radio_button_id,
                                                          choiceNames = button_classes,
                                                          choiceValues = button_classes,
                                                          label=NULL,
                                                          selected = character(0))
                      ),
                      
                      # Clear selection button
                      div(style="display:inline-block",
                          actionButton(inputId = button_clear,
                                       label = "Clear",
                                       status = "secondary",
                                       style= "width:75px;"
                          )
                      )
                  )
              )
          )
      )
    })
    
    #return the UI
    return(camera_button_ui)
  }
  
    observe({
      walk(.x = camera_info$camera_name, .f = function(.x){
        output[[paste0(tolower(.x), "_selection")]] <- render_camera_ui_no_model(
          cam_name = .x,
          cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
        )
      })
    })
  
  
  ####____________________________####
  ####__  User Data Collection  __####
  
  
  #------------------ Reactive reset buttons ----------------
  
  #####__ 1. Reset supervised buttons  ####
  
  # Edit button choiceNames and choiceValues
  walk(.x = camera_info$camera_name, .f = function(.x){
    observeEvent(input[[paste0(tolower(.x),"_clear")]],{
      updateRadioGroupButtons(session = session,
                              inputId = paste0(tolower(.x),"_button_select"),
                              choiceNames  = button_classes, 
                              choiceValues = button_classes, 
                              selected = character(0)
      )
    })
  })
  
  
  ###########  Reactive Button Info #######################
  walk(.x = camera_info$camera_name, .f = function(.x){
    observeEvent(c(input[[paste0(tolower(.x),"_button_select")]], input[[paste0(tolower(.x),"_clear")]]),{
      button_info[[paste0(tolower(.x),"_button_info")]] <- input[[paste0(tolower(.x),"_button_select")]]
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
      animation = T,
      text = "Please do not close the page or click 'Back'"
    )
    
    updateActionButton(session = session,
                       inputId = "submit",
                       label = "SUBMITTED!", 
                       icon = icon("ok", lib = "glyphicon"))
    
    # disables submit button
    shinyjs::disable("submit")
    
    
    ######  Supervised Model Feedback  ####
    
    # Function to pull relevant camera data from models and feedback
    store_cam_data_no_model <- function(cam_name, cam_time,  button_response){
      cam_data <- tibble(
        "date"          = c(cam_time),
        "location"      = c(cam_name),
        "filename"      = str_c(cam_name,"_",cam_time,".jpg"), 
        "model_score"   = NA,
        "model_class"   = NA,
        "user_response" = ifelse(is.null(button_response), NA, button_response)
      )
    }
    
    # Create reactive list to hold all of user and model data
    data_reactive_list <- reactiveValues()
    
      walk(
        .x = camera_info$camera_name,
        .f = function(.x) {
          data_reactive_list[[paste0(tolower(.x), "_data")]] <-
            store_cam_data_no_model(
              cam_name = .x,
              cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
              button_response = button_info[[paste0(tolower(.x), "_button_info")]]
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
      text = project_info %>% filter(variable == "submit_success") %>% pull(value)
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
