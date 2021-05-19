


# Packages  ---------------------------------------------------------------------
library(shiny)
library(shinymaterial)
library(datasets)
#library(keras)
#library(magick)
library(tidyverse)



####_____________________________####
####  Load Workspace  ####

# # For tensorflow w/shinyapps.io, need to add the line below:
# Sys.setenv(RETICULATE_PYTHON = '/usr/local/bin/python')



## 1. Load Models ---------------------------------------------------------------------







## 2. Load NCDOT Images ---------------------------------------------------------------------







## 3. Classify Images ---------------------------------------------------------------------




####_____________________________####
# UI ---------------------------------------------------------------------
ui <- material_page(
   
     # Application title - color details
    title = "NCDOT FloodCamML", 
    include_nav_bar = FALSE,
    primary_theme_color = "#00695c", 
    secondary_theme_color = "#00796b",
    
    
    
    ## Header Image ----------------------------------
    # Image in folder 'www' at same level as app.R
    material_parallax(
        # image source: https://obxstuff.com/pages/things-to-do
        image_source = "outer_banks_aerial.jpeg"
    ),
    
    
    ## App Description ----------------------------------
    material_row(
        material_column(
            width = 12,
            material_card(
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
    
    
    ####  Define Different Tabs  ####
    material_tabs(
        tabs = c(
            "Nuisance Flooding"       = "first_tab",
            "Model 1 Classifications" = "second_tab",
            "Model 2 Classifications" = "third_tab"#,
        )
    ),
    
    
    ### Tab 1 Content - Background on Flooding  ####
    material_tab_content(
        tab_id = "first_tab",

        # Content Card
        material_card(
            h3("What is Nuisance and High Tide Flooding?"),
            p("During extremely high tides, the sea literally spills onto land in some locations, inundating low-lying areas
                   with seawater until high tide has passed. Because this flooding causes public inconveniences such as road closures
                   and overwhelmed storm drains, the events were initially called nuisance flooding.
                   To help people understand the cause of these events, they are now referred to as high-tide floods."),
            a(href = "https://toolkit.climate.gov/topics/coastal-flood-risk/shallow-coastal-flooding-nuisance-flooding",
              "Source")
        )
    ),

    
    
    
    #### Tab 2 Content - Model 1 Results  ####
    material_tab_content(
        tab_id = "second_tab",
        
        ###_ Row 1 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Ocracoke",
                    plotOutput("cam1_img"),
                    actionButton(inputId = "m1_cam1_flood", label = "Flooding"),
                    actionButton(inputId = "m1_cam1_dry", label = "Not-Flooding")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Mirlo",
                    plotOutput("cam2_img"),
                    actionButton(inputId = "m1_cam2_flood", label = "Flooding"),
                    actionButton(inputId = "m1_cam2_dry", label = "Not-Flooding")
                )
            )
        ),
        
        ###_ Row 2 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Hatteras",
                    plotOutput("cam3_img"),
                    actionButton(inputId = "m1_cam3_flood", label = "Flooding"),
                    actionButton(inputId = "m1_cam3_dry", label = "Not-Flooding")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Buxton",
                    plotOutput("cam4_img"),
                    actionButton(inputId = "m1_cam4_flood", label = "Flooding"),
                    actionButton(inputId = "m1_cam4_dry", label = "Not-Flooding")
                )
            )
        ),
        
        ###_ Row 3 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Canal",
                    plotOutput("cam5_img"),
                    actionButton(inputId = "m1_cam5_flood", label = "Flooding"),
                    actionButton(inputId = "m1_cam5_dry", label = "Not-Flooding")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Avon",
                    plotOutput("cam6_img"),
                    actionButton(inputId = "m1_cam6_flood", label = "Flooding"),
                    actionButton(inputId = "m1_cam6_dry", label = "Not-Flooding")
                )
            )
        )
    ),
    
    
    
    
    #### Tab 3 Content - Model 1 Results  ####
    
    material_tab_content(
        tab_id = "third_tab",

        ###_ Row 1 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Ocracoke",
                    plotOutput("cam1_img"),
                    actionButton(inputId = "m2_cam1_flood", label = "Flooding"),
                    actionButton(inputId = "m2_cam1_dry", label = "Not-Flooding")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Mirlo",
                    plotOutput("cam2_img"),
                    actionButton(inputId = "m2_cam2_flood", label = "Flooding"),
                    actionButton(inputId = "m2_cam2_dry",   label = "Not-Flooding")
                )
            )
        ),

        ###_ Row 2 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Hatteras",
                    plotOutput("cam3_img"),
                    actionButton(inputId = "m2_cam3_flood", label = "Flooding"),
                    actionButton(inputId = "m2_cam3_dry", label = "Not-Flooding")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Buxton",
                    plotOutput("cam4_img"),
                    actionButton(inputId = "m2_cam4_flood", label = "Flooding"),
                    actionButton(inputId = "m2_cam4_dry", label = "Not-Flooding")
                )
            )
        ),


        ###_ Row 3 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Canal",
                    plotOutput("cam5_img"),
                    actionButton(inputId = "m2_cam5_flood", label = "Flooding"),
                    actionButton(inputId = "m2_cam5_dry", label = "Not-Flooding")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Avon",
                    plotOutput("cam6_img"),
                    actionButton(inputId = "m2_cam6_flood", label = "Flooding"),
                    actionButton(inputId = "m2_cam6_dry",   label = "Not-Flooding")
                )
            )
        )
    ), # Close Tab content 3
   
    
    
    
    ## Tail Image ----------------------------------
    # Image in folder 'www' at same level as app.R
    material_parallax(
        # image source: https://obxstuff.com/pages/things-to-do
        image_source = "outer_banks_aerial.jpeg"
    )
)







####_____________________________####

# SERVER ---------------------------------------------------------------------
server <- function(input, output) {

    
    ####  Model 1 Classifications  ####
    
    #####_  Mod 1 - Pic 1  ####
    output$cam1_img <- renderPlot({
        plot(faithful)
    })

    
    #####_  Mod 1 - Pic 2  ####
    output$cam2_img <- renderPlot({
        plot(airmiles)
    })
    
    
    #####_  Mod 1 - Pic 3  ####
    output$cam3_img <- renderPlot({
        plot(faithful)
    })

    #####_  Mod 1 - Pic 4  ####
    output$cam4_img <- renderPlot({
        plot(airmiles)
    })
    
    #####_  Mod 1 - Pic 5  ####
    output$cam5_img <- renderPlot({
        plot(airmiles)
    })

    #####_  Mod 1 - Pic 6  ####
    output$cam6_img <- renderPlot({
        plot(airmiles)
    })
    
    
    ####  Model 2 Classifications  ####
    
    # #####_  Mod 2 - Pic 1  ####
    # output$cam1_img <- renderPlot({
    #     plot(mtcars)
    # })
    # 
    # 
    # #####_  Mod 2 - Pic 2  ####
    # output$cam2_img <- renderPlot({
    #     plot(iris)
    # })
    # 
    # 
    # #####_  Mod 2 - Pic 3  ####
    # output$cam3_img <- renderPlot({
    #     plot(faithful)
    # })
    # 
    # #####_  Mod 2 - Pic 4  ####
    # output$cam4_img <- renderPlot({
    #     plot(airmiles)
    # })
    # 
    # #####_  Mod 2 - Pic 5  ####
    # output$cam5_img <- renderPlot({
    #     plot(airmiles)
    # })
    # 
    # #####_  Mod 2 - Pic 6  ####
    # output$cam6_img <- renderPlot({
    #     plot(airmiles)
    # })
    
    
    
}


# Run the Application
shinyApp(ui = ui, server = server)
