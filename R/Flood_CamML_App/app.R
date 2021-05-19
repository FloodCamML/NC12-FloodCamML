


# Packages  ---------------------------------------------------------------------
library(shiny)
library(shinymaterial)
library(datasets)
library(keras)
library(magick)
library(tidyverse)



####_____________________________####
####  Load Workspace  ####


## 1. Load Models ---------------------------------------------------------------------







## 2. Load NCDOT Images ---------------------------------------------------------------------





## 3. Classify Images ---------------------------------------------------------------------




####_____________________________####
# UI ---------------------------------------------------------------------
ui <- material_page(
    # Application title
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
                title = "About the App",
                h2("Subheader"),
                p("Paragraph explanation.")
            )
        )
    ),
    
    
    ####  Define tabs  ####
    material_tabs(
        tabs = c(
            "Sunny Day Flooding"      = "first_tab",
            "Model 1 Classifications" = "second_tab",
            "Model 2 Classifications" = "third_tab"
        )
    ),
    
    
    #### Tab 1 Content - Background on Flooding  ####
    material_tab_content(
        tab_id = "first_tab",
        material_card(
            title = "About High Tide Flooding",
            plotOutput("world_map")
        ),
    ),
    
    
    
    
    #### Tab 2 Content - Model 1 Results  ####
    material_tab_content(
        tab_id = "second_tab",
        ###_ Row 1 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Okracoke",
                    plotOutput("faithfulPlot")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Nags Head",
                    plotOutput("irisPlot")
                )
            )
        ),
        
        ###_ Row 2 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Duck",
                    plotOutput("mtcarsPlot")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Carolla",
                    plotOutput("airmilesPlot")
                )
            )
        ),
        
        
        ###_ Row 3 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Kittyhawk",
                    plotOutput("mtcarsPlot")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Avon",
                    plotOutput("airmilesPlot")
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
                    title = "Okracoke",
                    plotOutput("faithfulPlot")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Nags Head",
                    plotOutput("irisPlot")
                )
            )
        ),
        
        ###_ Row 2 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Duck",
                    plotOutput("mtcarsPlot")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Carolla",
                    plotOutput("airmilesPlot")
                )
            )
        ),
        
        
        ###_ Row 3 ----------------------------------
        material_row(
            material_column(
                width = 6,
                material_card(
                    title = "Kittyhawk",
                    plotOutput("mtcarsPlot")
                )
            ),
            material_column(
                width = 6,
                material_card(
                    title = "Avon",
                    plotOutput("airmilesPlot")
                )
            )
        )
    ),
   
    
    
    
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
    output$mtcarsPlot <- renderPlot({
        plot(mtcars)
    })
    output$irisPlot <- renderPlot({
        plot(iris)
    })
    
    output$faithfulPlot <- renderPlot({
        plot(faithful)
    })
    output$airmilesPlot <- renderPlot({
        plot(airmiles)
    })
}

shinyApp(ui = ui, server = server)
