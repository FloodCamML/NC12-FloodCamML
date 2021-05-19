# Packages to load
library(dplyr)
library(lubridate)
library(readr)
library(httr)
library(magick)
library(tensorflow)
library(keras)


# For tensorflow w/shinyapps.io, need to add the line below:
Sys.setenv(RETICULATE_PYTHON = '/usr/local/bin/python')

# #Connect to google drive w/JSON key. For writing images to google drive and google sheets
# googledrive::drive_auth(path = Sys.getenv("GOOGLE_JSON_KEY"))

# Load model - need to change path
model <- keras::load_model_tf("ml/MN2_model_TB")

# URL of picture to download
URL = ""


# I feel like all of this could be combined as a function with an input URL (like in Evan's scraping python code in NCTrafficCameras). The code for this commented out below
# classify_image <- function(URL){

tmpfile <- tempfile(fileext = ".jpg")
tmpfile2 <- tempfile(fileext = ".jpg")

# retrieve the image. We will need to know the time it was downloaded, so maybe this can be achieved by updating a reactive values list, where each site has it's own "time" value
pic <- magick::image_read(URL)
# time <-  Sys.time() %>% lubridate::with_tz("UTC")

# Write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile". 
pic %>% 
  magick::image_write(tmpfile)

# Scale image to correct dimensions & write to other temp file. Could also use built-in keras functions
pic %>% 
  magick::image_scale(geometry = geometry_size_pixels(width=224,height=224,preserve_aspect = F)) %>% 
  magick::image_write(tmpfile2)

# Reshape to correct dimensions (1, 224, 224, 3)
img_array <- keras::image_load(tmpfile2) %>% 
  keras::image_to_array() %>% 
  keras::array_reshape(., c(1, dim(.)))

# Model prediction. I think it outputs it as a list, so could convert with a simple "as.numeric()" or "c()"
prediction <- model %>% 
  predict(x = img_array) 

# return(as.numeric(prediction))
# }
