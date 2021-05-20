library(dplyr)
library(lubridate)
library(readr)
library(httr)
library(magick)
library(tensorflow)
library(keras)
library(reticulate)

# For tensorflow w/shinyapps.io, need to add the line below:
# Sys.setenv(RETICULATE_PYTHON = '/usr/local/bin/python')

# Sys.setenv(RETICULATE_PYTHON = 'C:/python39')

model <- keras::load_model_tf("C:/GitHub/FloodCamMLShiny/R/Flood_CamML_App/ml/supervised")

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


# Reshape to correct dimensions (1, 224, 224, 3)
img_array <- keras::image_load("C:/Users/Adam Gold/Downloads/Mirlo.jpg",
                               target_size = c(224,224)) %>% 
  keras::image_to_array() %>% 
  standardize() %>%
  keras::array_reshape(., c(1, dim(.)))

# Model prediction. I think it outputs it as a list, so could convert with a simple "as.numeric()" or "c()"
prediction <- model %>% 
  predict(x = img_array) 

prediction



