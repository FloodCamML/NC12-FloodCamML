
# NC-12 FloodCamML

This repository houses code for a shiny web application that uses a machine learning model to classify webcam images for flooding and collect training images and labels from users. 

Created during the [COPE COMET](https://copecomet.github.io/index.html), this app is known as the [NC12 Flood CamML](https://github.com/FloodCamML/NC12-FloodCamML). 

The published web application is: 
* built with R using {[shiny](https://github.com/rstudio/shiny)}
* writing data to Google Sheets & images to Google Drive
* containerized with [Docker](https://www.docker.com/)
* hosted with [Google Cloud Run](https://cloud.google.com/run)


## Instructions

See the [Flood CamML website](https://floodcamml.org/docs/intro) for detailed instructions on how to make your own versions of this app. There are two corresponding code templates (deployed with [Google Cloud Run](https://github.com/FloodCamML/FloodCamML_cloudrun) and [shinyapps.io](https://github.com/FloodCamML/FloodCamML_shinyapps)) and a tutorial that will explain how to use the code templates, set up the necessary Google APIs/permissions, and deploy the app.

## See the app in action

The NC-12 Flood CamML is available at [nc12.floodcamml.org](https://nc12.floodcamml.org/)

## About the CamML Project

CamML is an open source project for crowd labeling and ML prediction of real-time webcam imagery. See the full project description at [floodcamml.org](https://floodcamml.org/).

