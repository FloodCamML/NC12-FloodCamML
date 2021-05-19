library(dplyr)
library(lubridate)
library(readr)
library(httr)
library(magick)

# set the working directory

# retrieve the image. We will need to know the time it was downloaded, so maybe this can be achieved by updating a reactive values list, where each site has it's own "time" value

# GetTrafficCam('Mirlo')
get_traffic_cam <- function(camera_name) {

    # from   https://www.drivenc.gov/
    Mirlo <- 'https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_MirloBeach.jpg'
    # Ocracoke <- "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_OcracokeNorth.jpg"
    # Hatteras <- "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NorthHatterasVillage.jpg"
    # Buxton <- "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_Buxton.jpg"
    # NewInlet <- "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NewInlet.jpg"
    # Canal <- "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_CanalZone.jpg"
    NorthDock <- "https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_North_Dock.jpg"
    SouthDock <- "https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_South_Dock.jpg"
    SouthOcracoke <- "https://tims.ncdot.gov/tims/cameras/viewimage.ashx?id=Ocracoke_South.jpg"
    RBNurl <- "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=RodantheBridgeNorth.jpg"

    tmpfile <- tempfile(fileext = ".jpg")  # this file will need to be saved
    #tmpfile2 <- tempfile(fileext = ".jpg")

    # retrieve the image
    pic <- magick::image_read(URL)
    time <-  Sys.time() %>% lubridate::with_tz("UTC")

    # write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile".
    pic %>% magick::image_write(tmpfile)

    # Scale image to correct dimensions & write to other temp file. Could also use built-in keras functions
    #pic %>%
    #    magick::image_scale(geometry = geometry_size_pixels(width=224,height=224,preserve_aspect = F)) %>%
    #    magick::image_write(tmpfile2)

    return(tmpfile)
}

    ##determine image name
    #ImName = camera + '/' + str(datetime.datetime.now().strftime("%Y-%m-%d-%H-%M-%S")) + '-' + camera + '.jpg'

    #save image
    #os.rename('dummy.jpg', ImName)




