library(magick)

# get_traffic_cam('Mirlo')
# get_traffic_cam('NorthDock')
# get_traffic_cam('SouthDock')
# get_traffic_cam('SouthOcracoke')
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

    tmpfile <- tempfile(fileext = ".jpg")  # this file will need to be sent to GoogleDrive eventually: do in Shiny?

    # retrieve the image
    pic <- magick::image_read(URL)
    #time <-  Sys.time() %>% lubridate::with_tz("UTC")

    # write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile".
    magick::image_write(pic, path = tmpfile, format = "jpg")
    magick::image_write(pic, path = paste(camera_name,'.jpg'), format = "jpg")

    return(tmpfile)
}




