import os
import urllib.request
import sched, time
import datetime

# Change the WD
os.chdir("/home/pi/Documents/NC_TCs")

#Mirlo, Ocracoke, and Hatteras URLs
#from   https://www.drivenc.gov/
Mirlo = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_MirloBeach.jpg"
Ocracoke = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_OcracokeNorth.jpg"
Hatteras = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NorthHatterasVillage.jpg"
Buxton = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_Buxton.jpg"
NewInlet = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_NewInlet.jpg"
Canal = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=NC12_CanalZone.jpg"
NorthDock = "https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_North_Dock.jpg"
SouthDock = "https://tims.ncdot.gov/TIMS/Cameras/viewimage.ashx?id=Hatteras_Inlet_South_Dock.jpg"
SouthOcracoke = "https://tims.ncdot.gov/tims/cameras/viewimage.ashx?id=Ocracoke_South.jpg"

#unused RN
RBNurl = "https://tims.ncdot.gov/TIMS/cameras/viewimage.ashx?id=RodantheBridgeNorth.jpg"
OSurl = "https://tims.ncdot.gov/tims/cameras/viewimage.ashx?id=Ocracoke_South.jpg"

#Minute delay
Min = 10

#The function for the cameras
def GetTrafficCam(URL,camera):

    # retrieve the image
    urllib.request.urlretrieve(URL, "dummy.jpg")

    #determine image name
    ImName = camera + '/' + str(datetime.datetime.now().strftime("%Y-%m-%d-%H-%M-%S")) + '-' + camera + '.jpg'

    #save image
    os.rename('dummy.jpg', ImName)

def LookAtTraffic():

    #print for debug
    print ("looking at traffic...%s" % datetime.datetime.now())

    #cameras:
    GetTrafficCam(Mirlo,'Mirlo')
    GetTrafficCam(Ocracoke,'Ocracoke')
    GetTrafficCam(Hatteras, 'Hatteras')
    GetTrafficCam(Buxton, 'Buxton')
    GetTrafficCam(Canal, 'Canal')
    GetTrafficCam(NewInlet, 'NewInlet')
    GetTrafficCam(NorthDock,'NorthDock')
    GetTrafficCam(SouthDock, 'SouthDock')
    GetTrafficCam(SouthOcracoke, 'SouthOcracoke')

#schedule
scheduler = sched.scheduler(time.time, time.sleep)
scheduler.enter(0, 1, LookAtTraffic, ())

#loop to make it go
while True:
    scheduler.run()
    scheduler.enter(Min*60, 1, LookAtTraffic, ())

