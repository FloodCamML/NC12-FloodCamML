# FloodCamMLShiny

This repository houses the code for the [COPE COMET](https://copecomet.github.io/index.html) 
"sunny-day" flooding shiny application, the NC12 Flood CamML (camel). This project is currently in 
active development and will be updated with each new model release.

The published web application was built using {shiny}, is hosted via [shinyapps.io](https://www.shinyapps.io/) 
and can be found at [https://copecomet.shinyapps.io/Flood_CamML_App/](https://copecomet.shinyapps.io/Flood_CamML_App/)

<hr/>

## About the Flood CamML Project:

Flood CamML is an open source project funded by the NSF Coastlines and People program, and was 
completed over ~72 hours by a group of awesome scientists from across the country. 

**Our mission:** 
Develop a machine learning (ML) algorithm that can detect from a single image (NCDOT Camera Feed) 
whether or not a roadway is flooded.

**Why did we make Flood CamML?:** 
As scientists, we are interested in *how often* coastal roadways -- and the people that depend on 
these roadways -- are impacted by shallow (nuisance) flooding or ponding.
                     
It is *easy* for a human to look at a traffic camera and recognize whether a roadway is flooded, but
who has all day to look at web cameras? 


**Our questions:** 
Can we train a machine to detect flooding? Or can the machine train itself to detect flooding 
given enough images? Can we create an avenue for citizen science participation to foster community
engagement with science and improve model predictions?

**Why NC12?:** 
North Carolina Highway 12 (NC12) provides access to the Outer Banks, a chain of low-lying barrier
islands. Segments of NC12 are highly vulnerable to both storm and high-tide impacts, and when flooded,
isolate communities from the mainland. The NC Department of Transportation maintains a series of webcams
along NC12, which we utilize here!

**Instructions:** 
Please help us validate our ML models to identify flooded roadways!

 - *What do we mean by flooded?* -- Images should be labeled *"flooded"* if several
inches or more of water is on the roadway (typically recognizable by a sheen). Wet roadways should 
be classified as "not flooded".   

 - *What if you are not sure if the roadway is flooded, or if the image is blurred?* -- If you cannot
see the roadway in an image, or if you are not sure if flood waters are in the roadway or off to the
side, classify the image as "not sure"

