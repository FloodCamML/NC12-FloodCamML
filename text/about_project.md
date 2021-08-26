## About the CamML Project

CamML is an open source project for crowd labeling and ML prediction of real-time webcam imagery. Here, we present 
the first CamML framework: the NC12 Flood CamML. The model and website was created in 72 hours in May 2021 during the 
NSF sponsored **C**oastlines and people **O**pen data and **M**achin**E** learning sprin**T** by a 
group of awesome coastal scientists from across the USA (see full list below). 

**Why did we make the NC12 Flood CamML?:** 
As coastal scientists, we are interested in *how often* coastal roadways -- and the people that depend on 
these roadways -- are impacted by flooding or ponding.
                     
It is *easy* for a human to look at a traffic camera and recognize whether a roadway is flooded, but
who has all day to look at web cameras? 

**Our mission:** 
Develop a machine learning (ML) model that can determine from a single image if a roadway is flooded.

**Why NC12?:** 
North Carolina Highway 12 (NC12) provides access to the Outer Banks, a chain of low-lying barrier
islands. Segments of NC12 are highly vulnerable to both storm and high-tide impacts, and when flooded,
isolate communities from the mainland. The NC Department of Transportation maintains a series of webcams
along NC12 at [DriveNC.gov](https://drivenc.gov), which we utilize here.

**Instructions:** 
The ML model has been trained using images from NC12 webcams during Hurricane Teddy in 2019. While the model does a good job at predicting roadway flooding, it was trained using images from only one storm. We think it can be better, but we need more labeled images! 

When classifying the images, keep these things in mind:
 - **What do we mean by flooded?** Images should be labeled `Flooding` if several inches or more of water is on the roadway (typically recognizable by a sheen or reflection).
 - **If the roadway is just wet** -- i.e., there is no depth to the water on the road -- the image should be classified as `No Flooding`.
 - **What if there is water on the lens, the image is very blurred, or the camera isn't working?** If you cannot see the roadway in an image, images should be labeled `Bad Image`.
 - **If you are not sure if flood waters are in the roadway or off to the side of the roadway**, classify the image as `Not Sure`.

**Contributors (in alphabetical order):** 
- *Katherine Anarde* - North Carolina State University
- *Adam Gold* - University of North Carolina - Chapel Hill
- *Evan Goldstein* - University of North Carolina - Greensboro
- *Adam Kemberling* - Gulf of Maine Research Institute
- *Carter Smith* - Duke University
- *Christine Wei* - University of North Carolina - Chapel Hill
- *Emory Wellman* - University of Florida
