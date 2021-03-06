# Saxontropy
> Saxontropy (The use of maximum entropy to increase the informational content of hydrological networks by additional gauges)

This application written in R creates a Shiny-GUI allowing to use the Saxontropy tool.
This is a working example with modified data, which produces roughly the same results as with original data.

## Installation
- Install the newest R version with RStudio and Java JDK 1.2 or higher
- Set the working path *setwd(dir)*
- 'Run the line 8 to 19 to install all required packages'
- Download the newest Maxent file *maxent.jar* from here http://biodiversityinformatics.amnh.org/open_source/maxent/ (Maxent cannot be redistributed or used for commercial or for-profit purposes)
- Put the *maxent.jar*
- Run the command *system.file("java", package="dismo")* in R and put the *maxent.jar* into the returned folder
- Run the Saxontropy app by using the *Run App* button in RStudio

## Data Sources
- Maxent software for modeling species niches and distributions:  
http://biodiversityinformatics.amnh.org/open_source/maxent/

- Sächsisches Landesamt für Umwelt, Landwirtschaft und Geologie:  
https://www.umwelt.sachsen.de/umwelt/infosysteme/ida/index.xhtml

- Elevation Model and River Network HYDRO 1K DEM:  
https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-hydro1k?qt-science_center_objects=0#qt-science_center_objects
 
- Google Maps:  
https://console.cloud.google.com/home/dashboard

- OpenStreetMap (OSM):  
 https://www.openstreetmap.org

## Screenshot
> Graphical user interface
![Schreenshot 1](/Screenshot1.png?raw=true "Graphical user interface")  

> Graphical user interface
![Schreenshot 2](/Screenshot2.png?raw=true "Map of catchments of size 10-30 km² with their Probability of non-similarity")
