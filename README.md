# NYC taxi data visualization and services using Shiny

## check out this app live on Shiny server: https://simonsong-first-shiny.shinyapps.io/nyc_taxi_map_traffic/


### background
* This is a project for my visualization course
    - It uses NYC taxi data for 8/2013
    - It combined with the daily weather and borough (nta) information to the original data
    - It uses R shiny as the main tools
    - Packages required in this App:
        * library(shiny)
        * library(leaflet) for map visualization
        * library(leaflet.extras) for traffic heatmap
        * library(osrm) for shortest path finding and calculations
        * library(shinyjs) for disable and enable shiny widgets
        * library(tidyverse) for basic data processing
        * library(nycgeo) for NYC borough and NTA data
        * library(sf) for manipulate GIS data
        * library(geosphere) for Haversine distance calculation

### functionalities support 
* Taxi traffic visualization
    1. Taxi traffic heatmap visualization (total and 24-hour in a sliding bar)
    ![heatmap](img/heatmap.png)
        - the sliderbar -1 means total traffic,
        - sliderbar 0-23 means 24 hours starting from midnight to 11pm
    2. Taxi traffic with borough and NTA visualization 
    ![borough](img/nta.png)
        - The two red NTAs are two airports
        - Manhattan has heavy taxi traffic too 
    3. Taxi traffic with drive speed visualization
    ![speed](img/speed.png)
        - Speed averaged by NTA region
        - The more red, the slower speed
        - most region in NYC has heavy traffic in average
* Taxi reservation info
    1. add and drop pin on NYC maps
    ![pin](img/add_drop_pin.png)
        - support random drop marker on the map
    2. Calculate shortest distance between two markers on NYC map
    ![shortest](img/shortest_path.png)
        - show shortest routes between two markers
    3. Recommend real cost, distance, and drive duration using taxi data along with ideal(free traffic) distance and duration.
    ![cost](img/cost_calc.png)
        - Once you dropped pin for start and destination, you can click the "reserve confirmed" button, it will show the following
            * show real and ideal route distance between two markers calculated from taxi data and "osrm" package, respectively 
            * show real and ideal drive duration between two markers from taxi data and "osrm" package, respectively
            * show estimated cost from taxi data
        - If your start and destination markers are not in the taxi database, click "reserve confirmed" button will only return the ideal distance and duration, the cost is "unknown" because of lack of data.
            * You can see from the image, there are two red dots near the start and destination points, respectively, those red dots are the real data from Taxi data

### Happy exploring! Star if you like it!
