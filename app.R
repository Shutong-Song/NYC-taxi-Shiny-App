
library(shiny)
library(leaflet)
library(leaflet.extras)
library(osrm)
library(shinyjs)
library(tidyverse)
library(nycgeo)
library(sf)
library(geosphere)

# 1. read data and preprocessing: 200,000 rows of total 12,597,109
## used NYC taxi data from 2013-8, random select 200,000 rows of total 12,597,109 rows

## import data and util helper functions
source("util.r")
df_nta_merged <- readr::read_csv("dataset/df_nta_merged.csv")
traffic_weather <- readr::read_csv("dataset/traffic_weather.csv")
trip_speed <- sf::st_read("dataset/trip_speed.shp")
pop_traffic <- sf::st_read("dataset/pop_traffic.shp")


## ui
ui <- fluidPage(
    useShinyjs(),
    navbarPage("taxi App",
               tabPanel("taxi traffic",
                        sidebarLayout(
                            sidebarPanel(
                                h3("Traffic by"),
                                radioButtons("type_vis", label="select by types", choices = list("boroughs"=3, "speed"=2, "hourly"=1),selected = 1),
                                hr(),
                                sliderInput("hour_slider", label = "by hour traffic", min = -1, max = 23, value = -1, step = 1),
                                width = 3),
                            mainPanel(
                                leafletOutput("mymap", width = 1400, height = 800),
                                # hanging small plot window
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 70, left = "auto", right = 15, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h3("Summary traffic"),
                                              plotOutput("daily_traffic", height = 250),
                                              plotOutput("hourly_traffic", height = 200)
                                )
                            ))),
               tabPanel("taxi reservation",
                        sidebarLayout(
                            sidebarPanel(
                                h5("start location:"),
                                textInput("src_loc", label = "", value = "start - drop a pin"),
                                hr(),
                                h5("destination location:"),
                                textInput("dst_loc", label = "", value = "end - drop a pin"),
                                actionButton("clear_locs", "clear locations"),
                                hr(),
                                h5("est. distance (mile):"),
                                textInput("show_distance", label = "", value = ""),
                                h5("est. duration (minute):"),
                                textInput("show_duration", label = "", value = ""),
                                h5("est. fare amount (dollar):"),
                                textInput("show_fare", label = "", value = ""),
                                actionButton("reserve_confirm", "reserve confirmed!", class = "btn-warning"),
                                width = 3),
                            mainPanel(
                                leafletOutput("reserve_map", width = 1400, height = 800)
                            )))
    ))


## server
server <- function(input, output, session){
    # 1. traffic map for boroughs
    # intial map
    poppal <- colorNumeric(palette = colorRampPalette(c("gray","blue", "red"))(length(pop_traffic$n)),
                           domain = pop_traffic$n)
    output$mymap <- renderLeaflet({
        mymap <- leaflet(sf::st_transform(pop_traffic, crs="EPSG:4326")) %>%
            setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
            clearHeatmap() %>%
            addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(noWrap = TRUE)) %>%
            addPolygons(stroke = FALSE, color = ~poppal(n), fillOpacity = 0.7)
    })
    
    # control radio buttons 
    observeEvent(input$type_vis, {
        if(input$type_vis == 3)
        {
            shinyjs::disable("hour_slider")
            poppal <- colorNumeric(palette = colorRampPalette(c("gray","blue","red"))(length(pop_traffic$n)),
                                   domain = pop_traffic$n)
            leafletProxy("mymap", data = sf::st_transform(pop_traffic, 4326)) %>%
                clearShapes() %>%
                clearHeatmap() %>%
                addPolygons(stroke = FALSE, color = ~poppal(n), fillOpacity = 0.7)
        }else if(input$type_vis == 2)
        {
            shinyjs::disable("hour_slider")
            pal_speed <- colorNumeric(palette = colorRampPalette(c("red","blue", "gray"))(length(trip_speed$avg)),domain = trip_speed$avg)
            leafletProxy("mymap", data = st_transform(trip_speed, 4326)) %>%
                clearShapes() %>%
                clearHeatmap() %>%
                addPolygons(stroke = FALSE, color = ~pal_speed(avg), fillOpacity = 0.7)
            
        }else if(input$type_vis == 1)
        {
            shinyjs::enable("hour_slider")
            updateSliderInput(session, "hour_slider", value = -1)
            leafletProxy("mymap", data = df_nta_merged) %>%
                clearShapes() %>%
                addHeatmap(
                    lng = ~pickup_longitude,
                    lat = ~pickup_latitude,
                    max = 5,
                    radius = 3.5,
                    blur = 10)
        }
    })
    
    # control slider bar
    observeEvent(input$hour_slider, {
        if(input$hour_slider == -1)
        {
            leafletProxy("mymap", data = df_nta_merged) %>%
                clearShapes() %>%
                clearHeatmap() %>%
                addHeatmap(
                    lng = ~pickup_longitude,
                    lat = ~pickup_latitude,
                    max = 5,
                    radius = 3.5,
                    blur = 10)
        }else
        {
            leafletProxy("mymap", data = df_nta_merged[df_nta_merged$pickup_datetime_hourly == input$hour_slider,]) %>%
                clearShapes() %>%
                clearHeatmap() %>%
                addHeatmap(
                    lng = ~pickup_longitude,
                    lat = ~pickup_latitude,
                    max = 5,
                    radius = 3.5,
                    blur = 10)
        }
    })
    
    ## hanging plot control
    output$daily_traffic <- renderPlot({
        traffic_weather %>% ggplot(aes(x = by_day, y = n)) +
            geom_col(aes(fill = PRCP)) +
            scale_fill_viridis_c() +
            labs(x = "day in month", y = "total trips") +
            theme_bw() +
            theme(legend.position = "bottom")
        
    })
    
    output$hourly_traffic <- renderPlot({
        
        df_nta_merged %>% ggplot(aes(x = as.factor(pickup_datetime_hourly))) +
            geom_bar() +
            labs(x = "hourly", y = "total trips") +
            theme_bw()
    })
    
    #reserve taxi page
    output$reserve_map <- renderLeaflet({
        reserve_map <- leaflet(df_nta_merged) %>% setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
            addTiles()
    })
    
    click_cnt <- reactiveValues(cnt = 0)
    
    observeEvent(input$reserve_map_click,{
        click_cnt$cnt <- click_cnt$cnt + 1
        click <- input$reserve_map_click
        if(click_cnt$cnt == 1)
        {
            leafletProxy("reserve_map") %>% addMarkers(lng = click$lng, lat = click$lat)
            updateTextInput(session, "src_loc", value = paste0("(",click$lng, ", ",click$lat,")"))
            
        }else
        {
            updateTextInput(session, "dst_loc", value = paste0("(",click$lng, ", ",click$lat,")"))
            ## add shortest route on maps 
            kstart <- unlist(str_split(input$src_loc, ","))
            start_lon <- as.double(substring(kstart[1],2))
            start_lat <- as.double(substring(kstart[2],1,nchar(kstart[2])-1))
            route <- osrmRoute(src = c(start_lon, start_lat), dst = c(click$lng, click$lat), overview = "full")#,returnclass = "sf")
            leafletProxy("reserve_map") %>% clearGroup("new_marker") %>% 
                clearShapes() %>%
                addMarkers(lng = click$lng, lat = click$lat, group = "new_marker") %>%
                addPolylines(lng = route$lon, lat=route$lat)
            #updateTextInput(session, "show_fare", value = paste0("Estimate fare amount is: ",click$lng, ", ",click$lat,")"))
        }
    })
    
    observeEvent(input$clear_locs, {
        leafletProxy("reserve_map") %>% clearMarkers() %>% clearShapes()#clearPolylines()
        updateTextInput(session, "src_loc", value = "start - drop a pin")
        updateTextInput(session, "dst_loc", value = "end - drop a pin")
        click_cnt$cnt <- 0
    })
    
    ## reservation action button control
    observeEvent(input$reserve_confirm, {
        # 1. get start lon and lat
        loc_start <- unlist(str_split(input$src_loc, ", "))
        loc_start_lon <- as.double(substring(loc_start[1], 2)) 
        loc_start_lat <- as.double(substring(loc_start[2], 1, nchar(loc_start[2])-1))
        
        # 2. get end lon and lat
        loc_end <- unlist(str_split(input$dst_loc, ", "))
        loc_end_lon <- as.double(substring(loc_end[1], 2)) 
        loc_end_lat <- as.double(substring(loc_end[2], 1, nchar(loc_end[2])-1))
        
        # 3. get start nta and end nta
        start_point <- data.frame(lon = loc_start_lon, lat = loc_start_lat) %>%
            st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
        end_point <- data.frame(lon = loc_end_lon, lat = loc_end_lat) %>%
            st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
        nyc_bound <- nyc_boundaries(geography = "tract")
        start_point_trans <- st_transform(start_point, 2163)
        end_point_trans <- st_transform(end_point, 2163)
        nyc_trans <- st_transform(nyc_bound, 2163)
        start_nta <- sf::st_join(start_point_trans, nyc_trans) %>% pull(nta_id)
        end_nta <- sf::st_join(end_point_trans, nyc_trans) %>% pull(nta_id)
        
        # 4. calculate start end distance and duration (ideal values)
        source <- c(loc_start_lon, loc_start_lat)
        dest <- c(loc_end_lon, loc_end_lat)
        route <- osrmRoute(src = source, dst = dest, overview = FALSE)
        route <- unname(route)
        ideal_distance <- route[2]
        ideal_duration <- route[1]
        #return duration in minutes (5.67), and distance in km(3.45)
        
        # 4. get df rows with nta in start_nta and end_nta
        df_narrowed <- df_nta_merged %>% filter(pickup_nta_id == start_nta, dropoff_nta_id == end_nta)
        if(dim(df_narrowed)[1] != 0)
        {
            krows <- nrow(df_narrowed)
            df_start <- data.frame(lon = rep(loc_start_lon,krows),lat = rep(loc_start_lat, krows))
            df_end <- data.frame(lon = rep(loc_end_lon, krows), lat = rep(loc_end_lat, krows))
            dist_start_base <- distGeo(df_start[, c("lon", "lat")], df_narrowed[, c("pickup_longitude", "pickup_latitude")])/1609.35
            dist_end_base <- distGeo(df_end[,c("lon", "lat")], df_narrowed[,c("dropoff_longitude", "dropoff_latitude")])/1609.35
            dist_error <- dist_start_base + dist_end_base
            min_row_idx <- which(dist_error == min(dist_error))
            
            updateTextInput(session, "show_distance", value = paste0(
                          df_narrowed[min_row_idx,"trip_distance"]," (ideal: ",round(ideal_distance/1.60934,2),")"))
            updateTextInput(session, "show_duration", value = paste0(
                         round(df_narrowed[min_row_idx, "trip_time_in_secs"]/60,2)," (ideal: ",ideal_duration,")"))
            updateTextInput(session, "show_fare", value = paste0(df_narrowed[min_row_idx, "fare_amount"]))
            
            leafletProxy("reserve_map") %>% addCircles(lng = as.numeric(df_narrowed[min_row_idx,"pickup_longitude"]), 
                                                       lat = as.numeric(df_narrowed[min_row_idx, "pickup_latitude"]), radius = 30, color = "red") %>%
                addCircles(lng = as.numeric(df_narrowed[min_row_idx, "dropoff_longitude"]),
                           lat = as.numeric(df_narrowed[min_row_idx, "dropoff_latitude"]), radius = 30, color = "red")
        }else
        {
            updateTextInput(session, "show_distance", value = paste0("ideal: ", round(ideal_distance/1.60934,2)))
            updateTextInput(session, "show_duration", value = paste0("ideal: ", ideal_duration))
            updateTextInput(session, "show_fare", value = "unknown")
        }
    })
    
}


## run App
shinyApp(ui, server)
