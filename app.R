#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

## function to find nearest point on a set of lines to a given point
# this must be optimizable!
nearest_point_on_lines <- function(input_lat, input_lng, lines){
    point <- dplyr::tibble(lat = input_lat, lng = input_lng) %>%
        sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
    
    pointonline <- lines %>%
        mutate(line = sf::st_nearest_points(geometry, point)) %>%
        mutate(length = sf::st_length(line)) %>%
        select(NAME_1,length, line) %>%
        arrange(length) %>%
        filter(length > units::as_units(0, "m")) %>%
        slice_head(n=1) %>% 
        #  sf::st_set_geometry(NULL) %>%
        pull(line) %>%
        sf::st_cast("POINT") %>%
        purrr::map(as.matrix) %>%
        unlist() %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        as_tibble() %>%
        rename(lat = V2, lon = V1) %>%
        filter (lat != input_lat, lon != input_lng)
    
    return(pointonline)
}

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Leaflet Draggable Marker Map Snap"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tableOutput("snap_output"),
            hr(),
            actionButton("button_add_marker", "Add Marker"),
            actionButton("button_clear_markers", "Clear Markers"),
            hr(),
            shiny::checkboxInput("checkbox_snap", "Snap to lines", value = FALSE)
            
            
        ),

        # Show a map
        mainPanel(
           leaflet::leafletOutput("snap_map")
        )
    )
)

# Define server logic 
server <- function(input, output) {
    # counter for new marker ids
    num_markers <- 1
    
    # map lines come from leaflet package
    swiss_lines <- leaflet::gadmCHE %>%
        sf::st_as_sf(crs = "WGS84") %>%
        as_tibble() %>%
        sf::st_as_sf(crs = "WGS84") 
    
    # start point in Bern
    start_point <- tibble(lat = 46.943060, lon = 7.441868) %>% 
        sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84",  remove = FALSE)
    
    # set up starting map
    output$snap_map <- leaflet::renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolylines(data = swiss_lines) %>%
            addMarkers(data = start_point, 
                       layerId = "marker_1",
                       options = markerOptions(draggable = TRUE))
    })
    
    # set up proxy for updating
    snap_map_proxy <- leafletProxy("snap_map")
    
    # observe dragging a marker
    observeEvent(input$snap_map_marker_dragend, {
        dragged_id <- input$snap_map_marker_dragend$id
        dragged_lat = input$snap_map_marker_dragend$lat
        dragged_lng = input$snap_map_marker_dragend$lng
        
        print(input$snap_map_marker_dragend)
        output$snap_output <- renderTable(dplyr::as_tibble(input$snap_map_marker_dragend))
        
        # if we're snapping, do that
        if (input$checkbox_snap){
            message("should snap")
            
            new_latlon <- nearest_point_on_lines(input_lat = dragged_lat,
                                                 input_lng = dragged_lng,
                                                 lines = swiss_lines)
            
            snap_map_proxy %>%
                removeMarker(layerId = dragged_id) %>%
                addMarkers(data = new_latlon,
                           layerId = dragged_id,
                           options = markerOptions(draggable = TRUE))
            message("snapped")
            
        }
        
    })
    
    # if button is pushed add another marker
    observeEvent(input$button_add_marker,{
        num_markers <<- num_markers + 1
        snap_map_proxy %>%
            addMarkers(data = start_point,
                       layerId = paste0("marker_", num_markers),
                       options = markerOptions(draggable = TRUE))
        
    })
    
    # if button is pushed, clear all markers
    observeEvent(input$button_clear_markers, {
        num_markers <<- 0
        snap_map_proxy %>%
            leaflet::clearMarkers()
        
        output$snap_output <- renderTable(NULL)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
