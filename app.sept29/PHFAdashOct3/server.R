library(conflicted)
library(shiny)
library(rsconnect)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(stringr)
library(jsonlite)
library(magrittr)
library(HatchedPolygons)
library(tidyverse)
library(tigris)
library(mapview)


#### Data processing ####  
dat <- st_read("PHFA_dash_data_October3.geojson") 

panel.sf <- dat %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  mutate(
    lat = st_coordinates(.)[, 2],
    lon = st_coordinates(.)[, 1]
  ) %>%
  st_drop_geometry() %>%
  left_join(dat, by = "county") %>%
  st_as_sf()

panel <- st_drop_geometry(panel.sf)


rural <- hatched.SpatialPolygons(panel.sf %>% dplyr::filter(rural == 1), density = 13, angle = c(45, 135)) %>%
  st_union() %>%
  st_as_sf()


#### Server ####
server <- function(input, output, session) {
  
  
  
  #### reactive palette ####
  mapPalette <- reactive({
    colorNumeric(
      palette = "YlGnBu",
      domain = NULL,
      na.color = "gray",
      reverse = FALSE)
  })
 
  #### reactive dataframe ####
  varInput.sf <- reactive({
    input$variable
  })
  
  varInput <- reactive({
    input$variable
  })
  
  dat.sf = reactive({
    panel.sf %>%
      dplyr::select(variable = input$variable, county, geometry, lat, lon) %>%
      st_as_sf()
  })
  
  dat = reactive({
    panel %>%
      dplyr::select(county, variable = input$variable, lat, lon)
  })
  
  
  
  ##### plot #####
  output$plot <- renderPlot({
    v <- input$variable

    ggplot(data = dat(), aes(x = county, y = variable, fill = variable)) +
      geom_bar(color = "transparent", stat = "identity") +
      geom_text(aes(label=variable), hjust=0, colour = "navy", alpha = 0.6,  position = "dodge") +
      scale_fill_distiller(palette = "YlGnBu", direction = 1) +
      labs(title = "", fill = v, color = "Rural County") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 16),
            axis.title.y = element_text(face = "bold")) +
      coord_flip()
  })

  ##### summary #####
  output$tab <- renderTable({
    data.frame(quartile_1 = quantile(dat()$variable, probs = 0.25, na.rm = TRUE),
               mean = mean(dat()$variable, na.rm = TRUE),
               median = median(dat()$variable, na.rm = TRUE),
               quartile_3 = quantile(dat()$variable, probs = 0.75, na.rm = TRUE),
               max = max(dat()$variable, na.rm = TRUE)) 
    
    
  })

  #### Leaflet ####
  ##### title #####
  title_dat <- tags$div(
    HTML("<strong>Indicators by County (%)</strong><br/>
        Hover to see individual counties"))
  
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = dat.sf(), fillColor = ~mapPalette()(dat.sf()$variable),
                  color = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  dashArray = "3",
                  highlightOptions = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addControl(title_dat, position = "topright") %>%
    addLegend(pal = mapPalette(), title = "", opacity = 1, values = dat.sf()$variable,
              position = "bottomright") %>%
    addLabelOnlyMarkers(data = dat.sf(), ~dat.sf()$lon, ~dat.sf()$lat, label =  ~as.character(dat.sf()$county),
                        labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
                          "color" = "DarkGray",
                          "font-family" = "sans-serif",
                          "font-size" = "12px")),
                        group = "txt_labels") %>%
      groupOptions("txt_labels", zoomLevels = 8:100)
    
  })
  
}
