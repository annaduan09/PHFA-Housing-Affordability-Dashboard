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
counties <- counties(state = 42) %>%
  rename(county = NAME) %>%
  dplyr::select(county) %>%
  st_transform("WGS84")

dat <- st_read("phfa_dash_data_9.28.geojson") %>%
  st_drop_geometry() %>%
  left_join(counties, by = "county") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform("WGS84")

panel.sf <- dat %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  st_drop_geometry() %>%
  left_join(dat, by = "county") %>%
  st_as_sf() %>%
  st_transform("WGS84")

panel <- panel.sf %>%
  st_drop_geometry()


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
      dplyr::select(variable = input$variable, county, geometry) %>%
      st_as_sf()
  })
  
  dat = reactive({
    panel %>%
      dplyr::select(county, variable = input$variable)
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
      groupOptions("txt_labels", zoomLevels = 12:100)
  })
  # 
  # output$leaflet <- renderLeaflet({
  #   leaflet(dat.sf()) %>%
  #     addPolygons(fillColor = ~mapPalette()(dat.sf()$variable),
  #                 color = "white",
  #                 weight = 1,
  #                 opacity = 1,
  #                 dashArray = "3",
  #                 fillOpacity = 0.8,  # Reduce opacity here
  #                 highlightOptions = highlightOptions(
  #                   weight = 1,
  #                   color = "#666",
  #                   dashArray = "",
  #                   fillOpacity = 0.5,
  #                   bringToFront = TRUE)
  #                 # label = labs_dat,
  #                 # labelOptions = labelOptions(
  #                 #   style = list("font-weight" = "normal", padding = "3px 8px"),
  #                 #   textsize = "15px",
  #                 #   direction = "auto")
  #     ) %>%
  #     # addLegend(pal = mapPalette(), title = "", opacity = 1, values = dat.sf()$variable,
  #     #           position = "bottomright") %>%
  #     # addLabelOnlyMarkers(data = dat.sf(), ~dat.sf()$lon, ~dat.sf()$lat, label =  ~as.character(dat.sf()$county),
  #     #                     labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
  #     #                       "color" = "DarkCyan",
  #     #                       "font-family" = "sans-serif",
  #     #                       "font-size" = "12px")),
  #     #                     group = "txt_labels") %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     groupOptions("txt_labels", zoomLevels = 12:100)
  # })
  
}
