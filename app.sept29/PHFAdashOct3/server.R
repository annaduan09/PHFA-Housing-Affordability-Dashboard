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
      dplyr::select(variable = input$variable, county, geometry, lat, lon, rural) %>%
      st_as_sf()
  })
  
  dat = reactive({
    panel %>%
      dplyr::select(county, variable = input$variable, lat, lon, rural)
  })
  
  
  ##### plot #####
  output$plot <- renderPlot({
    v <- input$variable
    df <- dat() %>% as.data.frame()
    df <- df %>%
      mutate(rural_score = ifelse(rural == 1, 100000, 0),
             order_id = rural_score + variable)

    variable_aliases <- c(
      "owner_occ_hh_pct2021" = "Homeownership rate (%)",
      "renter_occ_hh_pct2021" = "Rentership rate (%)",
      "renter_vacant_pct2021" = "Vacant rental units (%)",
      "med_age_home2021" = "Median age of home (years)",
      "med_age_home2021" = "Median home value ($)",
      "internet_hh_pct2021" = "Households with internet access (%)",
      "rent_burdened_pct2021" = "Rent burdened households (%)",
      "mortgage_burdened_pct2021" = "Mortgage burdened households (%)",
      "med_gross_rent2021" = "Median gross rent ($)",
      "afford_avail_units" = "Affordable rent units available",
      "housing_balance" = "Housing supply",
      "rural" = "Rural"
    )
    
    v <- input$variable
    alias <- variable_aliases[v]
  
    ggplot(data = df, aes(x = reorder(county, order_id), y = variable, fill = variable)) +
      geom_bar(color = "transparent", stat = "identity") +
      geom_text(aes(label=variable), hjust=0, colour = "navy", alpha = 0.6,  position = "dodge") +
      scale_fill_distiller(palette = "YlGnBu", direction = 1) +
      labs(title = "", fill = alias, color = "Rural County", y = alias, x = "Urban Counties                                          Rural Counties") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 16),
            axis.title.y = element_text(face = "bold")) +
      coord_flip() 
    
  })
  
  #### plot header text ####
  output$plotHeaderText <- renderText({
    variable_aliases <- c(
      "owner_occ_hh_pct2021" = "Homeownership rate (%)",
      "renter_occ_hh_pct2021" = "Rentership rate (%)",
      "renter_vacant_pct2021" = "Vacant rental units (%)",
      "med_age_home2021" = "Median age of home (years)",
      "med_age_home2021" = "Median home value ($)",
      "internet_hh_pct2021" = "Households with internet access (%)",
      "rent_burdened_pct2021" = "Rent burdened households (%)",
      "mortgage_burdened_pct2021" = "Mortgage burdened households (%)",
      "med_gross_rent2021" = "Median gross rent ($)",
      "afford_avail_units" = "Affordable rent units available",
      "housing_balance" = "Housing supply",
      "rural" = "Rural"
    )
    v <- input$variable
    alias <- variable_aliases[v]
    return(paste(alias, "by Pennsylvania County, 2023", sep = " "))
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
  
#### Rural hashing update ####
x = reactiveVal(1)
  observeEvent(input$rural,{
    x(x()+1) # increment x by 1
    x <- as.numeric(x())
  })
  
  observeEvent(input$rural, {
    if((x() %% 2) == 0) {
      leafletProxy("leaflet") %>%
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
        addPolylines(
          color = "orange",
          data = rural,
          weight = 1.5,
          layerId  = "rural") 
    } else {
      leafletProxy("leaflet") %>%
        removeShape(layerId  = "rural")
    }
  })
  
  
#### Downloadable csv of selected dataset ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$variable, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )
}

