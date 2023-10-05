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
library(mapview)
library(plotly)
library(DT)


#### Data processing ####  
dat <- st_read("PHFA_dash_data_October3.geojson") %>%
  mutate(housing_balance = ifelse(housing_balance < 0, abs(housing_balance), 0))

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
  
  dat.sf = reactive({
    panel.sf %>%
      dplyr::select(variable = input$variable, county, geometry, lat, lon, rural) %>%
      st_as_sf()
  })
  
  dat = reactive({
    panel %>%
      dplyr::select(county, variable = input$variable, 
                    variable_bar = input$variable_bar, 
                    variable_scatter_x = input$variable_scatter_x, variable_scatter_y = input$variable_scatter_y, 
                    variable_tab = input$variable_tab, rural)
  })
  
  
  #### bar plot ####
  output$plot <- renderPlotly({
    v <- input$variable_bar
    df <- dat() %>% as.data.frame()
    df <- df %>%
      mutate(rural_score = ifelse(rural == 1, 100000, 0),
             order_id = rural_score + variable_bar)
    

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
      "housing_balance" = "Affordable housing shortage (units)",
      "rural" = "Rural"
    )
    
    v <- input$variable_bar
    alias <- variable_aliases[v]
  
barp <- ggplot(data = df, aes(x = reorder(county, order_id), y = variable_bar, fill = variable_bar)) +
      geom_bar(color = "transparent", stat = "identity") +
      scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "") +
      labs(title = paste(alias, "by PA county", sep = " "), caption = "Duan, Anna. Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, Oct. 2023, annaduan09.shinyapps.io/PHFAdashOct3/. ", fill = alias, color = "Rural County", y = alias, x = "Urban Counties                                          Rural Counties") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip() 

return(ggplotly(barp))
    
  })
  
  ##### barplot header text #####
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
      "housing_balance" = "Affordable housing shortage (units)",
      "rural" = "Rural"
    )
    v <- input$variable_bar
    alias <- variable_aliases[v]
    return(paste(alias, "by Pennsylvania County, 2023", sep = " "))
  })
  
  
  #### scatter plot ####
  output$scatter <- renderPlotly({
df <- dat() %>% as.data.frame()

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
  "housing_balance" = "Affordable housing shortage (units)",
  "rural" = "Rural"
)

x <- input$variable_scatter_x
y <- input$variable_scatter_y
alias_x <- variable_aliases[x]
alias_y <- variable_aliases[y]

    scatterp <- ggplot(df, aes(x = variable_scatter_x, y = variable_scatter_y)) +
      geom_smooth(se = FALSE, colour = "gray80") +
      geom_point(stat = "identity", aes(color = as.factor(rural))) +
      scale_color_brewer(palette = "YlGnBu", direction = 1, name = "Rural") +
      labs(title = paste(alias_x, "as a function of", alias_y, sep = " "), 
           caption = "Duan, Anna. Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, Oct. 2023, annaduan09.shinyapps.io/PHFAdashOct3/.", 
           x = alias_x, y = alias_y)+
      theme_minimal() 
    
    return(ggplotly(scatterp+ theme(legend.position = c(0.6, 0.6))))
    
  })

  #### Data viewer ####
  output$table <- DT::renderDataTable({
    v <- input$variable_tab
    dat.tab <- dat() %>% 
      as.data.frame() 
    
    names(dat.tab) <- c("county", v, "rural")
    
    DT::datatable(as.data.frame(dat.tab), options = list(pageLength = 10))
  })

  
  output$sum <- renderTable({
    data.frame(quartile_1 = quantile(dat()$variable_tab, probs = 0.25, na.rm = TRUE),
               mean = mean(dat()$variable_tab, na.rm = TRUE),
               median = median(dat()$variable_tab, na.rm = TRUE),
               quartile_3 = quantile(dat()$variable_tab, probs = 0.75, na.rm = TRUE),
               max = max(dat()$variable_tab, na.rm = TRUE)) 
    
    
  })

  #### Leaflet ####
  ##### title #####
  title_dat <- tags$div(
    HTML("<strong>Indicators by County</strong><br/>
        Hover to see individual counties"))
  
  ##### labels #####
  labs_dat <- reactive({
    
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
      "housing_balance" = "Affordable housing shortage (units)",
      "rural" = "Rural"
    )
    
    v <- input$variable
    alias <- variable_aliases[v]
    
    sprintf(
      "<strong>%s County</strong><br/>
      <strong>%s</strong>: %.0f<sup></sup><br/>
      <strong>Statewide Median:</strong> %.0f",
      dat.sf()$county, alias, dat.sf()$variable, median(dat.sf()$variable)
    ) %>% lapply(htmltools::HTML)
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
                    bringToFront = TRUE),
                  label = labs_dat()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addControl(title_dat, position = "topright") %>%
      addLegend(pal = mapPalette(), title = "", opacity = 1, values = dat.sf()$variable,
              position = "bottomright") %>%
      addLabelOnlyMarkers(data = dat.sf(), ~dat.sf()$lon, ~dat.sf()$lat, label =  ~str_to_upper(as.character(dat.sf()$county)),
                        labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
                          "color" = "gray",
                          "font-family" = "sans-serif",
                          "font-size" = "10px")),
                        group = "county names") %>%
      addPolylines(data = rural,
        color = "white",
        weight = 2,
        opacity = 1,
        group  = "rural counties") %>%
      addLayersControl(
        overlayGroups = c("rural counties", "county names"), 
        options = layersControlOptions(collapsed = F))%>%
      groupOptions("county names", zoomLevels = 9:100)
    
  })

# ##### Rural hashing update #####
# x = reactiveVal(1)
#   observeEvent(input$rural,{
#     x(x()+1) # increment x by 1
#     x <- as.numeric(x())
#   })
#   
#   observeEvent(input$rural, {
#     if((x() %% 2) == 0) {
#       leafletProxy("leaflet") %>%
#         addPolygons(data = dat.sf(), fillColor = ~mapPalette()(dat.sf()$variable),
#                     color = "white",
#                     weight = 1,
#                     opacity = 1,
#                     fillOpacity = 0.8,
#                     dashArray = "3",
#                     highlightOptions = highlightOptions(
#                       weight = 1,
#                       color = "#666",
#                       dashArray = "",
#                       fillOpacity = 0.5,
#                       bringToFront = TRUE)) %>%
#         addPolylines(
#           color = "white",
#           data = rural,
#           weight = 1.5,
#           layerId  = "rural") 
#     } else {
#       leafletProxy("leaflet") %>%
#         removeShape(layerId  = "rural")
#     }
#   })
  
  
#### data download ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$variable, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )
}

