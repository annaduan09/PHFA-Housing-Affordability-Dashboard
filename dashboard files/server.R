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
  dplyr::mutate(housing_balance = ifelse(housing_balance < 0, abs(housing_balance), 0))

panel.sf <- dat %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  dplyr::mutate(
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

state_avg <- st_read("state_avg.csv")

#### Server ####
server <- function(input, output, session) {
  
  pa_avg = reactive({
    state_avg %>%
      dplyr::select(variable = input$variable)
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
  
  
  
  #### reactive palette ####
  mapPalette <- reactive({
    leaflet::colorQuantile(
      palette = "YlGnBu",
      domain = NULL,
      reverse = FALSE,
      probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    )
  })
  
  
  #### bar plot ####
  output$plot <- renderPlotly({
    v <- input$variable_bar
    df <- dat() %>% as.data.frame()
    df <- df %>%
      dplyr::mutate(rural_score = ifelse(rural == 1, 100000, 0),
             order_id = rural_score + variable_bar) %>%
      head(input$slider_bars)
    
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

ggplotly(barp) %>%
  plotly::layout(margin = list(l = 50, r = 50, b = 100, t = 50),
         annotations = list(x = 0.5, y = -0.12, text = "Source: Duan, Anna. Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, Oct. 2023, annaduan09.shinyapps.io/PHFAdashOct3/. ",
                            xref='paper', yref='paper', showarrow = F, 
                            xanchor='center', yanchor='bottom', xshift=0, yshift=0,
                            font = list(size = 12, color = "gray")))
    
  })
  
##### leaflet menu indicator information #####
  output$indicator_desc_text <- renderText({
  description <- c(
    "owner_occ_hh_pct2021" = "Homeownership rate (%) is the percentage of households that own their homes. A higher rate indicates a greater proportion of homeowners in the area.",
    "renter_occ_hh_pct2021" = "Rentership rate (%) is the percentage of households that rent their homes. It provides insight into the proportion of the population that does not own property.",
    "renter_vacant_pct2021" = "Vacant rental units (%) represents the percentage of rental units that are currently unoccupied. A higher percentage can suggest that there's a surplus of rental housing or potentially decreased demand.",
    "med_age_home2021" = "Median age of home (years) indicates the midpoint age of homes in a specific area. Older median ages can suggest historical or older neighborhoods, while lower values might indicate newer developments.",
    "med_age_home2021" = "Median home value ($) is the midpoint value of homes in the area. This can provide an insight into the overall affordability and property values of a region.",
    "internet_hh_pct2021" = "Households with internet access (%) is the percentage of households that have access to the internet. This can provide insights into the area's technological infrastructure and development.",
    "rent_burdened_pct2021" = "Rent burdened households (%) represents households that spend 30% or more of their income on rent. Higher percentages can suggest issues with affordability for renters.",
    "mortgage_burdened_pct2021" = "Mortgage burdened households (%) indicates the households spending 30% or more of their income on mortgage payments. Higher percentages may show potential financial strain for homeowners.",
    "med_gross_rent2021" = "Median gross rent ($) is the midpoint monthly rent amount households pay, inclusive of utilities. It helps gauge the typical rental costs in an area.",
    "afford_avail_units" = "Affordable rent units available measures the total number of rental units in the area that are deemed affordable based on set income standards or thresholds.",
    "housing_balance" = "Affordable housing shortage (units) refers to the difference between the demand for affordable housing and the supply available. A positive number indicates a shortage of affordable housing units."
  )
    v <- input$variable
    desc <- description[v]
    return(desc)
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
      geom_smooth(se = FALSE, colour = "black", size = 0.5) +
      geom_point(stat = "identity", aes(color = as.factor(rural)), size = 4, alpha = 0.5) +
      scale_color_manual(values = c("cyan2", "chartreuse3"), name = "Rural") +
      labs(title = paste(alias_x, "as a function of", alias_y, sep = " "), 
           x = alias_x, y = alias_y) + theme_minimal() 
    
ggplotly(scatterp + theme(legend.position = c(0.6, 0.6))) %>%
  plotly::layout(margin = list(l = 50, r = 50, b = 100, t = 50),
                 annotations = list(x = 0.5, y = -0.155, text = "Source: Duan, Anna. Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, Oct. 2023, annaduan09.shinyapps.io/PHFAdashOct3/. ",
                                    xref='paper', yref='paper', showarrow = F, 
                                    xanchor='center', yanchor='bottom', xshift=0, yshift=0,
                                    font = list(size = 12, color = "gray")))
    
  })

  #### Data table ####
  output$table <- DT::renderDataTable({
    v <- input$variable_tab
    
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

    alias <- variable_aliases[v]
    
    dat.tab <- dat() %>% 
      dplyr::select(county, variable_tab, rural) %>%
      as.data.frame() 
    
    names(dat.tab) <- c("county", alias, "rural")
    
    DT::datatable(as.data.frame(dat.tab), options = list(pageLength = 10))
  })
  
  #####  data table text #####
  output$tableheader <- renderText({
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
      "housing_balance" = "Affordable housing shortage (units)"
    )
    v <- input$variable_tab
    alias <- variable_aliases[v]
    return(paste(alias, "rankings, 2023", sep = " "))
  })
  

  
  output$sum <- renderTable({
    data.frame(quartile_1 = quantile(dat()$variable_tab, probs = 0.25, na.rm = TRUE),
               mean = mean(dat()$variable_tab, na.rm = TRUE),
               median = median(dat()$variable_tab, na.rm = TRUE),
               quartile_3 = quantile(dat()$variable_tab, probs = 0.75, na.rm = TRUE),
               max = max(dat()$variable_tab, na.rm = TRUE)) 
    
    
  })

  #### Leaflet prep ####
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
      "rural" = "Rural")
    
    v <- input$variable
    alias <- variable_aliases[v]
    
    sprintf(
      "<strong>%s County</strong><br/>
      <strong>%s</strong>: %.0f<sup></sup><br/>
      <strong>Pennsylvania:</strong> %.0f",
      dat.sf()$county, alias, dat.sf()$variable, round(as.numeric(pa_avg()$variable))
    ) %>% lapply(htmltools::HTML)
  })
  
  #### Leaflet map ####
  output$leaflet <- renderLeaflet({
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
      "housing_balance" = "Affordable housing shortage (units)"
    )
    
    v <- input$variable
    alias <- variable_aliases[v]
    
    var_map <- dat.sf()$variable

    # legend labels
    labels_map <- c(
      as.character(quantile(var_map, probs = c(0.2))),
      as.character(quantile(var_map, probs = c(0.4))),
      as.character(quantile(var_map, probs = c(0.6))),
      as.character(quantile(var_map, probs = c(0.8))),
      as.character(quantile(var_map, probs = c(1))))
    
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
      addLegend(pal = mapPalette(), 
                title = paste(as.character(alias), "<br>(Quintile breaks)", sep = ""), 
                opacity = 1, 
                labFormat = function(type, cuts, p) {   
                  paste0(labels_map)},
                values = round(quantile(dat.sf()$variable, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))),
              position = "bottomright") %>%
      addLabelOnlyMarkers(data = dat.sf(), ~dat.sf()$lon, ~dat.sf()$lat, label =  ~as.character(dat.sf()$county),
                        labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
                          "color" = "gray",
                          "font-family" = "sans-serif",
                          "font-size" = "10px")),
                        group = "county names") %>%
      addPolylines(data = rural,
        color = "orchid",
        weight = 2,
        opacity = 1,
        group  = "rural counties") %>%
      addLegend(group = "rural counties",
                colors = "orchid",
                labels = "Rural Counties",
                position = "bottomright") %>%
      addLayersControl(
        overlayGroups = c("rural counties", "county names"), 
        options = layersControlOptions(collapsed = F))%>%
      groupOptions("county names", zoomLevels = 8.5:100) %>%
      groupOptions("rural counties", zoomLevels = 8.5:100)
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
  output$downloadDataSel <- downloadHandler(
    filename = function() {
      paste(input$variable_tab, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataAll <- downloadHandler(
    filename = function() {
      "phfa_dash_2023.csv"
    },
    content = function(file) {
      write.csv(panel, file, row.names = FALSE)
    }
  )
}
