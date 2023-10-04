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
library(shinythemes)
library(plotly)
library(bslib)

ui <- fluidPage(tags$style(
  HTML(
    "
      .navbar-header {
        height: 60px; /* You can adjust the height as needed */
        line-height: 60px; /* Align text vertically within the header */
      }
      "
  )
),
navbarPage(theme = bs_theme(bootswatch = "flatly"),
                 title = h2("PA Housing Affordability Dashboard"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               h2("Menu"), 
                               selectInput("variable", "Select a variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                                        "Rentership rate (%)" = "renter_occ_hh_pct2021",
                                                                                        "Vacant rental units (%)" = "renter_vacant_pct2021",
                                                                                        "Median age of home (years)" = "med_age_home2021",
                                                                                        "Median home value ($)" = "med_age_home2021",
                                                                                        "Households with internet access (%)" = "internet_hh_pct2021",
                                                                                        "Rent burdened households (%)" = "rent_burdened_pct2021",
                                                                                        "Mortgage burdened households (%)" = "mortgage_burdened_pct2021",
                                                                                        "Median gross rent ($)" = "med_gross_rent2021",
                                                                                        "Affordable rent units available" = "afford_avail_units",
                                                                                        "Housing supply" = "housing_balance",
                                                                                        "Rural" = "rural"), selected = "owner_occ_hh_pct2021"),
                  actionButton("rural", "Show Rural Counties"),
                  br(), 
                  br(), 
                  shiny::p("Use this web app to explore housing trends across Pennsylvania counties."),
                  downloadButton("downloadData", "Download Data"),  
                  br(),
                  )
                  ,
                  mainPanel(
                    tabsetPanel(tabPanel(width = 9, title = h4("Interactive Map"), 
                                         leafletOutput("leaflet", height = "800px", width = "100%")),
                                tabPanel(width = 9, title = h4("Statewide Comparisons"), 
                                         plotlyOutput("plot", height = "1000px", width = "100%"),
                                         h6(textOutput("caption", container = span))),
                                tabPanel(width = 9, title = h4("Data Explorer"), 
                                         plotlyOutput("scatter", height = "1000px", width = "100%")),
                                tabPanel(width = 9, title = h4("Data Viewer"), 
                                         h3("Data table"), DT::dataTableOutput("table"),
                                         h3("Summary"), tableOutput("sum")))
                  )
                )
))
