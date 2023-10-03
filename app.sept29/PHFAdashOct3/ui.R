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

ui <- navbarPage(theme = shinytheme("united"), 
                 title = "PHFA Housing Dashboard",
                sidebarLayout(
                  sidebarPanel(width = 3,
                               selectInput("variable", "Select a variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                                        "Rentership rate (%)" = "renter_occ_hh_pct2021",
                                                                                        "Vacant rental units (%)" = "renter_vacant_pct2021",
                                                                                        "Median age of home (years)" = "med_age_home2021",
                                                                                        "Median home value ($)" = "med_age_home2021",
                                                                                        "Households with internet access (%)" = "internet_hh2021",
                                                                                        "Rent burdened households (%)" = "rent_burdened_pct2021",
                                                                                        "Mortgage burdened households (%)" = "mortgage_burdened_pct2021",
                                                                                        "Median gross rent ($)" = "med_gross_rent2021",
                                                                                        "Affordable rent units available" = "afford_avail_units",
                                                                                        "Housing supply" = "housing_balance",
                                                                                        "Rural" = "rural"), selected = "owner_occ_hh_pct2021"
                                           )
                               )
                  ,
                  mainPanel(
                    tabsetPanel(type = "pills",
                                tabPanel(title = h4("Data Mapper"), h3("Housing characteristics across Pennsylvania"),
                                         leafletOutput("leaflet", height = "800px", width = "100%")),
                                tabPanel(title = h4("Statewide Comparisons"), h3("Housing Characteristics Across Counties"), 
                                         plotOutput("plot", height = "1000px", width = "800px"),
                                         h6(textOutput("caption", container = span))),
                                tabPanel(title = h4("Data Viewer and Download"), h3("Summary"), tableOutput("tab")))
                  )
                )
)
