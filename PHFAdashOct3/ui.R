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

ui <- navbarPage(theme = bs_theme(bootswatch = "flatly"),
                 title = "PHFA Housing Explorer",
           
        tabPanel("Interactive map", sidebarLayout(
          sidebarPanel(width = 3, h3("Affordable Housing Explorer"), 
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
                                                                                "Affordable housing shortage (units)" = "housing_balance",
                                                                                "Rural" = "rural"), selected = "owner_occ_hh_pct2021"),
                       br(), 
                       shiny::p("Use this web map to explore housing conditions across Pennsylvania counties. Select an indicator to begin."),
                       downloadButton("downloadData", "Download Data"),  
                       br()),
          mainPanel(width = 9, leafletOutput("leaflet", height = "800px", width = "100%")))),
        
        tabPanel("Statewide comparisons", sidebarLayout(
          sidebarPanel(width = 3,h3("Affordable Housing Explorer"), 
                       selectInput("variable_bar", "Select a variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                                "Rentership rate (%)" = "renter_occ_hh_pct2021",
                                                                                "Vacant rental units (%)" = "renter_vacant_pct2021",
                                                                                "Median age of home (years)" = "med_age_home2021",
                                                                                "Median home value ($)" = "med_age_home2021",
                                                                                "Households with internet access (%)" = "internet_hh_pct2021",
                                                                                "Rent burdened households (%)" = "rent_burdened_pct2021",
                                                                                "Mortgage burdened households (%)" = "mortgage_burdened_pct2021",
                                                                                "Median gross rent ($)" = "med_gross_rent2021",
                                                                                "Affordable rent units available" = "afford_avail_units",
                                                                                "Affordable housing shortage (units)" = "housing_balance",
                                                                                "Rural" = "rural"), selected = "owner_occ_hh_pct2021"),
                       br(), 
                       shiny::p("Compare housing indicators across counties."),
                       br()),
          mainPanel(width = 9, plotlyOutput("plot", height = "900px", width = "100%")))),
        
        tabPanel("Data Explorer", 
                 sidebarLayout(
                   sidebarPanel(width = 3,h3("Affordable Housing Explorer"),
                                selectInput("variable_scatter_x", "X variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                                "Rentership rate (%)" = "renter_occ_hh_pct2021",
                                                                                "Vacant rental units (%)" = "renter_vacant_pct2021",
                                                                                "Median age of home (years)" = "med_age_home2021",
                                                                                "Median home value ($)" = "med_age_home2021",
                                                                                "Households with internet access (%)" = "internet_hh_pct2021",
                                                                                "Rent burdened households (%)" = "rent_burdened_pct2021",
                                                                                "Mortgage burdened households (%)" = "mortgage_burdened_pct2021",
                                                                                "Median gross rent ($)" = "med_gross_rent2021",
                                                                                "Affordable rent units available" = "afford_avail_units",
                                                                                "Affordable housing shortage (units)" = "housing_balance",
                                                                                "Rural" = "rural"), selected = "owner_occ_hh_pct2021"),
                                selectInput("variable_scatter_y", "Y variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                              "Rentership rate (%)" = "renter_occ_hh_pct2021",
                                                                              "Vacant rental units (%)" = "renter_vacant_pct2021",
                                                                              "Median age of home (years)" = "med_age_home2021",
                                                                              "Median home value ($)" = "med_age_home2021",
                                                                              "Households with internet access (%)" = "internet_hh_pct2021",
                                                                              "Rent burdened households (%)" = "rent_burdened_pct2021",
                                                                              "Mortgage burdened households (%)" = "mortgage_burdened_pct2021",
                                                                              "Median gross rent ($)" = "med_gross_rent2021",
                                                                              "Affordable rent units available" = "afford_avail_units",
                                                                              "Affordable housing shortage (units)" = "housing_balance",
                                                                              "Rural" = "rural"), selected = "housing_balance"),
                                br(), 
                                shiny::p("Use this scatter plot to visualize the relationship between any two housing indicators."),
                                br()),
                   mainPanel(width = 9, br(), plotlyOutput("scatter", height = "700px", width = "80%")))),
        
        tabPanel("Data Viewer", 
                 sidebarLayout(
                   sidebarPanel(width = 3, h3("Affordable Housing Explorer"), 
                                selectInput("variable_tab", "Select a variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                                         "Rentership rate (%)" = "renter_occ_hh_pct2021",
                                                                                         "Vacant rental units (%)" = "renter_vacant_pct2021",
                                                                                         "Median age of home (years)" = "med_age_home2021",
                                                                                         "Median home value ($)" = "med_age_home2021",
                                                                                         "Households with internet access (%)" = "internet_hh_pct2021",
                                                                                         "Rent burdened households (%)" = "rent_burdened_pct2021",
                                                                                         "Mortgage burdened households (%)" = "mortgage_burdened_pct2021",
                                                                                         "Median gross rent ($)" = "med_gross_rent2021",
                                                                                         "Affordable rent units available" = "afford_avail_units",
                                                                                         "Affordable housing shortage (units)" = "housing_balance",
                                                                                         "Rural" = "rural"), selected = "owner_occ_hh_pct2021"),
                                br(), 
                                shiny::p("Use this table to view data for any indicator, by county."),
                                br()),
                   mainPanel(width = 9, h3("Data table"), DT::dataTableOutput("table"),
                             br(),
                             h3("Summary statistics"), tableOutput("sum")))))