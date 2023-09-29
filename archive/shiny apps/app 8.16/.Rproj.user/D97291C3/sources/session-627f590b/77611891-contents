library(shiny)
library(rsconnect)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(pander)
library(stringr)
library(pander)
library(stringr)
library(kableExtra)
library(jsonlite)
library(magrittr)
library(HatchedPolygons)

ui <- fluidPage(
  titlePanel(title = "Pennsylvania Housing Conditions Explorer", windowTitle = "PHFA Housing Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # checkboxGroupInput("dataLayers", "Select Indicator",
      #                    choices = c(
      #                      "Rural Counties" = "rural"),
      #                    selected = character(0))
      )
    ,
    
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Interactive Map", leafletOutput("leaflet")),
                tabPanel("Distribution", plotOutput("plot"),
                         h6(textOutput("caption", container = span))),
                tabPanel("Summary Statistics", verbatimTextOutput("summary"))),

  )
  )
)

