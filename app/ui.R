```{r setup, include=FALSE}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("ggmap")) {
  install.packages("ggmap")
  library(ggmap)
}

if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}



incident_types <- sort(unique(merged_data$incidentType))
incident_types <- c("Total", incident_types)

ui <- fluidPage(
  titlePanel("FEMA Declarations by State"),
  
  sidebarLayout(
    sidebarPanel(
      # Add a dropdown menu to select incident type
      selectInput("incidentType", "Select Incident Type:",
                  choices = incident_types),
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)
