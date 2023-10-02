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


fema_data = read.csv('/Users/angelwang/Desktop/fall 2023/4243/ADS-Fall2023-Project2-ShinyApp-Group-8/data/DisasterDeclarationsSummaries.csv')

state_coords <- read.csv("/Users/angelwang/Desktop/fall 2023/4243/ADS-Fall2023-Project2-ShinyApp-Group-8/data/states.csv")

declaration_counts_total <- fema_data %>%
  group_by(state) %>%
  summarise(DeclarationCount = n())

declaration_counts_sub <- fema_data %>%
  group_by(state, incidentType) %>%
  summarise(DeclarationCount = n())
merged_data_total <- merge(declaration_counts_total, state_coords, by = "state", all.x = FALSE)
merged_data_sub <- merge(declaration_counts_sub, state_coords, by = "state", all.x = FALSE)

# Define server logic
server <- function(input, output, session) {
  # Filter data based on selected incident type
  filtered_data <- reactive({
    if (input$incidentType == "Total") {
      merged_data_total %>%
        mutate(incidentType = "Total")
    } else {
      merged_data_sub %>%
        filter(incidentType == input$incidentType) %>%
        mutate(incidentType = incidentType)
    }
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addCircles(
        data = filtered_data(),
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste("State:", state, "<br>Declaration Count:", DeclarationCount),
        labelOptions = labelOptions(noHide = FALSE, direction = "auto"),
        radius = ~DeclarationCount * 25,
        color = "blue",
        fill = TRUE,
        fillOpacity = 0.6
      ) %>%
      addLegend(
        position = "topright",
        colors = "blue",
        labels = "Declaration Count",
        opacity = 1
      )
  })
}

shinyApp(ui = ui, server = server)