library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(plotly)

# Load your data
fema_data = read.csv('/Users/mansi/Desktop/Fall 2023/Applied Data Science/ADS-Fall2023-Project2-ShinyApp-Group-8/data/DisasterDeclarationsSummaries.csv')
fema_data<-fema_data[!duplicated(fema_data$disasterNumber),]
state_coords <- read.csv("/Users/mansi/Desktop/Fall 2023/Applied Data Science/ADS-Fall2023-Project2-ShinyApp-Group-8/data/states.csv")
incident_types <- c("Total", unique(fema_data$incidentType))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "FEMA Declarations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Dataset", tabName = "data", icon = icon("table")),
      menuItem("Declaration Counts", tabName = "declaration_counts", icon = icon("dashboard")),
      menuItem("Trend for FEMA data", tabName = "trend_tab", icon = icon("question-circle")),
      menuItem("Undetermined 2", tabName = "undetermined2", icon = icon("question-circle")),
      menuItem("Undetermined 3", tabName = "undetermined3", icon = icon("question-circle")),
      menuItem("Business Values and Findings", tabName = "business", icon = icon("briefcase"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h1("Introduction"),
              p("Contents to be added..")
      ),
      tabItem(tabName = "data",
              h1("Dataset"),
              DT::dataTableOutput("table")
      ),
      tabItem(tabName = "declaration_counts",
              fluidRow(
                box(width = 2,
                    selectInput("incidentType", "Select Incident Type:", choices = incident_types),
                    selectInput("state_string", "Choose a State:", choices = unique(fema_data$state))
                ),
                box(leafletOutput("map", height = "450px"), width = 5),
                box(plotOutput("disasterPieChart", height = "450px"), width = 5)
              )
      ),
      tabItem(tabName = "trend_tab",
              h1("Trend for FEMA data"),
              selectInput("disaster_type", "Choose a Disaster Type", sort(unique(fema_data$incidentType))),
              selectInput("state", "Choose a State", c("None", sort(unique(fema_data$state)))),
              sliderInput("time_range", "Select Time Range:", min = min(fema_data$fyDeclared), max = max(fema_data$fyDeclared), value = c(min(fema_data$fyDeclared), max(fema_data$fyDeclared))),
              plotlyOutput("linePlot"),
              plotlyOutput("barPlot")
      ),
      
      tabItem(tabName = "undetermined2",
              h1("Undetermined 2"),
              p("Contents to be added...")
      ),
      tabItem(tabName = "undetermined3",
              h1("Undetermined 3"),
              p("Contents to be added...")
      ),
      tabItem(tabName = "business",
              h1("Business Values and Findings"),
              p("In this section, we discuss...")
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  # ... existing server logic ...
  
  # For the "Dataset" tab
  output$table <- DT::renderDataTable({
    fema_data
  })
  
  # Additional logic from your snippet
  declaration_counts_total <- fema_data %>%
    group_by(state) %>%
    summarise(DeclarationCount = n(), .groups = "drop")
  
  declaration_counts_sub <- fema_data %>%
    group_by(state, incidentType) %>%
    summarise(DeclarationCount = n(), .groups = "drop")
  
  merged_data_total <- merge(declaration_counts_total, state_coords, by = "state", all.x = FALSE)
  merged_data_sub <- merge(declaration_counts_sub, state_coords, by = "state", all.x = FALSE)
  
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
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addCircles(
        data = filtered_data(),
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste("State:", state, "   Declaration Count:", DeclarationCount),
        labelOptions = labelOptions(noHide = FALSE, direction = "auto"),
        radius = ~DeclarationCount * 250,
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
  
  output$disasterPieChart <- renderPlot({
    state_data <- fema_data[fema_data$state == input$state_string, ]
    
    ggplot(state_data, aes(x = "", fill = incidentType)) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      labs(
        title = paste("Fractions of Natural Disasters in", input$state_string),
        x = NULL,
        y = NULL,
        fill = "Incident Type"  # Changing legend title
      ) +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal()
  })
  
  
  output$linePlot <- renderPlotly({
    filtered_data <- fema_data %>%
      filter(incidentType == input$disaster_type & (state == input$state | input$state == "None")) %>%
      group_by(fyDeclared) %>%
      summarise(frequency = n())
    
    p <- ggplot(filtered_data, aes(x = fyDeclared, y = frequency)) +
      geom_line() +
      ggtitle(paste("Frequency of Selected Incident Type Over Years in", ifelse(input$state == "None", "All States", input$state)))
    
    
    ggplotly(p)
  })
  
  output$barPlot <- renderPlotly({
    filtered_data <- fema_data %>%
      filter(incidentType == input$disaster_type & (state == input$state | input$state == "None") & fyDeclared >= input$time_range[1] & fyDeclared <= input$time_range[2]) %>%
      group_by(fyDeclared) %>%
      summarise(frequency = n())
    
    p <- ggplot(filtered_data, aes(x = fyDeclared, y = frequency)) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Frequency of Selected Incident Type Over Specified Time Range in", ifelse(input$state == "None", "All States", input$state)))
    
    ggplotly(p)
  })

}

# Run the application
shinyApp(ui = ui, server = server)