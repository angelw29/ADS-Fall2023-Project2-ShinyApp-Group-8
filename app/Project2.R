library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tidyverse)

# Load your data
fema_data = read.csv('/Users/angelwang/Desktop/fall 2023/4243/ADS-Fall2023-Project2-ShinyApp-Group-8/data/DisasterDeclarationsSummaries.csv')
fema_data<-fema_data[!duplicated(fema_data$disasterNumber),]
state_coords <- read.csv("/Users/angelwang/Desktop/fall 2023/4243/ADS-Fall2023-Project2-ShinyApp-Group-8/data/states.csv")

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
              selectInput("state", "Choose a State", sort(unique(fema_data$state))),
              plotOutput("timeSeriesPlot"),
              plotOutput("trendPlot"),
              plotOutput("typePlot")
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
  
  
  output$timeSeriesPlot <- renderPlot({
    # Filter the data based on input
    filtered_data <- subset(fema_data, 
                            incidentType == input$disaster_type & 
                              state == input$state)
    
    # Convert declarationDate to Date class if not already
    filtered_data$declarationDate <- as.Date(filtered_data$declarationDate, format="%Y-%m-%d") 
    
    # Ensure that the filtered data is not empty
    if(nrow(filtered_data) == 0) {
      ggplot() +
        ggtitle("No data available for selected options") +
        theme_minimal()
    } else {
      # Resampling by month and count the occurrences
      monthly_data <- as.data.frame(table(cut(filtered_data$declarationDate, breaks = "month")))
      
      # Ensure that Var1 is in Date format
      monthly_data$Var1 <- as.Date(monthly_data$Var1)
      
      # Generate a time series plot
      ggplot(data=monthly_data, aes(x=Var1, y=Freq)) +
        geom_line() +
        labs(
          title = paste('Monthly Time Series for', input$disaster_type, 
                        'in', input$state),
          x = 'Date',
          y = 'Count'
        ) +
        theme_minimal() +
        scale_x_date(
          limits = as.Date(c("1960-01-01", "2023-01-01")),
          breaks = seq(as.Date("1960-01-01"), as.Date("2023-01-01"), by="5 years"),
          date_labels = "%Y"
        )
    }
  })
output$trendPlot <- renderPlot({ 
  Years = as.numeric(substr(fema_data$declarationDate, 1, 4))
  years <- data.frame(Years = as.numeric(substr(fema_data$declarationDate, 1, 4)))
  fema_data$year <- years$Years
  
  fema_data_trend <- subset(fema_data, year >= 1960)  %>%
    group_by(year) %>%
    summarise(count = n())
  
  ggplot(fema_data_trend)+
    geom_point(mapping =  aes(x=year, y=count)) +
    geom_line(mapping =  aes(x=year, y=count))+
    labs(title = "Total Count of Incident by Year",
         x = "Year",
         y = "Number of Incidents") 
})

output$typePlot <- renderPlot({ 
  Years = as.numeric(substr(fema_data$declarationDate, 1, 4))
  years <- data.frame(Years = as.numeric(substr(fema_data$declarationDate, 1, 4)))
  fema_data$year <- years$Years
  
  fema_data_type <- subset(fema_data, year >= 1960)  %>%
    group_by(year, incidentType) %>%
    summarise(count = n())
  
  ggplot(fema_data_type)+
    geom_line(mapping =  aes(x=year, y=count,color = incidentType))+
    labs(title = "Count of Incident by Year based on Incident Type",
         x = "Year",
         y = "Number of Incidents") 
})
  
}

# Run the application
shinyApp(ui = ui, server = server)