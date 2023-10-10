library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(plotly)
library(scales)
library(DT)

# Load your data
fema_data = read.csv('/Users/angelwang/Desktop/fall 2023/4243/ADS-Fall2023-Project2-ShinyApp-Group-8/data/DisasterDeclarationsSummaries.csv')
fema_data<-fema_data[!duplicated(fema_data$disasterNumber),]
state_coords <- read.csv("/Users/angelwang/Desktop/fall 2023/4243/ADS-Fall2023-Project2-ShinyApp-Group-8/data/states.csv")
incident_types <- c("Total", sort(unique(fema_data$incidentType)))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "FEMA Declarations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Dataset", tabName = "data", icon = icon("table")),
      menuItem("Declaration Counts", tabName = "declaration_counts", icon = icon("dashboard")),
      menuItem("Trend for FEMA data", tabName = "trend_tab", icon = icon("chart-bar")),
      menuItem("Program Activation Analysis", tabName = "ProgramActivation_tab", icon = icon("exclamation-triangle")),
      menuItem("Undetermined 3", tabName = "undetermined3", icon = icon("question-circle")),
      menuItem("Business Values and Findings", tabName = "business", icon = icon("briefcase"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              img(src = "https://kubrick.htvapps.com/htv-prod-media.s3.amazonaws.com/images/fema-0120-1652375316.jpg", width = "500px"),
              h1("Welcome to the Disaster Visualization App!"),
              p("This application is designed to provide a comprehensive and intuitive way to explore and analyze FEMA Disaster Declarations Summaries data. Through interactive maps, pie charts, line graphs, and histograms, you can gain valuable insights into the distribution and frequency of disasters. These visualizations offer a clear and engaging representation of disaster data, aiding in understanding the business impact and patterns associated with various types of disasters."),
              h3("About FEMA (Federal Emergency Management Agency)"),
              p("The Federal Emergency Management Agency (FEMA) is a crucial agency within the United States Department of Homeland Security. FEMA plays a pivotal role in coordinating disaster response and recovery efforts across the nation. Their mission revolves around assisting individuals and communities before, during, and after disasters. This assistance includes providing support, coordinating resources, and promoting disaster preparedness and mitigation strategies to minimize the impact of disasters on affected populations and regions. Through their initiatives, FEMA aims to enhance the resilience and preparedness of communities in the face of emergencies, ultimately saving lives and minimizing damage.")
              
              
      ),
      tabItem(tabName = "data",
              h1("Dataset"),
              DTOutput("table")
      ),
      tabItem(tabName = "declaration_counts",
              fluidRow(
                box(width = 2,
                    selectInput("incidentType", "Select Incident Type:", choices = incident_types),
                    selectInput("state_string", "Choose a State:", choices =c(sort(unique(fema_data$state)), "None"))
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
        
      tabItem(tabName = "ProgramActivation_tab",
              h1("Program Activation Analysis"),
              p("To analyze which programs are activated for different types of disasters."),
              selectInput("disaster_type_2", "Choose a Disaster Type", c("All", sort(unique(fema_data$incidentType)))),
              checkboxInput("include_tribal", "Include Tribal Requests", FALSE),
              h2("Public Assistance Program (PA)"),
              p("Denotes whether the Public Assistance program was declared for this disaster"),
              p("Through the PA Program, FEMA provides supplemental Federal grant assistance for debris removal, emergency protective measures, and the restoration of disaster-damaged, publicly owned facilities and specific facilities of certain Private Non-Profit organizations."),
              h2("Hazard Mitigation program (HA)"),
              p("Denotes whether the Hazard Mitigation program was declared for this disaster."),
              p("The FEMA Hazard Mitigation Grant Program provides funding to state, local, tribal, and territorial governments to develop hazard mitigation plans and rebuild in ways that reduce future disaster losses. The program emphasizes long-term risk reduction measures, including planning, acquisition of hazard-prone properties, flood protection, retrofitting, and construction projects to make communities more resilient."),
              h2("Individual Assistance Program (IA)"),
              p("Denotes whether the Individual Assistance program was declared for this disaster."),
              p("provides financial and direct services to eligible individuals affected by a disaster, who have uninsured or under-insured necessary expenses and serious needs."),
              h2("Individuals and Households Program (IH)"),
              p("Denotes whether the Individuals and Households program was declared for this disaster."),
              p("The program provides financial and direct services to eligible individuals and households affected by a disaster, who have uninsured or under-insured necessary expenses and serious needs."),
              plotlyOutput("pieChart"),
              plotlyOutput("barChart")
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
  output$table <- renderDT({
    datatable(
      fema_data,
      options = list(scrollX = TRUE)
    )
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
    if (input$state_string == "None"){
      ggplot(fema_data, aes(x = "", fill = incidentType)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(
          title = paste("Fractions of Natural Disasters in", input$state_string),
          x = NULL,
          y = NULL,
          fill = "Incident Type"  # Changing legend title
        ) +
        theme_minimal()
    }
    
    else
      {state_data <- fema_data[fema_data$state == input$state_string, ]
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
          theme_minimal()}
  })
  
  #For the trends tab 
  output$linePlot <- renderPlotly({
    filtered_data <- fema_data %>%
      filter(incidentType == input$disaster_type & (state == input$state | input$state == "None")) %>%
      group_by(fyDeclared) %>%
      summarise(frequency = n())
    if(nrow(filtered_data) == 0) {
      p <- ggplot(filtered_data) +
        ggtitle("No data available for selected options") +
        theme_minimal()
    } else {
      max_value <- max(filtered_data$frequency)
    p <- ggplot(filtered_data, aes(x = fyDeclared, y = frequency)) +
      geom_line() +
      geom_point()+
      coord_cartesian(ylim = c(0, max_value))+
      ggtitle(paste("Frequency of Selected Incident Type Over Years in", ifelse(input$state == "None", "All States", input$state)))
    }
    
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
  
  #For the program activation tab 
  output$barChart <- renderPlotly({
    filtered_data <- fema_data
    if (input$disaster_type_2 != "All") {
      filtered_data <- filtered_data %>% filter(incidentType == input$disaster_type_2)
    }
    if (!input$include_tribal) {
      filtered_data <- filtered_data %>% filter(tribalRequest == 0)
    }
    program_data <- filtered_data %>% 
      summarise(IH = sum(ihProgramDeclared),
                IA = sum(iaProgramDeclared),
                PA = sum(paProgramDeclared),
                HM = sum(hmProgramDeclared))
    
    p <- ggplot(tidyr::pivot_longer(program_data, cols = everything(), names_to = "variable", values_to = "value"), aes(x = variable, y = value)) +
      geom_bar(stat = "identity") +
      ggtitle("Number of Times Each Program is Activated")
    
    
    ggplotly(p)
  })
  
  # Pie Chart
  output$pieChart <- renderPlotly({
    filtered_data <- fema_data
    if (input$disaster_type_2 != "All") {
      filtered_data <- filtered_data %>% filter(incidentType == input$disaster_type_2)
    }
    if (!input$include_tribal) {
      filtered_data <- filtered_data %>% filter(tribalRequest == 0)
    }
    
    program_data <- filtered_data %>% 
      summarise(IH = sum(ihProgramDeclared),
                IA = sum(iaProgramDeclared),
                PA = sum(paProgramDeclared),
                HM = sum(hmProgramDeclared))
    
    program_data_long <- tidyr::pivot_longer(program_data, cols = everything(), names_to = "variable", values_to = "value")
    
    plot_ly(program_data_long, labels = ~variable, values = ~value, type = 'pie') %>%
      layout(title = "Proportion of Each Program Activated")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)