library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(plotly)
library(scales)
library(lubridate)

library(DT)

# Load your data
fema_data = read.csv('../data/DisasterDeclarationsSummaries.csv')
cleaned_data<-fema_data[!duplicated(fema_data$disasterNumber),]
state_coords <- read.csv("../data/states.csv")
incident_types <- c("Total", sort(unique(cleaned_data$incidentType)))
assistance_data = read.csv('../data/PublicAssistanceApplicantsProgramDeliveries.csv')


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
      menuItem("Damage Cost", tabName = "damage_cost", icon = icon("compass")),
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
                    selectInput("state_string", "Choose a State:", choices =c(sort(unique(cleaned_data$state)), "None"))
                ),
                box(leafletOutput("map", height = "450px"), width = 5),
                box(plotOutput("disasterPieChart", height = "450px"), width = 5)
              )
      ),
      tabItem(tabName = "trend_tab",
              h1("Trend for FEMA data"),
              selectInput("disaster_type", "Choose a Disaster Type", sort(unique(cleaned_data$incidentType))),
              selectInput("state", "Choose a State", c("None", sort(unique(cleaned_data$state)))),
              sliderInput("time_range", "Select Time Range:", min = min(cleaned_data$fyDeclared), max = max(cleaned_data$fyDeclared), value = c(min(cleaned_data$fyDeclared), max(cleaned_data$fyDeclared))),
              plotlyOutput("linePlot"),
              plotlyOutput("barPlot")
      ),
        
      tabItem(tabName = "ProgramActivation_tab",
              h1("Program Activation Analysis"),
              p("To analyze which programs are activated for different types of disasters."),
              selectInput("disaster_type_2", "Choose a Disaster Type", c("All", sort(unique(cleaned_data$incidentType)))),
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
      
      tabItem(tabName = "damage_cost",
              h1("Damage Costs"),
              selectInput("state1", "Choose a State", c("All", sort(unique(assistance_data$stateCode)))),
              selectInput("year", "Choose a Year", sort(unique(assistance_data$fyDeclared))),
              plotlyOutput("histogram"),
              plotlyOutput("CostlinePlot"),
      ),
      
      tabItem(tabName = "business",
              h1("Business Value"),
              
              tags$head(
                tags$style(HTML("
            .carousel-inner img {
              width: 100%;
              max-height: 400px;
            }
          "))
              ),
              
              tags$div(id = "myCarousel", class = "carousel slide", `data-ride` = "carousel",
                       tags$ol(class = "carousel-indicators",
                               tags$li(`data-target` = "#myCarousel", `data-slide-to` = "0", class = "active"),
                               tags$li(`data-target` = "#myCarousel", `data-slide-to` = "1"),
                               tags$li(`data-target` = "#myCarousel", `data-slide-to` = "2"),
                               tags$li(`data-target` = "#myCarousel", `data-slide-to` = "3"),
                               tags$li(`data-target` = "#myCarousel", `data-slide-to` = "4")
                       ),
                       
                       tags$div(class = "carousel-inner",
                                tags$div(class = "item active",
                                         tags$img(src = "https://d.newsweek.com/en/full/2018680/tornado.jpg", alt = "Image 1"),
                                         tags$div(class = "carousel-caption",
                                                  tags$h3("Tornado in Indiana, Arkansas", style = "position: absolute; bottom: 0; right: 0;")
                                         )
                                ),
                                tags$div(class = "item",
                                         tags$img(src = " https://mynorthwest.com/wp-content/uploads/2022/09/BoltCreekFire.jpg", alt = "Image 2"),
                                         tags$div(class = "carousel-caption",
                                                  tags$h3("Bolt Creek Fire", style = "position: absolute; bottom: 0; right: 0;")
                                         )
                                ),
                                tags$div(class = "item",
                                         tags$img(src = "https://www.snexplores.org/wp-content/uploads/2019/11/main_anchorage-street.jpg", alt = "Image 3"),
                                         tags$div(class = "carousel-caption",
                                                  tags$h3("1964 Alaska Earthquake", style = "position: absolute; bottom: 0; right: 0;")
                                         )
                                ),
                                tags$div(class = "item",
                                         tags$img(src = "https://dynaimage.cdn.cnn.com/cnn/c_fill,g_auto,w_1200,h_675,ar_16:9/https%3A%2F%2Fcdn.cnn.com%2Fcnnnext%2Fdam%2Fassets%2F201112174300-hiddenite-north-carollna-flooding.jpg", alt = "Image 4"),
                                         tags$div(class = "carousel-caption",
                                                  tags$h3("Flood in North Carolina", style = "position: absolute; bottom: 0; right: 0;")
                                         )
                                ),
                                tags$div(class = "item",
                                         tags$img(src = "https://i.ytimg.com/vi/jWHu-MJd3YQ/maxresdefault.jpg", alt = "Image 5"),
                                         tags$div(class = "carousel-caption",
                                                  tags$h3("Snowstorm in Buffalo", style = "position: absolute; bottom: 0; right: 0;")
                                         )
                                )
                       ),
                       tags$a(class = "left carousel-control", href = "#myCarousel", `data-slide` = "prev",
                              tags$span(class = "glyphicon glyphicon-chevron-left"),
                              tags$span(class = "sr-only", "Previous")
                       ),
                       tags$a(class = "right carousel-control", href = "#myCarousel", `data-slide` = "next",
                              tags$span(class = "glyphicon glyphicon-chevron-right"),
                              tags$span(class = "sr-only", "Next")
                       )
              ),
              
             
              h4("About"),
              p("The tool can be used to answer many interesting questions and explore trends. A user could analyze trends in the frequency and types of disasters over years or decades. It can also be used to identify areas more prone to certain types of disasters. By looking at which programs are most often activated, and understanding how often Tribal Nations independently request assistance we can get insights into the unique needs and challenges of these communities. Experts can use these insights to make recommendations for resource allocation."),
              h4("Importance"),
              p("The dataset offers crucial details on the different types of disasters, and the response procedures that were activated. This data can be extremely important, for emergency management agencies, policy makers, and even companies who have to be prepared for such catastrophes. This can offer insights into how different disasters trigger different types of aid and responses. It can also be intriguing for academic research, particularly in areas like public policy, sociology, and environmental science."),
              
              h1("Findings"),
              p("The tool offers a multi-faceted analysis of disaster data, providing insights into the frequency and types of incidents over time. It also sheds light on government response measures, specifically which programs are most frequently activated during disasters.  It also shows seasonal and geographical insights, helping to identify trends and patterns that are crucial for disaster preparedness and management."),
              
              box(
                title = "Insights on the Data",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                h3("Size of the Data"),
                textOutput("dataSize"),
                h3("Total Number of Disasters Every Year"),
                plotOutput("disastersPerYear"),
                p("1958 had the lowest number of disasters"),
                p("2020 had the highest number of disasters "),
                h3("Disaster Types"),
                p("Number of Disaster Types"),
                textOutput("numIncidentTypes"),
                tableOutput("incidentTypes")
              ),
              
              box(
                title = "State-Specific Insights",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                uiOutput("stateInsights"),
                div(style = "overflow-x: auto;",  
                    tableOutput("incidentStateTable")),
              ),
              
              box(
                title = "Seasonal Insights",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                h1("Seasonal Trends in Disasters"),
                plotOutput("seasonalTrendsPlot"),
                textOutput("seasonalInsights")
              ),
              
              box(
                title = "Program Activation Insights",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                box(title = "Tribal Requests", width = 6, textOutput("tribalRequests")),
                box(title = "Program Activation Stats", width = 6, textOutput("mostCommonProgram")),
                tableOutput("stateProgramTable")
              )
    
              
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
      cleaned_data %>% select(-designatedArea,-id, -hash),
      options = list(scrollX = TRUE)
    )
  })
  
  # Additional logic from your snippet
  declaration_counts_total <- cleaned_data %>%
    group_by(state) %>%
    summarise(DeclarationCount = n(), .groups = "drop")
  
  declaration_counts_sub <- cleaned_data %>%
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
      ggplot(cleaned_data, aes(x = "", fill = incidentType)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(
          title = paste("Fractions of Natural Disasters in", input$state_string),
          x = NULL,
          y = NULL,
          fill = "Incident Type"  
        ) +
        theme_minimal()
    }
    
    else
      {state_data <- cleaned_data[cleaned_data$state == input$state_string, ]
        ggplot(state_data, aes(x = "", fill = incidentType)) +
          geom_bar(width = 1) +
          coord_polar(theta = "y") +
          labs(
            title = paste("Fractions of Natural Disasters in", input$state_string),
            x = NULL,
            y = NULL,
            fill = "Incident Type"  
          ) +
          scale_fill_brewer(palette = "Set3") +
          theme_minimal()}
  })
  
  #For the trends tab 
  output$linePlot <- renderPlotly({
    filtered_data <- cleaned_data %>%
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
      ggtitle(paste("Frequency of Selected Incident Type Over Years in", ifelse(input$state == "None", "All States", input$state))) +
      xlab("Years")+
      ylab("Frequency")
    
    }
    
    ggplotly(p)
  })
  
  output$barPlot <- renderPlotly({
    filtered_data <- cleaned_data %>%
      filter(incidentType == input$disaster_type & (state == input$state | input$state == "None") & fyDeclared >= input$time_range[1] & fyDeclared <= input$time_range[2]) %>%
      group_by(fyDeclared) %>%
      summarise(frequency = n())
    
    p <- ggplot(filtered_data, aes(x = fyDeclared, y = frequency)) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Frequency of Selected Incident Type Over Specified Time Range in", ifelse(input$state == "None", "All States", input$state))) +
      xlab("Years")+
      ylab("Frequency")
    
    ggplotly(p)
  })
  
  #For the program activation tab 
  output$barChart <- renderPlotly({
    filtered_data <- cleaned_data
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
      ggtitle("Number of Times Each Program is Activated")+
      xlab("Types of Programs")+
      ylab("Count")
    
    
    ggplotly(p)
  })
  
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
  
  #For the Findings tab
  output$dataSize <- renderText({
    paste(nrow(cleaned_data), " rows")
  })
  
  
  output$disastersPerYear <- renderPlot({
    yearly_data <- cleaned_data %>%
      group_by(fyDeclared) %>%
      summarise(total_disasters = n()) %>%
      arrange(desc(total_disasters))
    
    highest_year <- yearly_data[1,]
    lowest_year <- yearly_data[nrow(yearly_data),]
    
    ggplot(yearly_data, aes(x = fyDeclared, y = total_disasters)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = total_disasters), vjust = -0.5) +
      annotate("text", x = highest_year$fyDeclared, y = highest_year$total_disasters, label = "Highest", color = "red") +
      annotate("text", x = lowest_year$fyDeclared, y = lowest_year$total_disasters, label = "Lowest", color = "blue") +
      scale_x_continuous(breaks = seq(min(yearly_data$fyDeclared), max(yearly_data$fyDeclared), by = 5))+
      xlab("Years")+
      ylab("Count")
  })
  
  output$numIncidentTypes <- renderText({
    num_types <- n_distinct(cleaned_data$incidentType)
    return(num_types)
  })
  
  output$incidentTypes <- renderTable({
    incident_data <- cleaned_data %>%
      group_by(incidentType) %>%
      summarise(total_incidents = n()) %>%
      arrange(desc(total_incidents))
    
    highest_incident <- incident_data[1,]
    lowest_incident <- incident_data[nrow(incident_data),]
    
    incident_data$Highlight <- ""
    incident_data$Highlight[incident_data$incidentType == highest_incident$incidentType] <- "Highest"
    incident_data$Highlight[incident_data$incidentType == lowest_incident$incidentType] <- "Lowest"
    
    incident_data
  })
  
  output$stateInsights <- renderUI({
    state_data <- cleaned_data %>%
      group_by(state) %>%
      summarise(total_disasters = n()) %>%
      arrange(desc(total_disasters))
    
    highest_state <- state_data[1,]
    lowest_state <- state_data[nrow(state_data),]
    
    tagList(
      tags$ul(
        tags$li(style = "color:black;", paste("State with Highest number of disasters: ", highest_state$state, " with ", highest_state$total_disasters, " disasters")),
        tags$li(style = "color:black;", paste("State with Lowest number of disasters: ", lowest_state$state, " with ", lowest_state$total_disasters, " disasters"))
      )
    )
  })
  
  output$incidentStateTable <- renderTable({
    incident_state_data <- cleaned_data %>%
      group_by(state, incidentType) %>%
      summarise(total_incidents = n())
    
    spread_data <- spread(incident_state_data, key = incidentType, value = total_incidents)
    
    spread_data
  })
  
  output$seasonalTrendsPlot <- renderPlot({
    cleaned_data$month <- lubridate::month(lubridate::ymd_hms(cleaned_data$declarationDate), label = TRUE)
    monthly_data <- cleaned_data %>%
      group_by(month) %>%
      summarise(total_disasters = n()) %>%
      arrange(desc(total_disasters))
    
    ggplot(monthly_data, aes(x = month, y = total_disasters)) +
      geom_bar(stat = "identity") +
      ggtitle("Total Number of Disasters by Month")+
      xlab("Months")+
      ylab("Count")
  })
  
  output$seasonalInsights <- renderText({
    cleaned_data$month <- lubridate::month(lubridate::ymd_hms(cleaned_data$declarationDate), label = TRUE)
    monthly_data <- cleaned_data %>%
      group_by(month) %>%
      summarise(total_disasters = n()) %>%
      arrange(desc(total_disasters))
    
    highest_month <- monthly_data$month[which.max(monthly_data$total_disasters)]
    lowest_month <- monthly_data$month[which.min(monthly_data$total_disasters)]
    
    paste(highest_month, "has the highest number of disasters.", lowest_month, "has the lowest number of disasters.")
  })
  
  output$tribalRequests <- renderText({
    total_disasters <- nrow(cleaned_data)
    tribal_requests <- sum(fema_data$tribalRequest == 1)
    percentage_tribal <- (tribal_requests / total_disasters) * 100
    paste("Percentage of disasters with Tribal Requests: ", round(percentage_tribal, 2), "%")
  })
  
  output$mostCommonProgram <- renderText({
    # Summing up the program activations
    program_data <- cleaned_data %>% 
      summarise(
        IH = sum(ihProgramDeclared, na.rm = TRUE),
        IA = sum(iaProgramDeclared, na.rm = TRUE),
        PA = sum(paProgramDeclared, na.rm = TRUE),
        HM = sum(hmProgramDeclared, na.rm = TRUE)
      )
    
    # Finding the most commonly activated program
    most_common_program <- names(which.max(unlist(program_data)))
    
    paste("Most commonly activated program for disasters:", most_common_program, "(Public Assistance Program)")
  })
  
  output$stateProgramTable <- renderTable({
    program_by_state <- cleaned_data %>%
      group_by(state) %>%
      summarise(
        IH = sum(ihProgramDeclared, na.rm = TRUE),
        IA = sum(iaProgramDeclared, na.rm = TRUE),
        PA = sum(paProgramDeclared, na.rm = TRUE),
        HM = sum(hmProgramDeclared, na.rm = TRUE)
      )
    
    program_by_state$Total = rowSums(program_by_state[-1], na.rm = TRUE)
    
    program_by_state$IH_Percent = (program_by_state$IH / program_by_state$Total) * 100
    program_by_state$IA_Percent = (program_by_state$IA / program_by_state$Total) * 100
    program_by_state$PA_Percent = (program_by_state$PA / program_by_state$Total) * 100
    program_by_state$HM_Percent = (program_by_state$HM / program_by_state$Total) * 100
    
    program_by_state$Activated_Programs <- apply(program_by_state[, 2:5], 1, function(row) {
      activated_programs <- paste(names(row), "(", round(row / sum(row) * 100, 1), "%)", sep = "")
      paste(activated_programs, collapse = ", ")
    })
    
    program_by_state %>% select(state, Activated_Programs)
  })
  
  
  
  
  #histogram
  output$histogram <- renderPlotly({
    selected_state <- input$state1
    selected_year <- input$year
    
    if (selected_state != "All") {
      filtered_data <- assistance_data %>%
        filter(stateCode == selected_state, fyDeclared == selected_year) %>%
        group_by(incidentType) %>%
        summarise(TotalDamageCost = sum(totalAppDamageCost))
      if(nrow(filtered_data) == 0) {
        p <- ggplot(filtered_data) +
          ggtitle("No data available for selected options") +
          theme_minimal()
      } else {
      
      p <- ggplot(filtered_data, aes(x = incidentType, y = TotalDamageCost, fill = incidentType)) +
        geom_bar(stat = "identity") +
        labs(x = "Incident Type", y = "Total Damage cost (in Million $)") +
        scale_y_continuous(labels = scales::number_format(scale = 1e-6))+
        coord_flip()+
        ggtitle(paste("Total Damage Costs by Incident Type in", selected_state, "for the Year",selected_year))+
        theme(plot.title = element_text(hjust = 0.5))
    } 
      }
    else {
      filtered_data <- assistance_data %>%
        filter(fyDeclared == selected_year) %>%
        group_by(stateCode, incidentType) %>%
        summarise(TotalDamageCost = sum(totalAppDamageCost))
      
        p <- ggplot(filtered_data, aes(x = incidentType, y = TotalDamageCost, fill = incidentType)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Incident Type", y = "Total Damage cost (in Million $)") +
          scale_y_continuous(labels = scales::number_format(scale = 1e-6))+
          coord_flip()+
          ggtitle(paste("Total Damage Costs by Incident Type in the US for the Year", selected_year))+
          theme(plot.title = element_text(hjust = 0.5))
      }
      ggplotly(p)
  })
  
  #cost plot
  output$CostlinePlot <- renderPlotly({
    selected_state <- input$state1
    if (selected_state != "All"){
      filtered_data <- assistance_data %>%
        filter(stateCode == selected_state) %>%
        group_by(fyDeclared) %>%
        summarise(TotalDamageCost = sum(totalAppDamageCost)) %>%
        ungroup() %>%
        arrange(fyDeclared)
      
      p <- ggplot(filtered_data, aes(x = fyDeclared, y = TotalDamageCost)) +
        geom_line() +
        geom_point() +
        labs(x = "Incident Type", y = "Total Damage cost (in Million $)") +
        scale_y_continuous(labels = scales::number_format(scale = 1e-6))+
        ggtitle(paste("Total Damage Costs Over Years in", selected_state))+
        theme(plot.title = element_text(hjust = 0.5))
    } else{
      filtered_data <- assistance_data %>%
        group_by(fyDeclared) %>%
        summarise(TotalDamageCost = sum(totalAppDamageCost)) %>%
        ungroup() %>%
        arrange(fyDeclared)
      
      p <- ggplot(filtered_data, aes(x = fyDeclared, y = TotalDamageCost)) +
        geom_line() +
        geom_point() +
        labs(x = "Incident Type", y = "Total Damage cost (in Million $)") +
        scale_y_continuous(labels = scales::number_format(scale = 1e-6))+
        ggtitle(paste("Total Damage Costs Over Years in the US"))+
        theme(plot.title = element_text(hjust = 0.5))
      
    }
    
    
    
    ggplotly(p)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)