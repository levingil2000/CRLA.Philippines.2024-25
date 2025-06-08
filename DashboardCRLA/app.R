library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(DT)
library(plotly)

# Load data
df <- read.csv("test.csv")

# UI
ui <- fluidPage(
  titlePanel("Reading Index Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("division", "Select Division:",
                  choices = unique(df$Division.x),
                  selected = unique(df$Division.x)[1],
                  multiple = TRUE),
      checkboxGroupInput("indices", "Select Indices:",
                         choices = c("Reading_Index", "G3_MT_Index", "G3_Eng_Index", "G3_Fil_Index"),
                         selected = c("Reading_Index"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summaryTable")),
        tabPanel("Reading Index by Division", plotlyOutput("divisionPlot")),
        tabPanel("Distribution", plotlyOutput("distributionPlot")),
        tabPanel("G3 Index Comparison", plotlyOutput("g3IndexPlot")),
        tabPanel("Heatmap", plotOutput("heatmapPlot")),
        tabPanel("Top 10", plotOutput("top10Plot"))  # NEW TAB
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filteredData <- reactive({
    df %>% filter(Division.x %in% input$division)
  })
  
  output$summaryTable <- renderDT({
    filteredData() %>%
      group_by(Division.x, District.x) %>%
      summarise(across(c(Reading_Index, G3_MT_Index, G3_Eng_Index, G3_Fil_Index), mean, .names = "mean_{.col}"), .groups = 'drop')
  })
  
  output$divisionPlot <- renderPlotly({
    p <- filteredData() %>%
      group_by(Division.x) %>%
      summarise(mean_val = mean(Reading_Index)) %>%
      ggplot(aes(x = reorder(Division.x, -mean_val), y = mean_val)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Average Reading Index by Division", x = "Division", y = "Reading Index") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$distributionPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Reading_Index)) +
      geom_histogram(bins = 30, fill = "orange", color = "black") +
      labs(title = "Distribution of Reading Index", x = "Reading Index", y = "Count")
    ggplotly(p)
  })
  
  output$g3IndexPlot <- renderPlotly({
    data_long <- filteredData() %>%
      group_by(Division.x) %>%
      summarise(across(c(G3_MT_Index, G3_Eng_Index, G3_Fil_Index), mean), .groups = 'drop') %>%
      pivot_longer(cols = -Division.x, names_to = "Index", values_to = "Score")
    
    p <- ggplot(data_long, aes(x = Division.x, y = Score, fill = Index)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "G3 Index Comparison by Division") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$heatmapPlot <- renderPlot({
    heat_data <- filteredData() %>%
      group_by(Division.x, District.x) %>%
      summarise(Reading_Index = mean(Reading_Index), .groups = 'drop')
    
    ggplot(heat_data, aes(x = Division.x, y = District.x, fill = Reading_Index)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(title = "Heatmap of Reading Index by District and Division", x = "Division", y = "District") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$top10Plot <- renderPlot({
    top10_data <- df %>%
      group_by(Division.x, District.x) %>%
      summarise(mean_reading = mean(Reading_Index, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(mean_reading)) %>%
      slice_head(n = 10)
    
    ggplot(top10_data, aes(x = reorder(paste(District.x, Division.x, sep = ", "), mean_reading),
                           y = mean_reading, fill = Division.x)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Districts by Reading Index",
           x = "District, Division",
           y = "Average Reading Index") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
