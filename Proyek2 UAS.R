library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(reshape2)
library(dplyr)
library(DT)
library(rsconnect)

# Function to read data from uploaded file
read_data <- function(file_path) {
  read.csv(file_path, sep = ";")
}

dataset <- data.frame(
  Day = paste("Day", 1:10),
  LeftSidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  CenterPage = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  RightSidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Mengubah dataset menjadi format example_data

# UI for Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "CTR Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Entered Data", tabName = "entered", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(titlePanel(title = div("Default Data", 
                                     style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
              tabName = "home",
              mainPanel(
                column(12, DTOutput("data_table")),
                column(6, plotOutput("ctrPlot")),
                column(6, verbatimTextOutput("summary"))
              )),
      
      tabItem(titlePanel(title = div("Entered Data", 
                                     style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
              tabName = "entered",
              fluidRow(
                column(2,
                       numericInput(
                         "left_sidebar", "Left Sidebar", 0, min = 0, max = 10, step = 0.1)),
                column(2,
                       numericInput(
                         "center_page", "Center Page", 0, min = 0, max = 10, step = 0.1)),
                column(2,
                       numericInput(
                         "right_sidebar", "Right Sidebar", 0, min = 0, max = 10, step = 0.1))
              ),
              fluidRow(
                column(1,
                       actionButton("add_data", "Input Data")),
                column(1,
                       actionButton("reset_button", "Reset Data"))
              ),
              mainPanel(
                column(12, DTOutput("added_data_table")),
                column(6, plotOutput("ctrPlot1")),
                column(6, verbatimTextOutput("summary1"))
              )
      )
    )
  )
)

# Server for Shiny Dashboard
server <- function(input, output, session) {
  ##### Menu Data #####
  output$data_table <- renderDT({
    datatable(dataset, options = list(pageLength = 5))
  })
  
  data1 <- reactive({
    reshape2::melt(dataset, id.vars = "Day", variable.name = "AdPlacement", value.name = "CTR")
  })
  
  output$ctrPlot <- renderPlot({
    ggplot(data1(), aes(x = AdPlacement, y = CTR)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      labs(title = "CTR Performance by Ad Placement",
           x = "Ad Placement",
           y = "Click-Through Rate (CTR)")
  })
  
  output$summary <- renderPrint({
    result <- aov(CTR ~ AdPlacement, data = data1())
    summary_result <- summary(result)
    
    p_value <- as.numeric(summary_result[[1]][["Pr(>F)"]][1])
    significance <- ifelse(p_value < 0.05, "Statistically Significant", "Not Statistically Significant")
    
    cat(capture.output(print(summary_result), type = "message"))
    cat(paste("P-Value:", p_value, "\n"))
    cat(paste("Significance:", significance, "\n"))
  })
  
  ##### Entered Data #####
  data2 <- reactiveVal(data.frame())
  
  observeEvent(input$add_data, {
    reference_length <- length(input$left_sidebar)
    new_data <- data.frame(
      Hari = seq(nrow(data2()) + 1, nrow(data2()) + reference_length),
      LeftSidebar = input$left_sidebar,
      CenterPage = input$center_page,
      RightSidebar = input$right_sidebar
    )
    data2(rbind(data2(), new_data))
  })
  
  output$added_data_table <- renderDT({
    data2()
  })
  
  observeEvent(input$reset_button, {
    data_input(NULL)
  })
  
  data3 <- reactive({
    reshape2::melt(data2(), id.vars = "Hari", variable.name = "AdPlacement", value.name = "CTR")
  })
  
  output$ctrPlot1 <- renderPlot({
    ggplot(data3(), aes(x = AdPlacement, y = CTR)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      labs(title = "CTR Performance by Ad Placement",
           x = "Ad Placement",
           y = "Click-Through Rate (CTR)")
  })
  
  output$summary1 <- renderPrint({
    result <- aov(CTR ~ AdPlacement, data = data3())
    summary_result <- summary(result)
    
    p_value <- as.numeric(summary_result[[1]][["Pr(>F)"]][1])
    significance <- ifelse(p_value < 0.05, "Statistically Significant", "Not Statistically Significant")
    
    cat(capture.output(print(summary_result), type = "message"))
    cat(paste("P-Value:", p_value, "\n"))
    cat(paste("Significance:", significance, "\n"))
  })
}  

# Run the app
shinyApp(ui, server)
