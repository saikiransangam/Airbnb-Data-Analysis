library(shiny)
library(DT)  # interactive data tables
library(ggplot2)  # creating plots
library(dplyr)  # data manipulation

# UI
ui <- fluidPage(
    titlePanel("Advanced Airbnb Data Analysis - NYC 2019"),
    sidebarLayout(
        sidebarPanel(
            fileInput("fileInput", "Upload the NYC Airbnb Dataset",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"),
                      placeholder = "AB_NYC_2019.csv expected"),
            hr(),
            selectInput("selectVariable", "Select variable for plotting:",
                        choices = c("Room Type" = "room_type",
                                    "Neighbourhood Group" = "neighbourhood_group")),
            selectInput("summaryVariable", "Select variable for summary:",
                        choices = c("Price" = "price", "Minimum Nights" = "minimum_nights")),
            actionButton("updatePlot", "Update Plot"),
            actionButton("updateSummary", "Update Summary")
        ),
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Data View", DTOutput("dataTable")),
                       tabPanel("Plot", plotOutput("plot")),
                       tabPanel("Summary", verbatimTextOutput("summary"))
           )
        )
    )
)

# server logic
server <- function(input, output) {
    data <- reactiveVal()
    
    observeEvent(input$fileInput, {
        req(input$fileInput)
        df <- read.csv(input$fileInput$datapath)
        # Cleaning data specifically for your dataset
        df <- df[ , !(names(df) %in% c("id", "host_name", "name", "last_review"))]
        df$reviews_per_month[is.na(df$reviews_per_month)] <- 0
        data(df)
    })
    
    output$dataTable <- renderDT({
        req(data())
        datatable(data(), options = list(pageLength = 10))
    })
    
    observeEvent(input$updatePlot, {
        req(data(), input$selectVariable)
        output$plot <- renderPlot({
            plot_data <- data() %>%
                filter(!is.na(.data[[input$selectVariable]]))
            ggplot(plot_data, aes_string(x = input$selectVariable)) +
                geom_bar(fill = "blue") +
                theme_minimal() +
                labs(title = paste("Distribution of", input$selectVariable))
        })
    }, ignoreNULL = FALSE)

    output$summary <- renderPrint({
        req(input$summaryVariable != "", data())
        summary_info <- data() %>%
            summarize(
                Mean = mean(.data[[input$summaryVariable]], na.rm = TRUE),
                Median = median(.data[[input$summaryVariable]], na.rm = TRUE),
                Std_Dev = sd(.data[[input$summaryVariable]], na.rm = TRUE)
            )
        print(summary_info)
    })
}


shinyApp(ui = ui, server = server)
