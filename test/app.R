library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
dataset<-read.csv("D:/Documents/Quant/R present/WomensShoeShiny/cleaned_data.csv",sep="")

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Select Feature"),      # Third level header: Select Feature
      
      # Select variable for y-axis 
      sliderInput(inputId = "price", 
                  label = "Price:"
                  #,choices = unique(sort(dataset$price.avg_USD))
                  #,selected = min(dataset$price.avg_USD)
                  ,min=min(dataset$price.avg_USD)
                  ,max=max(dataset$price.avg_USD)
                  ,value=min(dataset$price.avg_USD)
                  ,step=1
                  ),
      
      selectInput(inputId = "weight", 
                  label = "Weight:",
                  choices = unique(sort(dataset$weight_KG ))
                  #,selected = min(dataset$weight_KG)
                  ),
      
      # Select variable for x-axis 
      #selectInput(inputId = "weight", 
      #label = "Weight:",
      #choices = unique(dataset$weight_KG), 
      #selected = min(dataset$weight_KG)),
      
      hr(),                # Horizontal line for visual separation
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
      
    ),
    
    # Output:
    mainPanel(
      h3("Data table"),     # Third level header: Data table
      DT::dataTableOutput(outputId = "datatable")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  price_selected <- reactive({
    req(input$price) # ensure availablity of value before proceeding
    filter(dataset, price.avg_USD %in% input$price)
  })
  
  weight_selected <- reactive({
    req(input$weight) # ensure availablity of value before proceeding
    filter(price_selected(), weight_KG %in% input$weight)
  })
  
  
  
  # Print data table if checked
  output$datatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = weight_selected(),price_selected(), 
                    options = list(pageLength = 10))
    }
  )
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)