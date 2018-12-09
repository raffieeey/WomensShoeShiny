library("shiny")
library(ggplot2)
library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(tools)

dataset<-read.csv("cleaned_data_color.csv",sep=" ")
dataset$urls <- as.character(dataset$urls)

dataset <- mutate(dataset,price_range = ifelse(dataset$price.avg_USD <=50, "0 ~ 50",
                                               ifelse(dataset$price.avg_USD >50 & dataset$price.avg_USD <= 100, "51 ~ 100",
                                                      ifelse(dataset$price.avg_USD > 100 & dataset$price.avg_USD <= 500, " 100 ~ 500",
                                                             ifelse(dataset$price.avg_USD > 500, "500 and above.","null")))))


dataset <- mutate(dataset,weight_range = ifelse(dataset$weight_KG <= 0.499, "Below 0.5 Kg",
                                                ifelse(dataset$weight_KG >0.499 & dataset$weight_KG < 1, "0.5 to 1 Kg",
                                                       ifelse(dataset$weight_KG > 1, "1 Kg and above",""))))
dataset<- mutate(dataset, trend = ifelse(dataset$date_trend <= 30, "Latest",
                                  ifelse(dataset$date_trend >30 &dataset$date_trend<=37 ,"Selling Fast",
                                  ifelse(dataset$date_trend> 37 ,"Vintage",date_trend
                                         ))))


ui <- fluidPage(
  titlePanel("Features"),
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      selectInput(inputId = "price", 
                  label = "Price range:",
                  choices = c( "0 ~ 50", "51 ~ 100"," 100 ~ 500", "500 and above.")
                  ,selected = "0 ~ 50"
      ),
      
      selectInput(inputId = "weight",
                  label = "Weight range" ,
                  choices = c("Below 0.5 Kg","0.5 to 1 Kg","1 Kg and above"),
                  selected = "Below 0.5 Kg"
                  ),
      
      selectInput(inputId = "preference",
                  label = "Preference" ,
                  choices = c("Latest","Selling Fast","Vintage"),
                  selected = "Selling Fast"
      ),
      selectInput(inputId = "color",
                  label = "Colors" ,
                  choices = c("Black" , "Blue",   "Bronze" ,"Brown",  "Gold"   ,"Green",  "Grey"  , "Multi",  "Orange"
                               , "Pink" ,  "Prints" ,"Purple" ,"Red"  ,  "Silver" ,"White"  ,"Yellow","Other"),
                  selected = "Other"
      )
  
    ),
    
    # Output:
    mainPanel(
      tabsetPanel(type= "tab",
                  tabPanel("Image Shoes",plotOutput("imageshoes")),
                  tabPanel("Shoe Info",tableOutput("dataset"))
                  )
    )
  )
)

server <- function(input, output, session) {
  
  
  price_selected <- reactive({
    req(input$price) 
  })
  
  weight_selected <- reactive({
    req(input$weight) 
  })
  
  color_selected<-reactive({
    req(input$color)
  })
  library("jpeg")
  
  getImage <- function(link){
      tryCatch({
        download.file(link ,'image.jpg', mode = 'wb')
        #imagee <- load.image('image.jpg')
        jj<- readJPEG("image.jpg",native=TRUE)
        plot(0:10,0:10,type="n",ann=FALSE,axes=FALSE)
        
        rasterImage(jj,0,0,10,10)
        
        
      }, error = function(err) {
        #error cant get image!
      })
      
  }
  
#
  
  output$imageshoes <- renderPlot({
    a = subset(dataset,dataset$price_range == input$price & dataset$weight_range == input$weight & dataset$trend == input$preference & dataset$colors==input$color)
    op <- par(no.readonly=TRUE)
    par(mfrow=c(3,3), mar=c(.1,.1,.1,.1))
    
    for (i in 1:9 ){
      
      link = strsplit(a$urls[i],",")[[1]][1]
      getImage(link)
      
      text(4,8.5, col="black", cex=2, paste('Avg$',a$price.avg_USD[i]))
      text(4, 10, col="black", cex=2,font =4, paste('Brand:',a$brand[i]))
    }
      par(op)
    }
  )
  
  output$dataset <- renderTable({
    a = subset(dataset,dataset$price_range == input$price & dataset$weight_range == input$weight & dataset$trend == input$preference&dataset$colors ==input$color)
    a$urls = strsplit(a$urls,",")[[1]][1]
    a[,c("name","Onlinemerchant","price.avg_USD","urls")]
    
  })
  
}

  

# Create Shiny app object
shinyApp(ui = ui, server = server)