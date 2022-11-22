
library(shiny)
library(tidyverse)
library(dplyr)

bcl <- read.csv("bcl-data.csv")

ui <- fluidPage(
  titlePanel("BC Liquor Store Data"),
  h5("Exploring the BC Liquor Store Dataset - choose the right liquor for you!"),
  #Feature 1: Add image to the UI
  img(src = "bcliquor_logo.png", align = "center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("PriceInput", "Price", 0, 100,
                  value = c(25,40), pre = "$"),
      radioButtons("typeInput", "Type", 
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      uiOutput("countryOutput"),
   
    ),
    mainPanel(
      #Feature 2: Add text to show how many results have been found 
      textOutput("text"),
      #Feature 3: Creating individual tabs for the plot and the table 
      tabsetPanel(
        tabPanel("Plot", plotOutput("alcohol_hist")), 
      #Feature 4: Create an Interactive Table 
        tabPanel("Table", DT::dataTableOutput("data_table"))
      )
    )
  ), 
  a(href = "https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", "Link to the original data set")
)

server <- function(input, output) {
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  }) 
  
  filtered_data <- 
    reactive({
      bcl %>% filter(Price >= input$PriceInput[1] &
                       Price <= input$PriceInput[2] & 
                       Type == input$typeInput)
      
    })
  
  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>%
        ggplot(aes(Alcohol_Content)) + geom_histogram() 
    })
  

  #Feature 4: Interactive table 
  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    })
  
  #Feature 2: Adding text 
  output$text <- renderText(paste("We found", nrow(filtered_data()), "options for you!"))
}

shinyApp(ui = ui, server = server)