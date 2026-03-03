library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()

ui <- dashboardPage(
  
  dashboardHeader(title="Range of Age and Height by Sex"),
  
  dashboardSidebar(
    
    selectInput("y",
                "Select Age or Height",
                choices=c("age","height"),
                selected="age")
    
  ),
  
  dashboardBody(
    
    plotOutput("plot", width="500px", height="400px")
    
  )
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    elephants %>% 
      ggplot(aes(y=.data[[input$y]],group = sex,fill=sex))+
      geom_boxplot()+
      labs(title="Range of Age and Height by Sex",x="Sex",y=input$y)
  })
  
}

shinyApp(ui, server)