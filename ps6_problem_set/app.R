## PS6 PROBLEM SET

library(shiny)
library(tidyverse)

social_data <- read_delim("../WhatsgoodlyData-6.csv")

n_distinct(social_data$`Segment Description`)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Problem Set 6: Social Media Marketing Effectiveness"),
  p("This app uses survey data collected by Adam Halper from ", 
    strong("Whatsgoodly,"), "a social polling company."),
  p("This dataset contains ", em(nrow(social_data)), "responses 
  from ", em(n_distinct(social_data$`Segment Description`)), " different questions 
  from pollers in the Generation Z and Millennial age groups about if they had
  purchased a product after seeing an ad on social media and their demographics."),
  tabsetPanel(
    tabPanel(
      "About"
    ),
    tabPanel(
      "Plot"
    ),
    tabPanel(
      "Table",
      dataTableOutput("filteredData")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter the data so that statistics on which social medias had the most
  # influence on ad purchases display
  output$filteredData = renderDataTable({
    social_data %>% 
      select(Question, Answer, Count) %>% 
      group_by(Answer) %>% 
      summarise(total_votes = sum(unique(Count))) %>% 
      arrange(rank(desc(total_votes)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
