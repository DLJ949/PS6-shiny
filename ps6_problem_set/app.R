## PS6 PROBLEM SET

library(shiny)
library(tidyverse)

social_data <- read_delim("../WhatsgoodlyData-6.csv")

n_distinct(social_data$`Segment Description`)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Problem Set 6: Social Media Marketing Effectiveness"),
  tabsetPanel(
    tabPanel(
      "About", 
      p("This app uses survey data collected by Adam Halper from ", 
        strong("Whatsgoodly,"), "a social polling company."),
      p("This dataset contains ", em(nrow(social_data)), "responses 
        from ", em(n_distinct(social_data$`Segment Description`)), 
        " different questions from pollers in the Generation Z and Millennial
        age groups about if they had purchased a product after seeing an ad
        on social media and their demographics."),
      sidebarLayout(
        mainPanel(
          wellPanel(
            p("All participants were asked the same question:", 
              em("What social platform has influenced your online shopping most?")),
            p("Random small (5) sample of data collected:"),
            dataTableOutput("randomSmallData")
          )
        ),
        sidebarPanel(
          wellPanel(
            p("The ", strong('Segment Description'), " gives us information on
            the type of demographic the responder was apart of. For example, 
            were attending a certain University, voted a certain way, or were
            affiliated with a religious organization?"),
            p("The ", strong('Segment Type'), " gives us context into the type
              of demographic question was asked to the responder."),
            p("The ", strong('Answer'), " categroy tells us what the responders
              answered to the question:", strong(em('What social platform
                                                       has influenced your
                                                       online shopping most?'))),
            p(strong('Count'), "indicates how many of the ",
              sum(social_data$Count), " responses were for this social media
            platform and from this demographic."),
            p(strong('Percentage'), "is the decimal percentage of participants from
              the specified demographic who agreed with this ", strong('Answer'))
          )
        )
      )
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
  
  # Filter the data to only display five random and distinct data rows with
  # questions, responses, and number of responses avaliable. To make the data
  # more interesting, only counts with more than 5 are displayed.
  output$randomSmallData = renderDataTable({
    social_data %>% 
      filter(Count > 5) %>% 
      select(`Segment Description`, `Segment Type`, Answer, Count, Percentage) %>% 
      sample_n(5)
  })
  
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

# LINK TO GITHUB REPOSATORY
# https://github.com/DLJ949/ps6-shiny
