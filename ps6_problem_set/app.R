## PS6 PROBLEM SET

library(shiny)
library(tidyverse)
library(plotly)

social_data <- read_delim("WhatsgoodlyData-6.csv")

# Data Cleaning
social_data$`Number of Voters` <- floor(social_data$Count/social_data$Percentage)
social_data$`Number of Voters` <- social_data$`Number of Voters` %>% 
  replace(is.na(.), 0)

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
              strong(em("What social platform has influenced your online
                        shopping most?"))),
            p("Random (with more than 5 responses) small (5) sample of data
              collected:"),
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
              sum(social_data$Count), " total responses were for this social
              media platform and from this demographic."),
            p(strong('Percentage'), "is the decimal percentage of participants from
              the specified demographic who agreed with this ", strong('Answer'))
          )
        )
      )
    ),
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            p("You can analyze how each demograhic viewed social media advertising
            as having affected what they purchase. Each bar represents a specific
            social media platform (or NONE) and the y-axis represents the total
            number of users."),
            fluidRow(
              # Couldn't get a second button with an option to select all
              # data working, nor a way to disable all the other buttons.
              
              # column(6, radioButtons("All", "Select All", choices = c("Off", "On"))),
              # column(6, 
               uiOutput("uniqueDemographics")
              # )
            )
          )
        ),
        mainPanel(
          wellPanel(
            plotlyOutput("barPlot")
          )
        )
      )
    ),
    tabPanel(
      "Table",
      sidebarPanel(
        p("This panel displays the total number of votes a social media
          platform received to the question: ", strong(em('What social platform
                                                          has influenced your
                                                          online shopping most?'))),
        p("This slider influences which demographics, based on size, are included
          into the total. This voter number is determined by how many responders
          CLASSIFIED as being apart of a demographic, not how many in a
          demographic responded for a certain platform."),
        sliderInput(
          "n", "Demographic needs to have at least THIS many voters",
          0, max(social_data$`Number of Voters`) - 1, 5
        )
      ),
      mainPanel(
        wellPanel(
          h2("Total Number of Responses for each Unique Answer"),
             strong("(based on size of demographics)"),
          dataTableOutput("filteredData")
        )
      )
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
  
  # Have the Plot UI show every unique demograhic in the data.
  # unique(social_data$`Segment Description`)
  output$uniqueDemographics <- renderUI({
    checkboxGroupInput("specification", "Select demographics",
                       choices = sort(unique(social_data$`Segment Description`)))
  })
  
  # Proper way to implement changing demographics without errors.
  sample <- reactive({
    # Using s1 allows for us to get around an Error if no demographic
    # is selected.
    s1 <- social_data %>% 
      filter(`Segment Description` %in% input$specification)
  })
  
  # Bar plot filtered by demographic categories for number of pollers who view
  # a certain social media platform's ads influenced their purchases.
  # Want to plot the complete, unedited graph when nothing is selected,
  # then only data found in selected demographics if one or more are selected.
  
  ####    ALSO NEED TO UPLOAD TO SHINYAPP   #####
  
  output$barPlot = renderPlotly({
    
    manipulated_data <- social_data %>% 
      select(Question, Answer, Count, `Segment Description`, `Number of Voters`) %>% 
      group_by(Answer) %>% 
      filter(`Segment Description` %in% input$specification) %>% 
      summarise(total_votes = sum(unique(Count))) %>% 
      arrange(rank(desc(total_votes)))
    
    plot_ly(data = sample(),
            x = manipulated_data$Answer,
            y = manipulated_data$total_votes,
            marker = list(size = 10),
            type = "bar")
  })
  
  # Filter the data so that statistics on which social medias had the most
  # influence on ad purchases display. Allows for users to give input about
  # at least now many votes should a categroy have before their votes are
  # valid for the total.
  output$filteredData = renderDataTable({
    social_data %>% 
      select(Question, Answer, Count, `Number of Voters`) %>% 
      group_by(Answer) %>% 
      filter(`Number of Voters` > input$n) %>% 
      summarise(total_votes = sum(unique(Count))) %>% 
      arrange(rank(desc(total_votes)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# LINK TO GITHUB REPOSATORY
# https://github.com/DLJ949/ps6-shiny

# Estimation for amount of time this problem set took:
# I'd say around 9 hours.
