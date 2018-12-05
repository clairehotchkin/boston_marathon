# TAB 1: About 
  # Wordcloud

library(shiny)
library(janitor)
library(readr)
library(lubridate)
library(stringr)
library(wordcloud2)
library(shinythemes)
library(sjlabelled)
library(stargazer)
library(ggrepel)
library(tidyverse)

all_years_data <- read_rds(path = "all_years_data")

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
   
    # Application title
    navbarPage("Analyzing the Boston Marathon",
   
   # TAB 1 
      tabPanel("About",
          htmlOutput("summary")
      ),
        
      tabPanel("Data",
          sidebarLayout(
              sidebarPanel(
                  numericInput("topnum",
                               "Topnum",
                                min = 1,
                                max = 100,
                                value = 100)
                ),
              mainPanel(
                  plotOutput("racePlot")
              )
          )
       )
    )
)


# Define server logic
server <- function(input, output) {

  output$summary <- renderUI({
    str <- 
    str <- ("Marathoners keep Getting Faster.")
    str <- paste("The first Boston Marathon was run in 1897. The winner was John McDermott, 
                  with a time of 2:55:10. 
                 ")
    str <- paste("more words")

      HTML(paste(h1(str1), p(str2), p(str3)))
  })
  
  output$racePlot <- renderPlot({
    top_women <- function(x, top_num){
      x %>%
        filter(gender == "F") %>%
        arrange((official_time)) %>%
        head(top_num)
    }
    
    boxplot_data <- bind_rows(top_women(results_2001, input$topnum), top_women(results_2002, input$topnum),
                                      top_women(results_2003, input$topnum), top_women(results_2004, input$topnum),
                                      top_women(results_2005, input$topnum), top_women(results_2006, input$topnum),
                                      top_women(results_2007, input$topnum), top_women(results_2008, input$topnum),
                                      top_women(results_2009, input$topnum), top_women(results_2010, input$topnum),
                                      top_women(results_2011, input$topnum), top_women(results_2012, input$topnum),
                                      top_women(results_2013, input$topnum), top_women(results_2014, input$topnum),
                                      top_women(results_2015, input$topnum), top_women(results_2016, input$topnum),
                                      top_women(results_2017, input$topnum))
   
    boxplot_data %>%
      # filter(year %in% input$yearslider) %>%
      mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
      mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
      mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
      mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
      ggplot(aes(x = year, y = time_minutes)) +
      geom_boxplot() 
    })
  
  #output$distPlot <- renderWordcloud2({
   # wordcloud2(all_years_data$country)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

