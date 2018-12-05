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
library(ggplot2)
library(tidyverse)

all_years_data <- read_rds(path = "all_years_data")

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
   
    # Application title
    navbarPage("Analyzing Boston Marathon Race Times",
   
   # TAB 1 
      tabPanel("About",
          htmlOutput("summary")
      ),
        
      tabPanel("Data",
          sidebarLayout(
              sidebarPanel(
                  selectInput("gender",
                              "Male/Female:",
                              choices = c("Male" = "M", "Female" = "F")
                              ),
                  numericInput("topnum",
                               "Topnum",
                                min = 1,
                                max = 100,
                                value = 100
                               )
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
    str1 <- ("Runners keep Getting Faster")
    str2 <- paste("The first Boston Marathon was run in 1897. The winner was John McDermott, 
                  with a time of 2:55:10. In the 2017 Boston Marathon, McDermott's time would
                  have placed 266th. Since 1897, the Boston Marathon has grown immensely in 
                  number of participants, and now draws elite runners from around the world.")
    str3 <- paste("While there's clearly been a drastic increase in speed since 1897, Boston
                 Marathon times are still getting faster. In the past 17 years, advances in 
                 sports science, runing shoe technology, along with other factors, have 
                 contributed to faster and faster times.")
    str4 <- paste("About this App")
    str5 <- paste("This app explores Boston Marathon race times from 2001-2017.")

      HTML(paste(h1(str1), p(str2), p(str3), h1(str4), p(str5)))
  })
  
  output$racePlot <- renderPlot({
    
    top_men <- function(x, top_num){
      x %>%
        filter(gender == "M") %>%
        arrange((official_time)) %>%
        head(top_num)
    }
    
    top_women <- function(x, top_num){
      x %>%
        filter(gender == "F") %>%
        arrange((official_time)) %>%
        head(top_num)
    }
    
    women_data <- bind_rows(top_women(results_2001, input$topnum), top_women(results_2002, input$topnum),
                            top_women(results_2003, input$topnum), top_women(results_2004, input$topnum),
                            top_women(results_2005, input$topnum), top_women(results_2006, input$topnum),
                            top_women(results_2007, input$topnum), top_women(results_2008, input$topnum),
                            top_women(results_2009, input$topnum), top_women(results_2010, input$topnum),
                            top_women(results_2011, input$topnum), top_women(results_2012, input$topnum),
                            top_women(results_2013, input$topnum), top_women(results_2014, input$topnum),
                            top_women(results_2015, input$topnum), top_women(results_2016, input$topnum),
                            top_women(results_2017, input$topnum))
    
    men_data <- bind_rows(top_men(results_2001, input$topnum), top_men(results_2002, input$topnum),
                          top_men(results_2003, input$topnum), top_men(results_2004, input$topnum),
                          top_men(results_2005, input$topnum), top_men(results_2006, input$topnum),
                          top_men(results_2007, input$topnum), top_men(results_2008, input$topnum),
                          top_men(results_2009, input$topnum), top_men(results_2010, input$topnum),
                          top_men(results_2011, input$topnum), top_men(results_2012, input$topnum),
                          top_men(results_2013, input$topnum), top_men(results_2014, input$topnum),
                          top_men(results_2015, input$topnum), top_men(results_2016, input$topnum),
                          top_men(results_2017, input$topnum))
    
    if(input$gender == "M") {
      men_data %>%
        mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
        mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
        mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
        mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
        ggplot(aes(x = year, y = time_minutes)) +
        geom_boxplot() +
        ggtitle("Male Race Times Over Time") +
        xlab("Year") +
        ylab("Average Race Time")
    }  
    
    else {
      women_data %>%
        # filter(year %in% input$yearslider) %>%
        mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
        mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
        mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
        mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
        #mutate(year = as.numeric(year)) %>%
        #group_by(year) %>%
        ggplot(aes(x = year, y = time_minutes)) +
        geom_boxplot() +
        ggtitle("Female Race Times Over Time") + 
        xlab("Year") +
        ylab("Average Race Time") 
        #geom_smooth()
    }
  })
  
  #output$distPlot <- renderWordcloud2({
   # wordcloud2(all_years_data$country)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

