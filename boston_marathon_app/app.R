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

### READ IN AND CLEAN DATA

# I used data from 3 different sources (all sources scraped data from baa.org). This took me a while 
# since I had to filter out handicapped results and the race times were originally in different formats. 

# Source 1: data from 2001-2012.
  # https://github.com/llimllib/bostonmarathon
  # For this source I had to get rid of wheelchair and handcycle participants. Most 
  # wheelchar/handcycle had a letter on their bib #, so I could easily filter them out, others I had 
  # to do by hand. I also needed to change the official time variable from total minutes to an 
  # hour:minute format.I decided to use an hour:minute:second format because most of the data was 
  # formatted this way. Also, note that this github repo also contains data for 2013-2014, but I found
  # that the format changed and some top finishers were missing, so I did not use the data for those years. 

# Source 2: data from 2013-2014. 
  # https://github.com/flashrbt/BostonMarathon
  # The only thing I needed to change was the official time variable name. 

# Source 3: data from 2015-2017
  # https://www.kaggle.com/rojour/boston-results
  # Cleaned up variable names. 

# I also created a variable in each dataset that shows the year that the results come from, so they 
# would be marked when I joined all the separate years together into one dataset containing all years. 


results_2001 <- read_csv("data/2001/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2001") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2002 <- read_csv("data/2002/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Upper, Randy W.",
         name != "Gillespie, Gordon J.",
         name != "Hines, Helene",
         name != "Gur, Zvi",
         name != "Traum, Dick") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2002") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2003 <- read_csv("data/2003/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Corral, Ricardo V.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2003") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2004 <- read_csv("data/2004/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Farfan, Mario O.",
         name != "Maalouf, Edward",
         name != "Beaulieu, Jack E.",
         name != "Corral, Ricardo V.",
         name != "Traum, Dick",
         name != "Hines, Helene A.",
         name != "Beehner, Harrilyn M.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2004") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2005 <- read_csv("data/2005/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Savicki, Michael W.",
         name != "Hines, Helene A.",
         name != "Dowling, Joseph M.",
         name != "Warner, Harold S. III", 
         name != "Caesar, Hilbert",
         name != "Szymanski, Monica") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2005") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2006 <- read_csv("data/2006/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Dowling, Joseph M.",
         name != "Corral, Ricardo V. PhD.",
         name != "Robinson, Rodger W.",
         name != "Hines, Helene A.",
         name != "Traum, Dick",
         name != "Beehner, Harrilyn M.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2006") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2007 <- read_csv("data/2007/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Philpott, Todd",
         name != "Skrzypinski, Arkadiusz",
         name != "Robinson, Rodger",
         name != "Hines, Helene A.",
         name != "Traum, Richard",
         name != "Beaulieu, Jack E.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2007") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2008 <- read_csv("data/2008/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Skrzypinski, Arkadiusz",
         name != "Robinson, Rodger W.",
         name != "Corral, Ricardo V.",
         name != "Ayres, Chris",
         name != "Jacobs, Daniel",
         name != "Egger, Ralph W.",
         name != "Greene, Travis", 
         name != "Spinetto, Stephen M.",
         name != "Traum, Richard",
         name != "Tyndall, Mackey J.",
         name != "Rooney, Peter",
         name != "Dentler, Minda J.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2008") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2009 <- read_csv("data/2009/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Skrzypinski, Arkadiusz",
         name != "Ayres, Chris",
         name != "Solheim, Kent",
         name != "Ayres, Chris",
         name != "Solheim, Kent",
         name != "Walding, John W.",
         name != "Robinson, Rodger W.",
         name != "Dube, Pierre L.",
         name != "Chlimon, Nano",
         name != "Rooney, Pete",
         name != "Dentler, Minda",
         name != "Schar, Brian",
         name != "Brown, Raymond C.",
         name != "Joyce, Frank",
         name != "Wood, Travis",
         name != "Traum, Dick",
         name != "O'Connor, Dan",
         name != "Penland, Matt",
         name != "Blackburn, Greg",
         name != "Minard, Mike",
         name != "Major, Ryan", 
         name != "Ripatti, Kristina L.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2009") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2010 <- read_csv("data/2010/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "De Los Santos, Alfredo",
         name != "Kinard, Andrew",
         name != "Rooney, Pete",
         name != "Murphy, Michael G.",
         name != "Reynolds, William III",
         name != "Corral, Ricardo V. PhD.", 
         name != "Triangeli, Pier Jr.",
         name != "Harvey, Ron", 
         name != "Devine, John",
         name != "Hicks, Brian", 
         name != "Tuohy, Josh", 
         name != "Smith, Craig",
         name != "Traum, Dick", 
         name != "Morales, Luis") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2010") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2011 <- read_csv("data/2011/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2011") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2012 <- read_csv("data/2012/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:")) %>%
  mutate(seconds_decimal = substr(official, 4, 6)) %>%
  mutate(seconds_decimal = as.numeric(seconds_decimal)) %>%
  mutate(seconds = (seconds_decimal * 60)) %>%
  mutate(seconds = round(seconds, digits = 0)) %>%
  mutate(seconds = str_pad(seconds, width = 2, side = "left", pad = "0")) %>%
  mutate(seconds = case_when(is.na(seconds) ~ "00",
                             TRUE ~ seconds)) %>%
  mutate(official_time = paste(official_time, seconds, sep = ":")) %>%
  mutate(year = "2012") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2013 <- read_csv("data/bm_split2013.csv") %>%
  rename(official_time = offt) %>%
  mutate(official_time = as.character(official_time)) %>%
  mutate(year = "2013") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2014 <- read_csv("data/bm_split2014.csv") %>%
  rename(official_time = offt) %>%
  mutate(official_time = as.character(official_time)) %>%
  mutate(year = "2014") %>%
  select(bib, name, age, gender, country, official_time, year)

results_2015 <- read_csv("data/marathon_results_2015.csv") %>%
  clean_names() %>%
  mutate(official_time = as.character(official_time)) %>%
  mutate(year = "2015") %>%
  select(bib, name, age, m_f, country, official_time, year) %>%
  rename(gender = m_f) 

results_2016 <- read_csv("data/marathon_results_2016.csv") %>%
  clean_names() %>%
  mutate(official_time = as.character(official_time)) %>%
  mutate(year = "2016") %>%
  select(bib, name, age, m_f, country, official_time, year) %>%
  rename(gender = m_f) 

results_2017 <- read_csv("data/marathon_results_2017.csv") %>%
  clean_names() %>%
  mutate(official_time = as.character(official_time)) %>%
  mutate(year = "2017") %>%
  select(bib, name, age, m_f, country, official_time, year) %>%
  rename(gender = m_f) 

### CREATE SHINY APP

ui <- fluidPage(theme = shinytheme("yeti"),
   
    # I used navbarPage to create an app that allows you to toggle between different tabs at the top
    # of the app
    
    navbarPage("Analyzing Boston Marathon Race Times",
   
      # TAB 1: my first tab is the introduction to my project. It contains an image of the Boston Marathon
      # logo and a written introduction about my topic. 
      
      tabPanel("About",
          htmlOutput("image"),
          htmlOutput("summary")
      ),
      
      # TAB 2: the second tab contains the interactive boxplots. 
      
      tabPanel("Data",
          sidebarLayout(
              sidebarPanel(
                  
                  # I used a selectInput for gender so that the user can toggle between genders to see
                  # the differences in the boxplots. This is immportant because my project focuses on 
                  # gender differences. 
                
                  selectInput("gender",
                              "Male/Female:",
                              choices = c("Male" = "M", "Female" = "F")
                              ),
                  
                  # I used a numericInput for the top number of participants that the boxplot displays. 
                  # The boxplots look different over time based on how large of a sample size you have. 
                  # I wanted the user to be able to see the difference between the top ten runners each
                  # year and the average times of a larger top group of runners. I included text under
                  # this input to explain what the variable means, since it might not be clear at first.
                  # I used a step value of 10 because if the user tries to use the arrows at the end of
                  # the input bar to toggle, I wanted the user to jump by increments of 10. Otherwise, 
                  # it takes a long time to click the arrows up/down and see any impact (this also causes
                  # the app to freeze and reload). The user can still enter his/her own number in the input
                  # bar, though. 
                  
                  numericInput("topnum",
                               "Top number of participants",
                                min = 10,
                                max = 150,
                                value = 50,
                                step = 10
                               ),
                  tags$h6(helpText("Enter a number to select the top number of finishers that you wish 
                                   to view. The boxplot updates based on the number of participants that
                                   you select. For example, 50 = top 50 finishers."))
                ),
              mainPanel(
                  plotOutput("boxPlot"),
                  htmlOutput("datatext")
              )
          )
       ),
   
      # TAB 3: My last tab contains some summary graphs and a couple paragraphs explaining them. 
      # I used a sidebarLayout since I want the user to be able to see the graphs and the text
      # simultaneously, without scrolling. 
      
      tabPanel("Conclusions",
               sidebarLayout(
                 sidebarPanel(
                   htmlOutput("conclusiontext")
                  ),
                 
              mainPanel(
                plotOutput("menPlot"),
                plotOutput("womenPlot")
               )
              )
        )
    )
 )


# Define server logic

server <- function(input, output) {
  
  # TAB 1 OUTPUTS: 
  
  output$image <- renderUI({
    HTML('<center><img src="Bostonmarathonlogo.png" height = 300 width = 325 ></center>')
  })
  
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
    str5 <- paste("This app explores Boston Marathon race times from 2001-2017. Specifically, this app
                  examines the differences between improvements in race times between men and women. Since
                  women have only recently begun to compete, does this mean that the women's times have 
                  improved faster than the men's times?")

      HTML(paste(h1(str1), p(str2), p(str3), h1(str4), p(str5)))
  })
  
  # TAB 2 OUTPUTS:
  
  output$boxPlot <- renderPlot({
    
    # I created a function that takes a dataset, and gives you the top number of finishers from that year,
    # based on the number that you input. I created two functions, one that filters for men, and one that
    # filters for women. 
    
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
    
    # Using this function, I created datasets that could react to what the user enters in the 
    # numericInput. For example, if they want to see the top 50 runners from each year, this code 
    # creates a dataset with the top 50 runners from each year by binding together datasets from each
    # created by my function. 
    
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
    
    # The following code determines which boxplot appears based on the gender the user enters. 
    # I had to convert the time from hour:minute:second to time in minutes, since I needed to be 
    # able to average the times togther. To do this, I couldn't find any function that would do it easily
    # for me, so I used stringr to separate out the individual parts of the hour:minute:second variable. 
    # Then I could calculate the time in minutes given the indidual pieces. 
    
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
        ylab("Average Race Time (Minutes)") 
    }  
    
    else {
      women_data %>%
        mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
        mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
        mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
        mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
        ggplot(aes(x = year, y = time_minutes)) +
        geom_boxplot() +
        ggtitle("Female Race Times Over Time") + 
        xlab("Year") +
        ylab("Average Race Time (Minutes)") 
    }
  })
  
  # Here, I thought it was important to explain the graphs, and also to give some information about 
  # outlier years. 
  
  output$datatext <- renderUI({
    str1 <- ("Women VS. Men Over the Years")
    str2 <- paste("If you toggle between men and women, you can see that there appears to be a slightly 
                  steeper slope for the women's times.")
    str3 <- paste("Outlier years")
    str4 <- paste("Note that in years 2004 and 2012, the average times are considerably slower for both 
                   sexes. This is due to the suboptimal racing conditions during those races. For both 
                   2004 and 2012, the temperature was above 80 degrees Farenheit.")
  
    HTML(paste(h2(str1), p(str2), h2(str3), p(str4)))
  })
  
  # TAB 3 OUTPUTS: 
  
  # Here I am creating summary graphs for women and men. The information is similar to what is shown on 
  # the boxplots, except here I include a line of best fit with geom_smooth. This explicitly shows the 
  # differences between men's and women's race times over the years. 

  # I needed to include the function here again for this tab to work
  
  output$menPlot <- renderPlot({ 
    top_men <- function(x, top_num){
      x %>%
        filter(gender == "M") %>%
        arrange((official_time)) %>%
        head(top_num)
    }
    
    # I created a dataset the same way I did in tab 2, except instead of the user choosing the top 
    # number of finishers, I just set all of them to include the top 100 people. 
    
    all_years_top_100_men <- bind_rows(top_men(results_2001, 100), top_men(results_2002, 100),
                                       top_men(results_2003, 100), top_men(results_2004, 100),
                                       top_men(results_2005, 100), top_men(results_2006, 100),
                                       top_men(results_2007, 100), top_men(results_2008, 100),
                                       top_men(results_2009, 100), top_men(results_2010, 100),
                                       top_men(results_2011, 100), top_men(results_2012, 100),
                                       top_men(results_2013, 100), top_men(results_2014, 100),
                                       top_men(results_2015, 100), top_men(results_2016, 100),
                                       top_men(results_2017, 100))
    
    # Using this dataset of the top 100 men from each year, I create a graph of the average time in each
    # year. I filtered out 2004 and 2012 since they were outlier years due to temperature. I set the 
    # y-axis limit to be the same as the limit on the women's graph so that the graphs can be compared
    # side-by-side in an accurate way. This is important since we are comparing the slopes of the lines. 
    
    all_years_top_100_men %>%
      mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
      mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
      mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
      mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
      group_by(year) %>%
      summarize(avg_time = mean(time_minutes)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year != "2004" & year != "2012") %>%
      ggplot(aes(x = year, y = avg_time)) +
      ylim(140, 185) + 
      geom_line() + 
      geom_smooth() + 
      ggtitle("Average Top 100 Male Finishing Times") +
      labs(x = "Year", y = "Average Time of Top 100 Finishers (Minutes)")
  })
  
  # Repeat the same proccess with the women's graph. 
  
  output$womenPlot <- renderPlot({
    top_women <- function(x, top_num){
      x %>%
        filter(gender == "F") %>%
        arrange((official_time)) %>%
        head(top_num)
    }
    all_years_top_100_women <- bind_rows(top_women(results_2001, 100), top_women(results_2002, 100),
                                         top_women(results_2003, 100), top_women(results_2004, 100),
                                         top_women(results_2005, 100), top_women(results_2006, 100),
                                         top_women(results_2007, 100), top_women(results_2008, 100),
                                         top_women(results_2009, 100), top_women(results_2010, 100),
                                         top_women(results_2011, 100), top_women(results_2012, 100),
                                         top_women(results_2013, 100), top_women(results_2014, 100),
                                         top_women(results_2015, 100), top_women(results_2016, 100),
                                         top_women(results_2017, 100))
    all_years_top_100_women %>%
      mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
      mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
      mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
      mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
      group_by(year) %>%
      summarize(avg_time = mean(time_minutes)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year != "2004" & year != "2012") %>%
      ggplot(aes(x = year, y = avg_time)) +
      ylim(140, 185) + 
      geom_line() + 
      geom_smooth() + 
      ggtitle("Average Top 100 Female Finishing Times") +
      labs(x = "Year", y = "Average Time of Top 100 Finishers (Minutes)")
  })
  
    # I included some of my conclusions that I drew from the graphs. I used the lm() function on the 
    # datasets that I created (all_years_top_100_men and all_years_top_100_women) so that I could find
    # the correlation between the average time and the year for both men and women. The code I used:
       # lm(avg_time ~ year, all_years_top_100_women)
       # lm(avg_time ~ year, all_years_top_100_men)
    
    output$conclusiontext <- renderUI({
      str1 <- ("Women Seem to be Catching Men")
      str2 <- paste("These two graphs show the average times of the top 100 finishers for both men
                    and women over the years. The years 2004 and 2012 are removed from these graphs
                    since they are known outliers (temperature above 80 degrees Farenheit on race day).")
      
      str3 <- paste("The blue line on each graphs represents a linear model fit. For men, there is a
                    correlation of -0.2718 between average race time and year. For women, the correlation
                    is much more negative, at -0.7252. This supports the hypothesis that women are 
                    improving faster than men are.")
      str4 <- paste("Further Analysis")
      str5 <- paste("This is only a prelimminary analysis. While I did find different correlations
                    between the Male and Female groups, it would be neccessary to conduct tests to 
                    determine if this difference is statisically relevant.")
      
      HTML(paste(h2(str1), p(str2), p(str3), h2(str4), p(str5)))
      
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)

