# NOTES:
# How to account for different years have different conditions (2003 and 2004 slow years)?
# How to use function and have slider input of top runners?
# I am using marathon data, should I use track data instead? Perhaps another tab?

### Cool idea: show top marathon times colored by year (show that faster ones are more recent)

library(janitor)
library(readr)
library(lubridate)
library(stringr)
library(tidyverse)

year
temp
rain

### READ IN AND CLEAN DATA---------------------------------------------------------------------

# I used data from 3 different sources (all sources scraped data from baa.org)
# Source 1: data from 2001-2012.
  # https://github.com/llimllib/bostonmarathon
  # For this source I had to get rid of wheelchair and handcycle participants. Most 
  # wheelchar/handcycle had a letter on their bib #, so I could easily filter them out,
  # others I had to do by hand. To check my work: 
      # separate(division, into  = c("numerator", "denominator"), sep = " / ") %>%
      # mutate(denominator = as.numeric(denominator)) %>%
      # filter(denominator > 20)
  # I also needed to change the official time variable from total minutes to an hour:minute format.
  # I decided to use an hour:minute format because the rest of my data was formatted this way and
  # in my graphs I wanted to be able to visualize the information in an accessible way. No one talks
  # about marathon times in terms of total minutes. Note that this github repo also contains data for
  # 2013-2014, but I found that it was missing some top finishers.
# Source 2: data from 2013-2014. 
  # https://github.com/flashrbt/BostonMarathon
  # Only thing I needed to change was the official time variable name. 
# Source 3: data from 2015-2017
  # https://www.kaggle.com/rojour/boston-results
  # Cleaned up variable names. 
  
# Where to do this?
  # I added 2 variables to each results dataset: the year that the results came from, 
  # and the temperature (in degrees F) and weather for that day

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

### ANALYZE DATA----------------------------------------------------------------------------------

# Create giant table of all results 
all_years_data <- bind_rows(results_2001, results_2002, results_2003, results_2004,
                            results_2005, results_2006, results_2007, results_2008,
                            results_2009, results_2010, results_2011, results_2012,
                            results_2013, results_2014, results_2015, results_2016,
                            results_2017) %>%
  mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
  mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
  mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
  mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 ))

all_years_data %>%
write_rds(path = "all_years_data.rds")

# Temperature graph
all_years_top_men %>%
  mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
  mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
  mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
  mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
  group_by(year) %>%
  summarize(avg_time = mean(time_minutes)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(temp = case_when(year == 2001 ~ 54,
                          year == 2002 ~ 56, 
                          year == 2003 ~ 59,
                          year == 2004 ~ 86,
                          year == 2005 ~ 66,
                          year == 2006 ~ 53,
                          year == 2007 ~ 50,
                          year == 2008 ~ 53,
                          year == 2009 ~ 47,
                          year == 2010 ~ 49,
                          year == 2011 ~ 55,
                          year == 2012 ~ 87,
                          year == 2013 ~ 54,
                          year == 2014 ~ 63,
                          year == 2015 ~ 44,
                          year == 2016 ~ 61,
                          year == 2017 ~ 73)) %>%
  ggplot(aes(x = temp, y = avg_time)) +
  geom_line() + 
  geom_smooth()
  
  ggplot(aes(x = year)) + 
  geom_line(aes(y = avg_time)) +
  geom_line(aes(y = temp))
  
# Let's look at women's data
all_years_top_100_women %>%
  mutate(hours = as.numeric(substr(official_time, 1, 2))) %>%
  mutate(minutes = as.numeric(substr(official_time, 4, 5))) %>%
  mutate(seconds = as.numeric(substr(official_time, 7, 8))) %>%
  mutate(time_minutes = (hours * 60) + minutes + (seconds / 60 )) %>%
  ggplot(aes(x = year, y = time_minutes)) +
  geom_point() + 
  geom_smooth()
  
  
# Create function which returns top 25 performances

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


# Join together tables of top 25 men

all_years_top_men <- bind_rows(top_men(results_2001, 25), top_men(results_2002, 25),
                               top_men(results_2003, 25), top_men(results_2004, 25),
                               top_men(results_2005, 25), top_men(results_2006, 25),
                               top_men(results_2007, 25), top_men(results_2008, 25),
                               top_men(results_2009, 25), top_men(results_2010, 25),
                               top_men(results_2011, 25), top_men(results_2012, 25),
                               top_men(results_2013, 25), top_men(results_2014, 25),
                               top_men(results_2015, 25), top_men(results_2016, 25),
                               top_men(results_2017, 25))

all_years_top_100_men <- bind_rows(top_men(results_2001, 100), top_men(results_2002, 100),
                               top_men(results_2003, 100), top_men(results_2004, 100),
                               top_men(results_2005, 100), top_men(results_2006, 100),
                               top_men(results_2007, 100), top_men(results_2008, 100),
                               top_men(results_2009, 100), top_men(results_2010, 100),
                               top_men(results_2011, 100), top_men(results_2012, 100),
                               top_men(results_2013, 100), top_men(results_2014, 100),
                               top_men(results_2015, 100), top_men(results_2016, 100),
                               top_men(results_2017, 100))

all_years_top_25_women <- bind_rows(top_women(results_2001, 25), top_women(results_2002, 25),
                                    top_women(results_2003, 25), top_women(results_2004, 25),
                                    top_women(results_2005, 25), top_women(results_2006, 25),
                                    top_women(results_2007, 25), top_women(results_2008, 25),
                                    top_women(results_2009, 25), top_women(results_2010, 25),
                                    top_women(results_2011, 25), top_women(results_2012, 25),
                                    top_women(results_2013, 25), top_women(results_2014, 25),
                                    top_women(results_2015, 25), top_women(results_2016, 25),
                                    top_women(results_2017, 25))

all_years_top_100_women <- bind_rows(top_women(results_2001, 100), top_women(results_2002, 100),
                                    top_women(results_2003, 100), top_women(results_2004, 100),
                                    top_women(results_2005, 100), top_women(results_2006, 100),
                                    top_women(results_2007, 100), top_women(results_2008, 100),
                                    top_women(results_2009, 100), top_women(results_2010, 100),
                                    top_women(results_2011, 100), top_women(results_2012, 100),
                                    top_women(results_2013, 100), top_women(results_2014, 100),
                                    top_women(results_2015, 100), top_women(results_2016, 100),
                                    top_women(results_2017, 100))

# Create graph of top times over the years
# Could also plot temperature? (Either as line or color points)

all_years_top_men %>%
  select(official_time, year, gender) %>%
  group_by(year) %>%
  summarize(avg_time = mean(official_time)) %>%
  ggplot(aes(x = year, y = avg_time)) +
  geom_point()

 
# Top countries graph (WORD CLOUD??)
top_countries <- results_2017 %>%
  group_by(country) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  View()
results_2017 %>%
  filter(country %in% top_countries$country) %>%
  group_by(country) %>%
  ggplot(aes(x = country)) +
  geom_bar()



wordcloud2(all_years_data, size = 1)


# Are results getting faster? Are they geting faster for women disproportionately? 
results_2017 %>%
  filter(m_f == "F") %>%
  arrange(official_time) %>%
  head(20) %>%
  mutate(avg_time = mean(official_time))

