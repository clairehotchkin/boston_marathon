# NOTES:
# How to account for different years have different conditions (2003 and 2004 slow years)?
# How to use function and have slider input of top runners?
# I am using marathon data, should I use track data instead? Perhaps another tab?


library(janitor)
library(readr)
library(chron)
library(lubridate)
library(stringr)
library(tidyverse)


# Read in data for each year. 
# I used data from 2 different sources (both sources scraped data from baa.org)
# The first source gives me data from 2001-2013.
# For this source I had to get rid of wheelchair and handcycle participants. 
# Most wheelchar/handcycle had a letter on their bib #, so I could easily filter them out,
# Others I had to do by hand. 
# For the second data source (2015-2017), I just needed to clean up variable names.

results_2001 <- read_csv("2001/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  mutate(year = "2001")
  
results_2002 <- read_csv("2002/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Upper, Randy W.",
         name != "Gillespie, Gordon J.",
         name != "Hines, Helene",
         name != "Gur, Zvi",
         name != "Traum, Dick") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  mutate(year = "2002")

results_2003 <- read_csv("2003/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Corral, Ricardo V.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  mutate(year = "2003")

results_2004 <- read_csv("2004/results.csv") %>%
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
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  mutate(year = "2004")

results_2005 <- read_csv("2005/results.csv") %>%
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
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  mutate(year = "2005")

results_2006 <- read_csv("2006/results.csv") %>%
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
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) 

results_2007 <- read_csv("2007/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Philpott, Todd",
         name != "Skrzypinski, Arkadiusz",
         name != "Robinson, Rodger",
         name != "Hines, Helene A.") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) 

results_2008 <- read_csv("2008/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  filter(name != "Skrzypinski, Arkadiusz",
         name != "Robinson, Rodger W.",
         name != "Corral, Ricardo V.",
         name != "Ayres, Chris",
         name != "Jacobs, Daniel",
         name != "Egger, Ralph W.",
         name != "Greene, Travis") %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) 

results_2009 <- read_csv("2009/results.csv") %>%
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
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  View()

results_2010 <- read_csv("2010/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  separate(division, into  = c("numerator", "denominator"), sep = " / ") %>%
  mutate(denominator = as.numeric(denominator)) %>%
  filter(denominator > 20) %>%
  View()

results_2011 <- read_csv("2011/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2012 <- read_csv("2012/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2013 <- read_csv("2013/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2014 <- read_csv("2014/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2015 <- read_csv("marathon_results_2015.csv") %>%
  clean_names()

results_2016 <- read_csv("marathon_results_2016.csv") %>%
  clean_names()

results_2017 <- read_csv("marathon_results_2017.csv") %>%
  clean_names() 

# Create function which returns top 25 performances 
top_25_men <- function(x){
    x %>%
    filter(gender == "M") %>%
    arrange((official)) %>%
    head(25) 
}

# IT WORKS!!!
top_25_men(results_2003) %>%
  View()

# Join together tables of top 25 men
all_years_top_men <- bind_rows(top_25_men(results_2001), top_25_men(results_2002),
                               top_25_men(results_2003), top_25_men(results_2004),
                               top_25_men(results_2005))

# Create graph of top times over the years
all_years_top_men %>%
  select(official_time, official, year, gender) %>%
  group_by(year) %>%
  summarize(avg_time = mean(official)) %>%
  ggplot(aes(x = year, y = avg_time)) +
  geom_point()
  
 
# Top countries graph
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

# Are results getting faster? Are they geting faster for women disproportionately? 
results_2017 %>%
  filter(m_f == "F") %>%
  arrange(official_time) %>%
  head(20) %>%
  mutate(avg_time = mean(official_time))

