library(janitor)
library(readr)
library(chron)
library(lubridate)
library(tidyverse)


# Read in data for each year. 
# I used data from 2 different sources (both sources scraped data from baa.org)
# The first source gives me data from 2001-2013.
# For this source I had to get rid of wheelchair and handcycle participants. 
# For the second data source (2015-2017), I just needed to clean up variable names.

results_2001 <- read_csv("2001/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) 

results_2002 <- read_csv("2002/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H"), 
         small_division_1 = str_detect(division, "/ 2"),
         small_division_2 = str_detect(division, "/ 4")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE & 
         small_division_1 == FALSE & small_division_2 == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) 

results_2003 <- read_csv("2003/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0")) %>%
  filter(name != "Corral, Ricardo V.")

results_2004 <- read_csv("2004/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H"), 
         small_division_1 = str_detect(division, "/ 5"),
         small_division_2 = str_detect(division, "/ 2")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE &
           small_division_1 == FALSE & small_division_2 == FALSE) %>%
  mutate(official_time = as_datetime(official)) %>%
  mutate(official_time = str_remove(official_time, "1970-01-01 00:0"))

results_2005 <- read_csv("2005/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2006 <- read_csv("2006/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2007 <- read_csv("2007/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2008 <- read_csv("2008/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2009 <- read_csv("2009/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

results_2010 <- read_csv("2010/results.csv") %>%
  mutate(wheelchair = str_detect(bib, "W"),
         handcycle = str_detect(bib, "H")) %>%
  filter(wheelchair == FALSE & handcycle == FALSE)

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

