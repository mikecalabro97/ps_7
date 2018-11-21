#load in libraries with necessary functions
library(tidyverse)
library(utils)
library(fs)

#Place the csv for Mr Schroeders results into a variable called results
results <- read_csv("mt_2_results (1).csv") %>%
  #remove senate and governor races
  filter(! district == "gov") %>%
  filter(! district == "sen") %>%
  #create a column which states the state and district for each house race
  mutate(state_district = paste(state, district, sep = "-"))

#download the file from the link into a zip in my project
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

#unzip the zip
unzip("master.zip")

#gather all of the poll csv's together into a list
file_list <- dir_ls("2018-live-poll-results-master/data/")

#read in all the data and put it into 1 table, with the source indicated
poll_data <- map_dfr(file_list, read_csv, .id = "source")

#take poll data and do stuff to it to turn it into wave 3 data
#this includes...
wave_3_data <- poll_data %>%
  #taking only polls for whom the 5th to last character in the link is the number 3
  filter(str_sub(source, -5, -5) == 3) %>%
  #this ended up being more than i needed to do
  #obtain the district info in bad format (state and district number az06)
  mutate(district_info =  str_sub(source, -10, -7)) %>%
  #select just the state (as a new column)
  mutate(state = toupper(str_sub(district_info, 1, 2))) %>%
  #select just the district (as a new column)
  mutate(district = str_sub(district_info, 3, 4)) %>%
  #nicely formatted state and district
  mutate(state_district = paste(state, district, sep = "-"))



