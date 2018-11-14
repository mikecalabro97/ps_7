library(tidyverse)
library(utils)
library(fs)

raw <- read_csv("mt_2_results (1).csv") %>%
  filter(! district == "gov") %>%
  filter(! district == "sen")

#download the file from the link into a zip in my project
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

#unzip the zip
unzip("master.zip")

file_list <- dir_ls("2018-live-poll-results-master/data/")

#read in all the data and put it into 1 table, with the source indicated
poll_data <- map_dfr(file_list, read_csv, .id = "source")

wave_3_data <- poll_data %>%
  filter(str_sub(source, -5, -5) == 3) %>%
  mutate(district_info =  str_sub(source, -10, -7)) %>%
  mutate(state = toupper(str_sub(district_info, 1, 2))) %>%
  mutate(district = str_sub(district_info, 3, 4))
