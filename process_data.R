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
  mutate(state_district = paste(state, district, sep = "-")) %>%
  mutate(real_rep_adv = (rep_votes - dem_votes)/(rep_votes + dem_votes + other_votes)) %>%
  filter(state_district %in% wave_3$state_district) 

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

#make a new table for wave 3
wave_3 <- wave_3_data %>%
  #groups by response
  group_by(state_district, response) %>%
  #summarizes the number of responses and final weight average for each group
  summarize(responses = n(), avg_weight = mean(final_weight)) %>%
  #weights the responses accordingly
  mutate(weighted_total = avg_weight * responses) %>%
  #select only needed varaibles
  select(state_district, response, weighted_total) %>%
  #spread data with response as columns, weighted total as data
  spread(response, weighted_total)

#makes a table with the total weighted responses
vote_totals <- wave_3_data %>%
  group_by(state_district) %>%
  summarize(total_weighted = n()*mean(final_weight))

#combines the totals to the wave 3 response data for percentage purposes
wave_3 <- left_join(wave_3, vote_totals, by = "state_district")

#changes NAs to 0s for addition purposes
wave_3[is.na(wave_3)] <- 0

#calculates the republican advantage and only keeps that and the state/district
wave_3 <- wave_3 %>%
  mutate(poll_rep_adv = (Rep - Dem)/total_weighted) %>%
  select(state_district, poll_rep_adv) 

#combines the rep adv for polls and for actual results
poll_and_results <- left_join(wave_3, results, by = "state_district") %>%
  #only keeps the columns that we need
  select(state_district, poll_rep_adv, real_rep_adv) %>%
  #add the polling error
  mutate(polling_error = real_rep_adv - poll_rep_adv) %>%
  #removes the gov and senate races that snuck into my data
  filter(! is.na(real_rep_adv)) %>%
  #arranges based on polling error
  arrange(polling_error)

#will eventually add a new column to poll_and_results for % of female voters 
wave_3_gender <- wave_3_data %>%
  #groups every state/district / gender combo
  group_by(state_district, gender) %>%
  #summarizes number of responses and avg weight per group
  summarize(responses = n(), avg_weight = mean(final_weight)) %>%
  #weights the responses accordingly
  mutate(weighted_total = avg_weight * responses) %>%
  #selects only needed variables
  select(state_district, gender, weighted_total) %>%
  #spreads data with gender as columns, weighted_total as data
  spread(gender, weighted_total) %>%
  #calculates percentage of female respondants
  mutate(percent_female = Female/(Male + Female)) %>%
  #select only state/district and female percent
  select(state_district, percent_female) %>%
  #arranges by female percent
  arrange(percent_female)

#add this female percent column to poll_and_results
poll_and_results2 <- left_join(poll_and_results, wave_3_gender, by = "state_district")
  
#this code does the same as the code above, but for party id
wave_3_partyid <- wave_3_data %>%
  group_by(state_district, partyid) %>%
  summarize(responses = n(), avg_weight = mean(final_weight)) %>%
  mutate(weighted_total = avg_weight * responses) %>%
  select(state_district, partyid, weighted_total) %>%
  spread(partyid, weighted_total) %>%
  mutate(percent_independant = `Independent (No party)` / (`Independent (No party)` + Democrat + Republican)) %>%
  select(state_district, percent_independant)

#add the new party id column to poll_and results
poll_and_results_final <- left_join(poll_and_results2, wave_3_partyid, by = "state_district")

#helps move data to my app
write_rds(poll_and_results_final, path = "pset7_app/ps7_app_data")
