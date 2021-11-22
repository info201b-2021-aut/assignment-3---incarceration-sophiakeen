library(tidyverse)
library(dplyr)

# load in data set
county <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# creating a smaller data set with only the columns of interest
county_df <- select(county, year, county_name, state, region, total_pop_15to64, black_pop_15to64, black_prison_adm, white_prison_adm, total_prison_adm)
  
# SUMMARY-----------------------------------------------------------------------
# What is the average value of black prison admissions across all the counties (in the most recent year)?
# answer: 108.2567
#filter by most recent year of values with prison admissions
most_recent_year_adm <- filter(county_df, year == "2013")
#take the mean
avg_black_prison_adm <- mean(most_recent_year_adm$black_prison_adm, na.rm = TRUE)

# What is the average value of white prison admissions across all the counties (in the most recent year)?
# answer: 108.7893
#take the mean
avg_white_prison_adm <- mean(most_recent_year_adm$white_prison_adm, na.rm = TRUE)

# What county is black prison admission the highest (in the most recent year)?
# answer: Cook County, IL
# max black prison admission
max_black_prison_adm <- filter(most_recent_year_adm, black_prison_adm == max(black_prison_adm, na.rm = TRUE))
# select the county
county_of_max <- select(max_black_prison_adm, county_name, state)

# What is the highest black prison admission number (in the most recent year)?
# answer: 10911
# select black_prison_admission from county of max
black_adm_in_cook_county <- select(max_black_prison_adm, black_prison_adm)

# What is the white prison admissions in the county with the highest black prison admissions?
# answer: 1366
# select white_prison_admission from county of max
white_adm_in_cook_county <- select(max_black_prison_adm, white_prison_adm)

# TRENDS OVER TIME--------------------------------------------------------------

# proportion of black prison admissions per year from 2000 to 2013 in 4 regions in the United States
# group by year and then by region 
grouped_year_and_region <- group_by(county_df, year, region)
# summarize the groups by taking the mean of each 
summarized <- summarise(grouped_year_and_region, avg = mean((black_prison_adm/total_prison_adm), na.rm = TRUE))
# only include years 2000 to 2013
summarized_2000to2013 <- filter(summarized, year >= 2000 && year <= 2013)
# create graph
ggplot(data = summarized_2000to2013, aes(x = year, y = avg, color = region)) + geom_line() + ggtitle("
       prop of black prison admissions from 2000 - 2013 in the United States") + ylab("propotion of black prison admissions") 

# VARIABLE COMPARISON CHART-----------------------------------------------------
# this chart compares the proportion of black prison admissions to the proportion of black residents aged 15 to 64 in the county
# we would expect a one to one ratio and a straight line

# select the specific cols
selected_df <- select(county_df, year, region, black_pop_15to64, total_pop_15to64, black_prison_adm, total_prison_adm)
# make new cols of proportions
variable_chart_df <- mutate(selected_df, prop_pop_15to64 = black_pop_15to64/total_pop_15to64, prop_prison_adm = black_prison_adm/total_prison_adm)
# filter so that only the data from 2000 is shown
variable_chart_2000 <- filter(variable_chart_df, year == 2013)
# create graph
ggplot(data = variable_chart_2000, aes(x = prop_pop_15to64, y = prop_prison_adm, color = region)) + geom_point() + ggtitle("
       Prop of black prison admission vs. prop of black population") + xlab("black prop ages 15-64") + ylab("black prop in prison admission")

# If you choose to add a color encoding (not required), you need a legend for your different color and a clear legend title
  
# MAP---------------------------------------------------------------------------
# show how a variable is distributed geographically. Again, think carefully about what such a comparison means, and want to communicate to your user (you may have to find relevant trends in the dataset first!). Here are some requirements to help guide your design:
# Your map needs a title
# Your color scale needs a legend with a clear label
# Use a map based coordinate system to set the aspect ratio of your map (see reading)
# Use a minimalist theme for the map (see reading)
