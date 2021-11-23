library(tidyverse)
library(dplyr)
library(maps)
library(openintro)

# load in data set
county <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# creating a smaller data set with only the columns of interest
county_df <- select(county, year, fips, county_name, state, region, total_pop_15to64, black_pop_15to64, black_prison_adm, white_prison_adm, total_prison_adm)
  
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

# select the specific cols
selected_df <- select(county_df, year, region, black_pop_15to64, total_pop_15to64, black_prison_adm, total_prison_adm)
# make new cols of proportions
variable_chart_df <- mutate(selected_df, prop_pop_15to64 = black_pop_15to64/total_pop_15to64, prop_prison_adm = black_prison_adm/total_prison_adm)
# filter so that only the data from 2000 is shown
variable_chart_2000 <- filter(variable_chart_df, year == 2000)
# create graph
ggplot(data = variable_chart_2000, aes(x = prop_pop_15to64, y = prop_prison_adm, color = region)) + geom_point() + ggtitle("
       Prop of black prison admission vs. prop of black population") + xlab("black prop ages 15-64") + ylab("black prop in prison admission")

# MAP---------------------------------------------------------------------------
# black prison admission for each county

map_df <- county_df %>%
  filter(year == 2013)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(map_df, by = "fips")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_prison_adm), color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(min(map_data$black_prison_adm), max(map_data$black_prison_adm)), na.value = "white", low = "yellow", high = "red", name = "population") +
  blank_theme +
  ggtitle("Black prision admissions in the US in 2013")

# Your color scale needs a legend with a clear label
