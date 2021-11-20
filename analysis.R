library(tidyverse)
library(ggplot2)
library(ggpubr)
data <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

# Summary Statistics
# Create a data frame with only the data from 2018
df_2018 <- filter(data, year == 2018)
# How many people in 2018 were in jail?
total_jail_2018 <- sum(df_2018$total_jail_pop, na.rm = TRUE)
# What is the proportion of the total population that is black?
black_prop_total <- 100 * sum(df_2018$black_pop_15to64, na.rm = TRUE) / sum(df_2018$total_pop_15to64, na.rm = TRUE)
# What is the proportion of the jailed population that is black?
black_prop_jail <- 100 * sum(df_2018$black_jail_pop, na.rm = TRUE) / total_jail_2018
# How many times greater is the proportion of the black jailed population to the
# proportion of the black general population?
black_prop_comp <- black_prop_jail/black_prop_total
# What is the proportion of the total population that is white?
white_prop_total <- 100 * sum(df_2018$white_pop_15to64, na.rm = TRUE) / sum(df_2018$total_pop_15to64, na.rm = TRUE)
# What is the proportion of the jailed population that is white?
white_prop_jail <- 100 * sum(df_2018$white_jail_pop, na.rm = TRUE) / total_jail_2018
# How many times greater is the proportion of the black jailed population to the
# proportion of the black general population?
white_prop_comp <- white_prop_jail/white_prop_total


# Trends over Time Charts
# Create a dataframe that only contains information about different races
# and their full and jail population over time
race_over_time <- data %>%
  select(year, total_pop_15to64, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64,
         native_pop_15to64, white_pop_15to64, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop,
         native_jail_pop, white_jail_pop) %>%
  group_by(year) %>%
  summarize(total_all = sum(total_pop_15to64, na.rm = TRUE),
            total_aapi = sum(aapi_pop_15to64, na.rm = TRUE),
            total_black = sum(black_pop_15to64, na.rm = TRUE),
            total_latinx = sum(latinx_pop_15to64, na.rm = TRUE),
            total_native = sum(native_pop_15to64, na.rm = TRUE),
            total_white = sum(white_pop_15to64, na.rm = TRUE),
            total_jail_all = sum(total_jail_pop, na.rm = TRUE), 
            total_jail_aapi = sum(aapi_jail_pop, na.rm = TRUE), 
            total_jail_black = sum(black_jail_pop, na.rm = TRUE), 
            total_jail_latinx = sum(latinx_jail_pop, na.rm = TRUE), 
            total_jail_native = sum(native_jail_pop, na.rm = TRUE), 
            total_jail_white = sum(white_jail_pop, na.rm = TRUE))

# Edit dataframe to show only percentages of the population
race_over_time <- race_over_time %>%
  mutate(percent_aapi = 100*(total_aapi / total_all), 
         percent_black = 100*(total_black / total_all), 
         percent_latinx = 100*(total_latinx / total_all), 
         percent_native = 100*(total_native / total_all), 
         percent_white = 100*(total_white / total_all), 
         percent_jail_aapi = 100*(total_jail_aapi / total_jail_all), 
         percent_jail_black = 100*(total_jail_black / total_jail_all),
         percent_jail_latinx = 100*(total_jail_latinx / total_jail_all),
         percent_jail_native = 100*(total_jail_native / total_jail_all),
         percent_jail_white = 100*(total_jail_white / total_jail_all)) 
  
# Filter the dataframe to where there is sufficient data  
race_over_time <- filter(race_over_time, year >= 1990)
# Split the dataframe to make four relevant visualizations
total_percent_over_time <- select(race_over_time, year, percent_aapi, percent_black, percent_latinx, percent_native, percent_white)
jail_percent_over_time <- select(race_over_time, year, percent_jail_aapi, percent_jail_black, percent_jail_latinx, percent_jail_native, percent_jail_white)
total_jail_over_time <- select(race_over_time, year, total_jail_aapi, total_jail_black, total_jail_latinx, total_jail_native, total_jail_white, total_jail_all)
total_over_time <- select(race_over_time, year, total_aapi, total_black, total_latinx, total_native, total_white)
# Reshape the dataframe to be able to make a line graph with multiple lines
total_percent_over_time <- gather(total_percent_over_time, key = "Race", value = "Percentage", -year)
jail_percent_over_time <- gather(jail_percent_over_time, key = "Race", value = "Percentage", -year)
total_jail_over_time <- gather(total_jail_over_time, key = "Race", value = "Total", -year)
total_over_time <- gather(total_over_time, key = "Race", value = "Total", -year)
# Create line graphs that show the ways that percentages of races have changed over time
total_percent_time_plot <- ggplot(data = total_percent_over_time) + 
  geom_line(aes(x = year, y = Percentage, col = Race)) +
  theme(aspect.ratio = 1)
jail_percent_time_plot <- ggplot(data = jail_percent_over_time) + 
  geom_line(aes(x = year, y = Percentage, col = Race)) +
  theme(aspect.ratio = 1)
total_jail_time_plot <- ggplot(data = total_jail_over_time) + 
  geom_line(aes(x=year, y= Total, col = Race)) + 
  theme(aspect.ratio = 1)
total_time_plot <- ggplot(data = total_over_time) + 
  geom_line(aes(x = year, y = Total, col = Race)) + 
  theme(aspect.ratio = 1)




# Variable Comparison Chart 

# Create a data frame that contains information about the total jail population,
# the urbanicity of the county, the black total population, and the white total
# population.
var_comp_df <- df_2018 %>%
  select(black_pop_15to64, white_pop_15to64, urbanicity, total_jail_pop) %>%
  filter(urbanicity != "")
var_comp_df[is.na(var_comp_df)] <- 0
total_jail_to_black_pop <- ggplot(data = var_comp_df, aes(x = black_pop_15to64, y = total_jail_pop)) + 
  geom_point(aes(col = urbanicity)) + 
  geom_smooth(method = lm) + 
  theme(aspect.ratio = 1) +
  xlim(0, 2000000)
total_jail_to_white_pop <- ggplot(data = var_comp_df, aes(x = white_pop_15to64, y = total_jail_pop)) + 
  geom_point(aes(col = urbanicity)) + 
  geom_smooth(method = lm) + 
  theme(aspect.ratio = 1) + 
  xlim(0, 2000000)






