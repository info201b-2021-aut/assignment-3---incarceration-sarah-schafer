library(tidyverse)
library(ggplot2)
data <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

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
total_percent_time_plot <- ggplot(data = total_percent_over_time) + geom_line(aes(x = year, y = Percentage, col = Race))
jail_percent_time_plot <- ggplot(data = jail_percent_over_time) + geom_line(aes(x = year, y = Percentage, col = Race))
total_jail_time_plot <- ggplot(data = total_jail_over_time) + geom_line(aes(x=year, y= Total, col = Race))
total_time_plot <- ggplot(data = total_over_time) + geom_line(aes(x = year, y = Total, col = Race))




# Variable Comparison Chart 

# Create a data frame with only the data from 2018
df_2018 <- filter(data, year == 2018)
# Create a data frame that contains information about the total jail population,
# the urbanicity of the county, the black total popualtion, and the white total
# population.
var_comp_df <- select(df_2018, black_pop_15to64, white_pop_15to64, urbanicity, total_jail_pop)
total_jail_to_black_pop <- ggplot(data = var_comp_df) + geom_point(aes(x = total_jail_pop, y = black_pop_15to64, col = urbanicity))
total_jail_to_white_pop <- ggplot(data = var_comp_df) + geom_point(aes(x = total_jail_pop, y = white_pop_15to64, col = urbanicity))





# Old work:
# ggplot(data = race_over_time) + 
  # geom_line(aes(x=year, y=percent_jail_aapi)) +
  # geom_line(aes(x=year, y=percent_jail_black)) +
  # geom_line(aes(x=year, y=percent_jail_latinx)) +
  # geom_line(aes(x=year, y=percent_jail_native)) +
  # geom_line(aes(x=year, y=percent_jail_white))

# washington <- filter(data, state == "WA")
# king_county <- filter(washington, county_name == "King County")
# ggplot(data = king_county) + 
  # geom_point(aes(x = year, y = total_jail_pop))

# What is the proportion of the total population that is black?
# black_prop_total <- sum(data$black_pop_15to64, na.rm = TRUE) / sum(data$total_pop_15to64, na.rm = TRUE)
# What is the proportion of the jailed population that is black?
# black_prop_jail <- sum(data$black_jail_pop, na.rm = TRUE) / sum(data$total_jail_pop, na.rm = TRUE)
# How many times greater is the proportion of the black jailed population to the
# proportion of the black general population?
# black_prop_comp <- black_prop_jail/black_prop_total

# trial1 <- mutate(data, black_prop_total_row = black_pop_15to64 / total_pop_15to64)
# ggplot(trial1) + geom_col(aes(x=urbanicity, y = black_prop_total_row))
# sum_trial1 <- trial1 %>%
  # mutate(black_prison_prop = black_prison_pop / total_prison_pop) %>%
  # group_by(urbanicity) %>%
  # summarize(avg_black_pris_prop = mean(black_prison_prop, na.rm = TRUE))
# ggplot(sum_trial1) + geom_point(aes(x=urbanicity, y=avg_black_pris_prop))


# ggplot(trial1) + geom_col(aes(x=urbanicity, y = total_prison_pop))
