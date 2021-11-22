library(tidyverse)
library(ggplot2)
library(ggpubr)
library(maps)
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# SUMMARY STATISTICS

# Create a data frame with only the data from 2018
df_2018 <- filter(data, year == 2018)

# How many people were in jail on a single day in 2018?
total_jail_2018 <- sum(df_2018$total_jail_pop, na.rm = TRUE)

# How many people were in jail on a single day in 2008?
total_jail_1998 <- data %>%
  filter(year == 1998) %>%
  select(total_jail_pop) %>%
  sum(na.rm = TRUE)

# What is the proportion of the total population that is black?
black_prop_total <- 100 *
  sum(df_2018$black_pop_15to64, na.rm = TRUE) /
  sum(df_2018$total_pop_15to64, na.rm = TRUE)

# What is the proportion of the jailed population that is black?
black_prop_jail <- 100 * sum(df_2018$black_jail_pop, na.rm = TRUE) /
  total_jail_2018

# How many times greater is the proportion of the black jailed population to the
# proportion of the black general population?
black_prop_comp <- black_prop_jail / black_prop_total

# What is the proportion of the total population that is white?
white_prop_total <- 100 * sum(df_2018$white_pop_15to64, na.rm = TRUE) /
  sum(df_2018$total_pop_15to64, na.rm = TRUE)

# What is the proportion of the jailed population that is white?
white_prop_jail <- 100 * sum(df_2018$white_jail_pop, na.rm = TRUE) /
  total_jail_2018

# How many times greater is the proportion of the black jailed population to the
# proportion of the black general population?
white_prop_comp <- white_prop_jail / white_prop_total



# Trends over Time Charts
# Create a dataframe that only contains information about different races
# and their full and jail population totals over time.
race_over_time <- data %>%
  select(year, total_pop_15to64, aapi_pop_15to64, black_pop_15to64,
         latinx_pop_15to64, native_pop_15to64, white_pop_15to64, total_jail_pop,
         aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop,
         white_jail_pop) %>% # selecting relevant columns
  group_by(year) %>% # finding the total of all the counties in each year
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

# Edit dataframe to add data about the percentages of the population
race_over_time <- race_over_time %>%
  mutate(percent_aapi = 100 * (total_aapi / total_all),
         percent_black = 100 * (total_black / total_all),
         percent_latinx = 100 * (total_latinx / total_all),
         percent_native = 100 * (total_native / total_all),
         percent_white = 100 * (total_white / total_all),
         percent_jail_aapi = 100 * (total_jail_aapi / total_jail_all),
         percent_jail_black = 100 * (total_jail_black / total_jail_all),
         percent_jail_latinx = 100 * (total_jail_latinx / total_jail_all),
         percent_jail_native = 100 * (total_jail_native / total_jail_all),
         percent_jail_white = 100 * (total_jail_white / total_jail_all))
  
# Filter the dataframe to where there is sufficient data (from a glance at the
# data set, it is clear that there is much more data from 1990 until 2018)
race_over_time <- filter(race_over_time, year >= 1990)

# Split the dataframe to make four relevant visualizations
# Data frame showing only the percents of the general population
total_percent_over_time <- select(race_over_time, year, percent_aapi,
                                  percent_black, percent_latinx, percent_native,
                                  percent_white)
# Data frame showing only the percents of the jailed population
jail_percent_over_time <- select(race_over_time, year, percent_jail_aapi,
                                 percent_jail_black, percent_jail_latinx,
                                 percent_jail_native, percent_jail_white)
# Data frame showing only the totals of the jailed population
total_jail_over_time <- select(race_over_time, year, total_jail_aapi,
                               total_jail_black, total_jail_latinx,
                               total_jail_native, total_jail_white)
# Data frame showing only the totals of the general population
total_over_time <- select(race_over_time, year, total_aapi, total_black,
                          total_latinx, total_native, total_white)

# Reshape the dataframe to be able to make a line graph with multiple lines
total_percent_over_time <- gather(total_percent_over_time, key = "Race",
                                  value = "Percentage", -year)
jail_percent_over_time <- gather(jail_percent_over_time, key = "Race",
                                 value = "Percentage", -year)
total_jail_over_time <- gather(total_jail_over_time, key = "Race",
                               value = "Total", -year)
total_over_time <- gather(total_over_time, key = "Race", value = "Total", -year)

# Create line graphs that show the ways that percentages of races have changed
# over time.  Each line graph is set up in a similar way.  The
# "aspect.ratio = 1" term is to ensure that all of the charts are square.
total_percent_time_plot <- ggplot(data = total_percent_over_time) +
  geom_line(aes(x = year, y = Percentage, col = Race)) +
  theme(aspect.ratio = 1) +
  labs(title = "General Population Racial \nPercentages Over Time",
       x = "Time",
       y = "Percentage of General Population") +
  scale_color_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))
jail_percent_time_plot <- ggplot(data = jail_percent_over_time) +
  geom_line(aes(x = year, y = Percentage, col = Race)) +
  theme(aspect.ratio = 1) +
  labs(title = "Jailed Population Racial \nPercentages Over Time",
       x = "Time",
       y = "Percentage of Jailed Population"
  ) +
  scale_color_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))
total_jail_time_plot <- ggplot(data = total_jail_over_time) +
  geom_line(aes(x = year, y = Total, col = Race)) +
  theme(aspect.ratio = 1) +
  labs(title = "Total Jailed Racial \nPopulations Over Time",
       x = "Time",
       y = "Total Jailed Population"
  ) +
  scale_color_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))
total_time_plot <- ggplot(data = total_over_time) +
  geom_line(aes(x = year, y = Total, col = Race)) +
  theme(aspect.ratio = 1) +
  labs(title = "Total General Racial \nPopulations Over Time",
       x = "Time",
       y = "Total General Population"
       ) +
  scale_color_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))




# Variable Comparison Chart

# Create a data frame that contains information about the total jail population,
# the urbanicity of the county, the black total population, and the white total
# population.
var_comp_df <- df_2018 %>%
  select(total_pop_15to64, black_pop_15to64, white_pop_15to64, urbanicity,
         total_jail_pop) %>%
  filter(urbanicity != "") %>% # Removes rows with undefined urbanicity
  mutate(
    percent_black_total = 100 * (black_pop_15to64 / total_pop_15to64),
    percent_white_total = 100 * (white_pop_15to64 / total_pop_15to64))

# Set any value in the dataframe that is NA to 0
# var_comp_df[is.na(var_comp_df)] <- 0

# Create two scatter plots, one that compares total jailed population of a
# county to the percent black population of a county, and one that compares the
# total jailed population of a county to the percent white population of a
# county.  The points will be color-coded by the urbanicity of the county.  A
# linear trend line will be drawn over the plots.  The "alpha = 0.3" term
# decreases the opacity so that the points that are overlapping each other can
# be more easily distinguished.
total_jail_to_black_percent <- ggplot(data = var_comp_df,
                                      aes(x = percent_black_total,
                                          y = total_jail_pop)) +
  geom_point(aes(col = urbanicity, alpha = 0.3)) +
  geom_smooth(method = lm) +
  labs(
    title = "Total Jail Population to Overall\n County Black Population, 2018",
    x = "Black Population (Percent of General Pop.)",
    y = "Total Jailed Population",
    color = "Urbanicity"
  )

total_jail_to_white_percent <- ggplot(data = var_comp_df,
                                      aes(x = percent_white_total,
                                          y = total_jail_pop)) +
  geom_point(aes(col = urbanicity, alpha = 0.3)) +
  geom_smooth(method = lm)  +
  labs(
    title = "Total Jail Population to Overall\nCounty White Population, 2018",
    x = "White Population (Percent of General Pop.)",
    y = "Total Jailed Population",
    color = "Urbanicity"
  )

# Map Section

# Load maps data frame
map_df <- map_data("county")
# Add polyname column so that it can be joined with a data frame of county fips
# codes.
map_df <- map_df %>%
  unite(polyname, region, subregion, sep = ",", remove = FALSE)
# Load county fips codes into a dataframe
county_fips <- county.fips
# Merge county_fips and map_df so lat/long and fips are in the same data frame
fips_lat_long <- left_join(map_df, county_fips, by = "polyname")
# Merge fips_lat_long and incarceration data from 2018 (df_2018)
map_incarceration_2018 <- left_join(df_2018, fips_lat_long, by = "fips")

# Make a blank theme (using the code for a blank theme from the textbook)
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

# Make map showing the distribution of the total black population
black_pop_map <- ggplot() +
  geom_polygon(
    data = map_incarceration_2018,
    mapping = aes(x = long, y = lat, group = group, fill = black_pop_15to64),
    color = "black",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "white", high = "blue", trans = "log") +
  labs(fill = "Black Population 15-64") +
  blank_theme +
  labs(title = "Distribution of Black Populations Across the U.S.",
       subtitle = "Ages 15-64; grouped by County; color is coded on a logarithmic scale")

# Make map showing the distribution of the total jail population
jail_pop_map <- ggplot() +
  geom_polygon(
    data = map_incarceration_2018,
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "black",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "white", high = "blue", trans = "log") +
  labs(fill = "Total Jail Population") +
  blank_theme +
  labs(title = "Distribution of Jailed Populations Across the U.S.",
       subtitle = "Grouped by County; color is coded on a logarithmic scale")
  
  
# Extra:
# Pie Charts to Add to Intro
# Make a data frame that contains national racial jail information in 2018
pie_jail_df <- total_jail_over_time %>%
  filter(year == 2018) %>%
  filter(Race != "total_jail_all") # filter out the total rows
# Make a data frame that contains national racial information about the general
# population in the year 2018.
pie_total_df <- filter(total_over_time, year == 2018)
# Make a pie chart showing the racial demographics of the 2018 jailed population
pie_jail <- ggplot(data = pie_jail_df,
                   mapping = aes(x = "", y = Total, fill = Race)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Jailed Population \nRacial Composition, 2018") +
  scale_fill_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))
# Make a pie chart of the racial demographics of the 2018 general population.
pie_total <- ggplot(data = pie_total_df,
                    mapping = aes(x = "", y = Total, fill = Race)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "General Population \nRacial Composition, 2018") +
  scale_fill_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))
