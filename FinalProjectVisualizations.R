# ------------------------------------------------------------------------------
# Final Project Visualizations
# 3-9-2026
# ------------------------------------------------------------------------------

### Set-up #####################################################################

library(tidyverse)
library(ggplot2)
library(readr)
library(janitor)
library(lubridate)
library(ggforce)
library(sf)
library(maps)
library(plotly)
library(htmlwidgets)
library(forcats)

setwd("/Users/mirachandriani/Desktop/*WellesleyClasses/DS340H/FinalProject")

# Reading in data
data <- read.csv("atus_all.csv") %>%
  clean_names()

### Minor Data Cleaning ########################################################

# Creating square root variables for time
data$sqrt_child_care_duration <- sqrt(data$child_care_duration)
data$sqrt_household_task_duration <- sqrt(data$household_task_duration)

# Converting NaN to NA
data <- data %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

# Converting date variable to date class
data$tudiarydate <- as.Date(data$tudiarydate, format = "%Y-%m-%d")

# Converting categorical variables to factor class
data$income_level <- as.factor(data$income_level)
data$tesex <- as.factor(data$tesex)
data$tudiaryday <- as.factor(data$tudiaryday)
data$employed <- as.factor(data$employed)
data$gestfips <- as.factor(data$gestfips)
data$gtmetsta <- as.factor(data$gtmetsta)
data$gtmetsta <- as.factor(data$gtmetsta)
data$race <- as.factor(data$race)

# Naming days of week and gender
data <- data %>%
  mutate(
    tudiaryday = factor(tudiaryday, levels = 1:7, labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
    tesex = ifelse(tesex == 1, "Male", "Female")
  )

### Visual 1 ###################################################################

# Sample 2000 points per sex
set.seed(1)
data_sampled <- data %>%
  group_by(tesex) %>%
  slice_sample(n = 2000) %>%
  ungroup()

# Fit loess splines on full data for each gender
fit_spline <- function(gender_data) {
  fit <- loess(sqrt_child_care_duration ~ as.numeric(tudiarydate), data = gender_data, span = 0.3)
  dates <- seq(min(gender_data$tudiarydate), max(gender_data$tudiarydate), length.out = 300)
  preds <- predict(fit, newdata = data.frame(tudiarydate = dates))
  data.frame(tudiarydate = dates, predicted = preds)
}
spline_male <- fit_spline(data[data$tesex == "Male", ])
spline_female <- fit_spline(data[data$tesex == "Female", ])

# Converting types
data_sampled$tudiarydate <- as.Date(data_sampled$tudiarydate)
spline_male$tudiarydate <- as.Date(spline_male$tudiarydate)
spline_female$tudiarydate <- as.Date(spline_female$tudiarydate)

# Plot
plot_1 <- plot_ly(
  data_sampled,
  x = ~tudiarydate,
  y = ~sqrt_child_care_duration,
  type = "scatter",
  mode = "markers",
  color = ~factor(tesex),
  colors = c("#F8766D", "#00BFC4"),
  marker = list(size = 6, opacity = 0.8),
  text = ~paste(
    "Diary Date:", tudiarydate,
    "<br>Diary Day:", as.character(tudiaryday),
    "<br>Sqrt Child Care Duration:", round(sqrt_child_care_duration, 2),
    "<br>Child Care Duration:", round(child_care_duration, 2),
    "<br>Gender:", tesex
  ),
  hoverinfo = "text"
) %>%
  add_lines(data = spline_male, x = ~tudiarydate, y = ~predicted,
            line = list(color = "#00BFC4", width = 2),
            inherit = FALSE, showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(data = spline_female, x = ~tudiarydate, y = ~predicted,
            line = list(color = "#F8766D", width = 2),
            inherit = FALSE, showlegend = FALSE, hoverinfo = "none") %>%
  layout(
    title = "Daily Child Care Over Time for Mothers and Fathers",
    xaxis = list(title = "Diary Date", type = "date"),
    yaxis = list(title = "Sqrt Child Care Minutes Per Day"),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
  )
plot_1

# Saving plot
saveWidget(plot_1, "plot1.html")

### Visual 2 ###################################################################

# Sample 2000 points per sex
set.seed(1)
data_sampled_2 <- data %>%
  group_by(tesex) %>%
  slice_sample(n = 2000) %>%
  ungroup()

# Fit loess splines on full data for each gender
fit_spline_hh <- function(gender_data) {
  fit <- loess(sqrt_household_task_duration ~ as.numeric(tudiarydate), data = gender_data, span = 0.3)
  dates <- seq(min(gender_data$tudiarydate), max(gender_data$tudiarydate), length.out = 300)
  preds <- predict(fit, newdata = data.frame(tudiarydate = dates))
  data.frame(tudiarydate = dates, predicted = preds)
}

spline_male_hh   <- fit_spline_hh(data[data$tesex == "Male", ])
spline_female_hh <- fit_spline_hh(data[data$tesex == "Female", ])

# Converting types
data_sampled_2$tudiarydate   <- as.Date(data_sampled_2$tudiarydate)
spline_male_hh$tudiarydate   <- as.Date(spline_male_hh$tudiarydate)
spline_female_hh$tudiarydate <- as.Date(spline_female_hh$tudiarydate)

# Plot
plot_2 <- plot_ly(
  data_sampled_2,
  x = ~tudiarydate,
  y = ~sqrt_household_task_duration,
  type = "scatter",
  mode = "markers",
  color = ~factor(tesex),
  colors = c("#F8766D", "#00BFC4"),
  marker = list(size = 6, opacity = 0.8),
  text = ~paste(
    "Diary Date:", tudiarydate,
    "<br>Diary Day:", as.character(tudiaryday),
    "<br>Sqrt Household Task Duration:", round(sqrt_household_task_duration, 2),
    "<br>Household Task Duration:", round(household_task_duration, 2),
    "<br>Gender:", tesex
  ),
  hoverinfo = "text"
) %>%
  add_lines(data = spline_male_hh, x = ~tudiarydate, y = ~predicted,
            line = list(color = "#00BFC4", width = 2),
            inherit = FALSE, showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(data = spline_female_hh, x = ~tudiarydate, y = ~predicted,
            line = list(color = "#F8766D", width = 2),
            inherit = FALSE, showlegend = FALSE, hoverinfo = "none") %>%
  layout(
    title = "Daily Household Tasks Over Time for Mothers and Fathers",
    xaxis = list(title = "Diary Date", type = "date"),
    yaxis = list(title = "Sqrt Household Tasks Minutes Per Day"),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
  )
plot_2

# Saving plot
saveWidget(plot_2, "plot2.html")

### Visuals 3 and 4 ############################################################

# Aggregating by month and gender
gender_race_monthly <- data %>%
  mutate(
    race = fct_collapse(race, "Multiracial/Other" = c("Multiracial", "Other")), # combining multiracial and other
    month = floor_date(tudiarydate, "month")
  ) %>%
  group_by(month, tesex, race) %>%
  summarise(mean_childcare = mean(child_care_duration, na.rm = TRUE),
            mean_household_task = mean(household_task_duration, na.rm = TRUE),
            n = n(),
            .groups = "drop")
gender_employed_monthly <- data %>%
  mutate(month = floor_date(tudiarydate, "month")) %>%
  group_by(month, tesex, employed) %>%
  summarise(mean_childcare = mean(child_care_duration, na.rm = TRUE),
            mean_household_task = mean(household_task_duration, na.rm = TRUE),
            .groups = "drop")
gender_income_monthly <- data %>%
  mutate(month = floor_date(tudiarydate, "month"),
         income_group = ifelse(income_level %in% c(1, 2, 3), "Income <$100k", "Income >$100k")) %>%
  group_by(month, tesex, income_group) %>%
  summarise(mean_childcare = mean(child_care_duration, na.rm = TRUE),
            mean_household_task = mean(household_task_duration, na.rm = TRUE),
            .groups = "drop")

# Pivoting wide and calculating child care difference within each race
childcare_race_diff <- gender_race_monthly %>%
  select(month, tesex, race, mean_childcare) %>%
  pivot_wider(names_from = tesex, values_from = mean_childcare) %>%
  mutate(difference = Female - Male) %>%
  filter(!is.na(difference)) %>% # dropping month-race combos with only one gender
  filter(race != "Multiracial/Other")
childcare_employed_diff <- gender_employed_monthly %>%
  select(month, tesex, employed, mean_childcare) %>%
  pivot_wider(names_from = tesex, values_from = mean_childcare) %>%
  mutate(difference = Female - Male,
         group = ifelse(employed == 1, "Employed", "Unemployed")) %>%
  filter(!is.na(difference))
childcare_income_diff <- gender_income_monthly %>%
  select(month, tesex, income_group, mean_childcare) %>%
  pivot_wider(names_from = tesex, values_from = mean_childcare) %>%
  mutate(difference = Female - Male) %>%
  filter(!is.na(difference))

# Pivoting wide and calculating household task difference within each race
household_task_race_diff <- gender_race_monthly %>%
  select(month, tesex, race, mean_household_task) %>%
  pivot_wider(names_from = tesex, values_from = mean_household_task) %>%
  mutate(difference = Female - Male) %>%
  filter(!is.na(difference)) %>% # dropping month-race combos with only one gender
  filter(race != "Multiracial/Other")
household_employed_diff <- gender_employed_monthly %>%
  select(month, tesex, employed, mean_household_task) %>%
  pivot_wider(names_from = tesex, values_from = mean_household_task) %>%
  mutate(difference = Female - Male,
         group = ifelse(employed == 1, "Employed", "Unemployed")) %>%
  filter(!is.na(difference))
household_income_diff <- gender_income_monthly %>%
  select(month, tesex, income_group, mean_household_task) %>%
  pivot_wider(names_from = tesex, values_from = mean_household_task) %>%
  mutate(difference = Female - Male) %>%
  filter(!is.na(difference))

# Colors
race_colors <- c("White" = "#F8766D", "Black" = "#C77CFF", "Asian" = "#0072B2", "Hispanic" = "#009E73")
employed_colors <- c("Employed" = "#E69F00", "Unemployed" = "#00BFC4")
income_colors   <- c("Income <$100k" = "#CC79A7", "Income >$100k" = "#FF7F00")
all_colors <- c(race_colors, employed_colors, income_colors)

# Plot (child care)
plot_3 <- ggplot(childcare_race_diff, aes(x = month, y = difference, color = race)) +
  geom_line(alpha = 0.4) +
  geom_line(data = childcare_employed_diff, aes(x = month, y = difference, color = group), alpha = 0.4) +
  geom_line(data = childcare_income_diff, aes(x = month, y = difference, color = income_group), alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_smooth(data = childcare_employed_diff, aes(x = month, y = difference, color = group), se = FALSE, method = "loess", span = 0.3) +
  geom_smooth(data = childcare_income_diff, aes(x = month, y = difference, color = income_group), se = FALSE, method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = all_colors) +
  scale_x_date(breaks = seq(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "year"), date_labels = "%Y") +
  labs(title = "Gender Gap in Child Care Time by Race Over Time",
       subtitle = "Positive Values = Mothers Spend More Time",
       x = "Month",
       y = "Gender Difference in Daily Child Care Minutes\n(Mother - Father)",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

# Plot (household tasks)
plot_4 <- ggplot(household_task_race_diff, aes(x = month, y = difference, color = race)) +
  geom_line(alpha = 0.4) +
  geom_line(data = household_employed_diff, aes(x = month, y = difference, color = group), alpha = 0.4) +
  geom_line(data = household_income_diff, aes(x = month, y = difference, color = income_group), alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_smooth(data = household_employed_diff, aes(x = month, y = difference, color = group), se = FALSE, method = "loess", span = 0.3) +
  geom_smooth(data = household_income_diff, aes(x = month, y = difference, color = income_group), se = FALSE, method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = all_colors) +
  scale_x_date(breaks = seq(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "year"), date_labels = "%Y") +
  labs(title = "Gender Gap in Time Spent on Household Tasks by Race Over Time",
       subtitle = "Positive Values = Mothers Spend More Time",
       x = "Month",
       y = "Gender Difference in Minutes of Daily Household Tasks\n(Mother - Father)",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

### Visual 5 ###################################################################

# Prepare heatmap data
heatmap_data <- data %>%
  mutate(year = year(tudiarydate)) %>%
  group_by(year, tesex) %>%
  summarise(
    mean_childcare = mean(child_care_duration, na.rm = TRUE),
    mean_household = mean(household_task_duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mean_childcare, mean_household),
               names_to = "task", values_to = "mean_duration") %>%
  mutate(category = case_when(
    task == "mean_childcare" & tesex == "Female" ~ "Child Care - Women",
    task == "mean_childcare" & tesex == "Male"   ~ "Child Care - Men",
    task == "mean_household" & tesex == "Female" ~ "Household Tasks - Women",
    task == "mean_household" & tesex == "Male"   ~ "Household Tasks - Men"
  ))

# Plot
plot_5 <- ggplot(heatmap_data, aes(x = factor(year), y = category, fill = mean_duration)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_duration, 1)), size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Average Daily Time Spent on Tasks by Gender (2010-2019)",
       x = "Year",
       y = NULL,
       fill = "Minutes") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11))

### Visuals 6 and 7 ############################################################

# Aggregating by month, income, and gender
gender_income_monthly <- data %>%
  mutate(
    month = floor_date(tudiarydate, "month")
  ) %>%
  group_by(month, tesex, income_level) %>%
  summarise(mean_childcare = mean(child_care_duration, na.rm = TRUE),
            mean_household_task = mean(household_task_duration, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# Pivoting wide and calculating child care difference within each income level
childcare_income_diff <- gender_income_monthly %>%
  select(month, tesex, income_level, mean_childcare) %>%
  pivot_wider(names_from = tesex, values_from = mean_childcare) %>%
  mutate(difference = Female - Male) %>%
  filter(!is.na(difference), !is.na(income_level)) # dropping month-income combos with only one gender

# Pivoting wide and calculating household task difference within each income level
household_task_income_diff <- gender_income_monthly %>%
  select(month, tesex, income_level, mean_household_task) %>%
  pivot_wider(names_from = tesex, values_from = mean_household_task) %>%
  mutate(difference = Female - Male) %>%
  filter(!is.na(difference), !is.na(income_level)) # dropping month-income combos with only one gender

# Plot (child care)
plot_6 <- ggplot(childcare_income_diff, aes(x = month, y = difference, color = income_level)) +
  geom_line(alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(breaks = seq(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "year"), date_labels = "%Y") +
  labs(title = "Gender Gap in Child Care Time by Income Level Over Time",
       subtitle = "Positive Values = Mothers Spend More Time",
       x = "Month",
       y = "Gender Difference in Daily Child Care Minutes\n(Mother - Father)",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

# Plot (household tasks)
plot_7 <- ggplot(household_task_income_diff, aes(x = month, y = difference, color = income_level)) +
  geom_line(alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(breaks = seq(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "year"), date_labels = "%Y") +
  labs(title = "Gender Gap in Time Spent on Household Tasks by Income Level Over Time",
       subtitle = "Positive Values = Mothers Spend More Time",
       x = "Month",
       y = "Gender Difference in Minutes of Daily Household Tasks\n(Mother - Father)",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

### Visuals 8 and 9 ############################################################

# Editing data for violins
violin_data <- data %>%
  mutate(
    year_group = case_when(
      year %in% c(2010, 2011) ~ "2010-2011",
      year %in% c(2012, 2013) ~ "2012-2013",
      year %in% c(2014, 2015) ~ "2014-2015",
      year %in% c(2016, 2017) ~ "2016-2017",
      year %in% c(2018, 2019) ~ "2018-2019"
    )
  )

# Function to create violin plots
make_violin_plot <- function(data, y_var, title, y_label) {
  set.seed(1)
  year_groups <- c("2010-2011", "2012-2013", "2014-2015", "2016-2017", "2018-2019")
  genders <- c("Female", "Male")
  colors <- c("Female" = "#F8766D", "Male" = "#00BFC4")
  
  p <- plot_ly()
  
  for (g in genders) {
    for (yg in year_groups) {
      full_df <- data %>% filter(tesex == g, year_group == yg)
      
      p <- p %>% add_trace(
        type = "violin",
        x = rep(yg, nrow(full_df)),
        y = full_df[[y_var]],
        name = g,
        legendgroup = g,
        showlegend = (yg == "2010-2011"),
        side = if (g == "Female") "negative" else "positive",
        spanmode = "hard",
        scalemode = "width",
        scalegroup = "all",
        width = 1,
        fillcolor = adjustcolor(colors[g], alpha.f = 0.7),
        line = list(color = colors[g]),
        points = FALSE,
        box = list(visible = FALSE),
        meanline = list(visible = TRUE, color = "black", width = 2)
      )
    }
  }
  
  p <- p %>% layout(
    title = list(text = title, x = 0.5),
    yaxis = list(title = y_label),
    xaxis = list(title = "Years", categoryorder = "array",
                 categoryarray = year_groups),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
  )
  
  return(p)
}

# Plot (child care)
plot_8 <- make_violin_plot(violin_data, "child_care_duration",
                 "Distribution of Daily Child Care Duration by Year and Gender",
                 "Daily Child Care Duration (Minutes)")

# Plot (household tasks)
plot_9 <- make_violin_plot(violin_data, "household_task_duration",
                 "Distribution of Daily Household Task Duration by Year and Gender",
                 "Daily Household Task Duration (Minutes)")

# Saving plots
saveWidget(plot_8, "plot8.html")
saveWidget(plot_9, "plot9.html")

### Visual 10 ##################################################################

# Configuring atus dataset
childcare_state_year <- data %>%
  group_by(year, gestfips, tesex) %>%
  summarise(
    mean_childcare = mean(child_care_duration, na.rm = TRUE),
    n_obs = n(),  # Count observations for each gender
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = tesex, 
    values_from = c(mean_childcare, n_obs),
    names_sep = "_"
  ) %>%
  mutate(
    difference = mean_childcare_Female - mean_childcare_Male,
    gestfips = as.numeric(as.character(gestfips))
  ) %>%
  filter(!is.na(difference))

# Mapping fips codes to state abbreviations
state_lookup <- data.frame(
  gestfips = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

# Merging with state abbreviations  
childcare_final <- childcare_state_year %>%
  left_join(state_lookup, by = "gestfips") %>%
  filter(!is.na(state_abb))

# Filtering to 2010-2019
childcare_final <- childcare_final %>%
  filter(year >= 2010 & year <= 2019)

# Function to create interactive map
create_interactive_childcare_map <- function() {
  years <- sort(unique(childcare_final$year))
  
  # Calculate global min and max for consistent color scale
  global_min <- min(childcare_final$difference, na.rm = TRUE)
  global_max <- max(childcare_final$difference, na.rm = TRUE)
  
  # Make the scale symmetric around 0 so white is exactly at 0
  scale_limit <- max(abs(global_min), abs(global_max))
  
  # Create custom diverging color scale - fixed order
  custom_colors <- list(
    c(0, "#00BFC4"),      # Most negative values (males do more)
    c(0.5, "white"),   # Exactly zero 
    c(1, "#F8766D")    # Most positive values (females do more)
  )
  
  # Initialize the plot
  fig <- plot_geo(locationmode = 'USA-states', width = 1200, height = 700)
  
  # Add traces for each year
  for(i in 1:length(years)) {
    year_data <- childcare_final %>% filter(year == years[i])
    
    fig <- fig %>%
      add_trace(
        data = year_data,
        z = ~difference,
        zmin = -scale_limit,  # Force symmetric scale around 0
        zmax = scale_limit,   # Force symmetric scale around 0
        locations = ~state_abb,
        colorscale = custom_colors,
        type = "choropleth",
        locationmode = 'USA-states',
        text = ~paste(
          "State:", state_name, 
          "<br>Year:", year, 
          "<br>Difference:", round(difference, 2), "minutes",
          "<br>Female respondents:", n_obs_Female,
          "<br>Male respondents:", n_obs_Male
        ),
        hovertemplate = "%{text}<extra></extra>",
        visible = ifelse(i == 1, TRUE, FALSE),
        showlegend = FALSE,
        showscale = TRUE,
        marker = list(line = list(color = "#333333", width = 0.5)),
        colorbar = list(
          title = "Gender Gap<br>in Minutes<br>(Female - Male)",
          len = 0.7,
          x = 1.02,
          y = 1,
          thickness = 15
        )
      )
  }
  
  # Create button list for years - SIMPLE FIX: Just handle the year traces
  button_list <- lapply(1:length(years), function(i) {
    visible_array <- rep(FALSE, length(years))  # Only for year traces
    visible_array[i] <- TRUE  # Show selected year
    
    list(
      label = as.character(years[i]),
      method = "update",
      args = list(
        list(visible = visible_array),
        list(title = paste("Gender Gap in Child Care Time by State in", years[i]))
      )
    )
  })
  
  # Add button menu for year selection
  fig <- fig %>%
    layout(
      title = list(
        text = paste("Gender Gap in Child Care Time by State in 2010"),
        y = 0.9
      ),
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      ),
      updatemenus = list(
        list(
          type = "buttons",
          direction = "right",
          xanchor = 'center',
          x = 0.57,
          y = 1.04,
          buttons = button_list
        )
      ),
      margin = list(t = 120, r = 80)
    )
  
  return(fig)
}

# Creating, displaying, and saving interactive map
plot_10 <- create_interactive_childcare_map()
plot_10
saveWidget(plot_10, file = "plot10.html", selfcontained = TRUE)

### Visual 11 ##################################################################

# All years
plot_11 <- ggplot(data, aes(x = sqrt_child_care_duration, 
                            y = sqrt_household_task_duration, 
                            color = tesex)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), 
             alpha = 0.5) +
  scale_color_manual(values = c("Female" = "#F8766D", "Male" = "#00BFC4")) +
  labs(x = "Sqrt Child Care Duration",
       y = "Sqrt Household Task Duration",
       title = "Sqrt Child Care Duration vs. Sqrt Household Tasks Duration by Gender",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

# Individual years
for (yr in 2010:2019) {
  data_yr <- data[data$year == yr, ]
  
  p <- ggplot(data_yr, aes(x = sqrt_child_care_duration, 
                           y = sqrt_household_task_duration, 
                           color = tesex)) +
    geom_point(position = position_jitter(width = 0.1, height = 0.1), 
               alpha = 0.5) +
    scale_color_manual(values = c("Female" = "#F8766D", "Male" = "#00BFC4")) +
    labs(x = "Sqrt Child Care Duration",
         y = "Sqrt Household Task Duration",
         title = paste("Sqrt Child Care Duration vs. Sqrt Household Tasks Duration by Gender,", yr),
         color = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5), 
          legend.position = "bottom")
  
  assign(paste0("plot_11_", yr), p)
}





plot_11
plot_11_2010
plot_11_2011
plot_11_2012
plot_11_2013
plot_11_2014
plot_11_2015
plot_11_2016
plot_11_2017
plot_11_2018
plot_11_2019

