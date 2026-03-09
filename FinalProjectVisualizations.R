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
library(plotly)
library(htmlwidgets)

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
