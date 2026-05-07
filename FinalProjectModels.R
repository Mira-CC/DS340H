# ------------------------------------------------------------------------------
# Final Project Models
# 4-1-2026
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
library(caret)
library(openxlsx)
library(broom)

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

# Creating a numeric date variable
data$tudiarydate_numeric <- as.numeric(data$tudiarydate)

# Converting categorical variables to factor class
# data$income_level <- as.factor(data$income_level)
data$tesex <- as.factor(data$tesex)
data$tudiaryday <- as.factor(data$tudiaryday)
data$employed <- as.factor(data$employed)
data$gestfips <- as.factor(data$gestfips)
data$race <- as.factor(data$race)

# Naming days of week and gender
data <- data %>%
  mutate(
    tudiaryday = factor(tudiaryday, levels = 1:7, labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
    tesex = ifelse(tesex == 1, "Male", "Female")
  )

# Re-leveling race
data$race <- relevel(as.factor(data$race), ref = "White")

# Adding weekend (vs. weekday) indicator
data$weekend <- ifelse(weekdays(data$tudiarydate) %in% c("Saturday", "Sunday"), 1, 0)

# Removing rows with missing income_level value
data <- data %>% filter(!is.na(income_level))

# Creating train and test datasets
set.seed(1)
train_indices <- sample(1:nrow(data), size = 0.85 * nrow(data))
train <- data[train_indices, ]
test <- data[-train_indices, ]

### Training 4 Models ##########################################################

model_1 <- lm(sqrt_child_care_duration ~ tesex + year + tesex*year + weekend + employed + income_level + gestfips + race + sqrt_household_task_duration, data = train)

model_2 <- lm(sqrt_child_care_duration ~ tesex + tudiarydate_numeric + tesex*tudiarydate_numeric + employed + income_level + gestfips + race + sqrt_household_task_duration, data = train)

model_3 <- lm(sqrt_household_task_duration ~ tesex + year + tesex*year + weekend + employed + income_level + gestfips + race + sqrt_child_care_duration, data = train)

model_4 <- lm(sqrt_household_task_duration ~ tesex + tudiarydate_numeric + tesex*tudiarydate_numeric + employed + income_level + gestfips + race + sqrt_child_care_duration, data = train)

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

### Evaluating 4 Models on Test Dataset ########################################

# Metrics for evaluation
eval_metrics <- function(model, test_data, outcome) {
  preds <- predict(model, newdata = test_data)
  actuals <- test_data[[outcome]]
  rmse <- sqrt(mean((preds - actuals)^2))
  r2 <- cor(preds, actuals)^2
  data.frame(RMSE = rmse, R2 = r2)
}

results_test <- rbind(
  Model1 = eval_metrics(model_1, test, "sqrt_child_care_duration"),
  Model2 = eval_metrics(model_2, test, "sqrt_child_care_duration"),
  Model3 = eval_metrics(model_3, test, "sqrt_household_task_duration"),
  Model4 = eval_metrics(model_4, test, "sqrt_household_task_duration")
)

print(results_test)

# Model 1 is better than Model 2 (lower RMSE, higher R-squared), so year is a better predictor of child care than date
# Model 3 is better than Model 4 (lower RMSE, higher R-squared), so year is a better predictor of household tasks than date
# Models 3 and 4 have better fit than Models 1 and 2 overall, so household tasks are more predictable than child care

### Evaluating 4 Models on Train Dataset #######################################

results_train <- rbind(
  Model1 = eval_metrics(model_1, train, "sqrt_child_care_duration"),
  Model2 = eval_metrics(model_2, train, "sqrt_child_care_duration"),
  Model3 = eval_metrics(model_3, train, "sqrt_household_task_duration"),
  Model4 = eval_metrics(model_4, train, "sqrt_household_task_duration")
)

print(results_train)

### Prediction Plot (Models 1 and 3) ###########################################

# Predictions for household tasks
predictions_householdtasks_data <- expand.grid(
  tesex = c("Female", "Male"),
  year = 2010:2019,
  weekend = 0,
  employed = factor(1, levels = levels(train$employed)),
  income_level = 3,
  gestfips = factor(6, levels = levels(train$gestfips)),
  race = "White",
  sqrt_child_care_duration = median(train$sqrt_child_care_duration, na.rm = TRUE)
)
predictions_householdtasks_data$sqrt_pred <- predict(model_3, newdata = predictions_householdtasks_data)
predictions_householdtasks_data$pred_duration <- predictions_householdtasks_data$sqrt_pred^2
predictions_householdtasks_data$model <- "Household Tasks (Model 1)"

# Predictions for child care
predictions_childcare_data <- expand.grid(
  tesex = c("Female", "Male"),
  year = 2010:2019,
  weekend = 0,
  employed = factor(1, levels = levels(train$employed)),
  income_level = 3,
  gestfips = factor(6, levels = levels(train$gestfips)),
  race = "White",
  sqrt_household_task_duration = median(train$sqrt_household_task_duration, na.rm = TRUE)
)
predictions_childcare_data$sqrt_pred <- predict(model_1, newdata = predictions_childcare_data)
predictions_childcare_data$pred_duration <- predictions_childcare_data$sqrt_pred^2
predictions_childcare_data$model <- "Child Care (Model 3)"

# Combined predictions
combined_preds <- rbind(
  predictions_householdtasks_data[, c("tesex", "year", "pred_duration", "model")],
  predictions_childcare_data[, c("tesex", "year", "pred_duration", "model")]
)

# Plot
prediction_plot <- ggplot(combined_preds, 
                          aes(x = year, y = pred_duration, color = tesex, 
                              linetype = model, shape = model,
                              group = interaction(tesex, model))) +
  geom_point(size = 3) +
  geom_line(alpha = 0.6) +
  scale_x_continuous(breaks = 2010:2019) +
  expand_limits(y = 0) +
  scale_color_manual(values = c("Male" = "#00BFC4", "Female" = "#F8766D")) +
  scale_linetype_manual(values = c("Household Tasks (Model 1)" = "dashed", "Child Care (Model 3)" = "dotted")) +
  scale_shape_manual(values = c("Household Tasks (Model 1)" = 15, "Child Care (Model 3)" = 17)) +
  labs(
    title = "Figure 6: Predicted Task Duration by Gender",
    subtitle = "Models 1 and 3",
    x = "Year",
    y = "Predicted Duration (Minutes)",
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(-5, "pt"))

### Gender Gap Model Predictions Average #######################################

# Average Marginal Effect of gender on household tasks
ame_data_household <- train

ame_data_household$tesex <- "Female"
ame_data_household$pred_female <- predict(model_3, newdata = ame_data_household)^2

ame_data_household$tesex <- "Male"
ame_data_household$pred_male <- predict(model_3, newdata = ame_data_household)^2

householdtasks_gap <- mean(ame_data_household$pred_female - ame_data_household$pred_male, na.rm = TRUE)
householdtasks_gap

# Average Marginal Effect of gender on childcare
ame_data_childcare <- train

ame_data_childcare$tesex <- "Female"
ame_data_childcare$pred_female <- predict(model_1, newdata = ame_data_childcare)^2

ame_data_childcare$tesex <- "Male"
ame_data_childcare$pred_male <- predict(model_1, newdata = ame_data_childcare)^2

childcare_gap <- mean(ame_data_childcare$pred_female - ame_data_childcare$pred_male, na.rm = TRUE)
childcare_gap

### Gender Gap Model Predictions for Main Subgroup (Plotted) ###################

# Household tasks
female_householdtasks_avg <- mean(predictions_householdtasks_data$pred_duration[predictions_householdtasks_data$tesex == "Female"])
male_householdtasks_avg <- mean(predictions_householdtasks_data$pred_duration[predictions_householdtasks_data$tesex == "Male"])
householdtasks_gap <- female_householdtasks_avg - male_householdtasks_avg
householdtasks_gap

# Childcare
female_childcare_avg <- mean(predictions_childcare_data$pred_duration[predictions_childcare_data$tesex == "Female"])
male_childcare_avg <- mean(predictions_childcare_data$pred_duration[predictions_childcare_data$tesex == "Male"])
childcare_gap <- female_childcare_avg - male_childcare_avg
childcare_gap

### Exporting Summary Tables to Excel ##########################################

# Pulling relevant information
clean_model <- function(model, model_name) {
  tidy(model) %>%
    filter(!grepl("gestfips", term)) %>%
    mutate(stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*",TRUE ~ "")) %>%
    select(term, estimate, std.error, stars)
}

# Cleaning all 4 models
m1 <- clean_model(model_1, "Model 1")
m2 <- clean_model(model_2, "Model 2")
m3 <- clean_model(model_3, "Model 3")
m4 <- clean_model(model_4, "Model 4")

# Writing to Excel with 4 tabs
wb <- createWorkbook()

addWorksheet(wb, "Model 1 - Child Care (year)")
addWorksheet(wb, "Model 2 - Child Care (date)")
addWorksheet(wb, "Model 3 - HH Tasks (year)")
addWorksheet(wb, "Model 4 - HH Tasks (date)")

writeData(wb, "Model 1 - Child Care (year)", m1)
writeData(wb, "Model 2 - Child Care (date)", m2)
writeData(wb, "Model 3 - HH Tasks (year)",  m3)
writeData(wb, "Model 4 - HH Tasks (date)",  m4)

saveWorkbook(wb, "model_results.xlsx", overwrite = TRUE)

### 4 Models Without Controlling For Each Other (old) ##########################

# Training models
model_1 <- lm(sqrt_child_care_duration ~ tesex + year + tesex*year + weekend + employed + income_level + gestfips + race, data = train)
model_2 <- lm(sqrt_child_care_duration ~ tesex + tudiarydate_numeric + tesex*tudiarydate_numeric + employed + income_level + gestfips + race, data = train)
model_3 <- lm(sqrt_household_task_duration ~ tesex + year + tesex*year + weekend + employed + income_level + gestfips + race, data = train)
model_4 <- lm(sqrt_household_task_duration ~ tesex + tudiarydate_numeric + tesex*tudiarydate_numeric + employed + income_level + gestfips + race, data = train)

# Training basic models
model_1_basic <- lm(sqrt_child_care_duration ~ weekend + employed + income_level + gestfips + race, data = train)
model_2_basic <- lm(sqrt_child_care_duration ~ employed + income_level + gestfips + race, data = train)
model_3_basic <- lm(sqrt_household_task_duration ~ weekend + employed + income_level + gestfips + race, data = train)
model_4_basic <- lm(sqrt_household_task_duration ~ employed + income_level + gestfips + race, data = train)

# ANOVA comparisons
anova(model_1_basic, model_1)
anova(model_2_basic, model_2)
anova(model_3_basic, model_3)
anova(model_4_basic, model_4)

### ANOVA for 4 Models (old) ###################################################

# Training basic models without interactions
model_1_basic <- lm(sqrt_child_care_duration ~ weekend + employed + income_level + gestfips + race + sqrt_household_task_duration, data = train)
model_2_basic <- lm(sqrt_child_care_duration ~ employed + income_level + gestfips + race + sqrt_household_task_duration, data = train)
model_3_basic <- lm(sqrt_household_task_duration ~ weekend + employed + income_level + gestfips + race + sqrt_child_care_duration, data = train)
model_4_basic <- lm(sqrt_household_task_duration ~ employed + income_level + gestfips + race + sqrt_child_care_duration, data = train)

# ANOVA comparisons
anova(model_1_basic, model_1)
anova(model_2_basic, model_2)
anova(model_3_basic, model_3)
anova(model_4_basic, model_4)

### 10-Fold Cross Validation on 4 Models (Old) #################################

# Setting up 10-fold CV
train_control <- trainControl(method = "cv", number = 10)

# Removing rows with NAs (only removing 108 rows due to missing income_level)
model_vars <- c("sqrt_child_care_duration", "sqrt_household_task_duration",
                "tesex", "year", "tudiarydate_numeric", "weekend", "employed",
                "income_level", "gestfips", "race")
data_clean <- data[complete.cases(data[, model_vars]), ]

# Model 1: child care, categorical
cv_model_1 <- train(
  sqrt_child_care_duration ~ tesex + year + tesex*year + weekend + employed + income_level + gestfips + race + sqrt_household_task_duration,
  data = data_clean,
  method = "lm",
  trControl = train_control
)

# Model 2: child care, continuous
cv_model_2 <- train(
  sqrt_child_care_duration ~ tesex + tudiarydate_numeric + tesex*tudiarydate_numeric + employed + income_level + gestfips + race + sqrt_household_task_duration,
  data = data_clean,
  method = "lm",
  trControl = train_control
)

# Model 3: household tasks, categorical
cv_model_3 <- train(
  sqrt_household_task_duration ~ tesex + year + tesex*year + weekend + employed + income_level + gestfips + race + sqrt_child_care_duration,
  data = data_clean,
  method = "lm",
  trControl = train_control
)

# Model 4: household tasks, continuous
cv_model_4 <- train(
  sqrt_household_task_duration ~ tesex + tudiarydate_numeric + tesex*tudiarydate_numeric + employed + income_level + gestfips + race + sqrt_child_care_duration,
  data = data_clean,
  method = "lm",
  trControl = train_control
)

# Comparing results
results <- resamples(list(
  Model1 = cv_model_1,
  Model2 = cv_model_2,
  Model3 = cv_model_3,
  Model4 = cv_model_4
))

summary(results)