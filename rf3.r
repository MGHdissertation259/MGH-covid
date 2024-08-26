
library(randomForest)
library(pdp)
library(reshape2)
library(dplyr)
library(tidyr)

setwd("C:/Users/yning/Desktop")
data_IMD<- readRDS('data_IMD_0613')

data_IMD_2122 <- data_IMD[data_IMD$year %in% c("2021","2022"),]


saveRDS(data_IMD_2122, file ="data_IMD_2122.rds")

#remove week 53
data_IMD_2122 <- data_IMD_2122 %>%
  filter(natural_week_number != 53)

data_IMD_2122$location <- as.factor(data_IMD_2122$location)
data_IMD_2122$year <- as.factor(data_IMD_2122$year)
unique_locations <- unique(data_IMD_2122$location)


# for loop ####
results <- list()

for (i in 1:length(unique_locations)) {

  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.8, 0.2))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]

  rf_model <- randomForest(weekly_total_tests ~ ., data = training,
                           ntree = 5000, nodesize = 100, importance = TRUE)

  results[[i]] <- list(
    model = rf_model,
    importance = importance(rf_model)
  )
}


# partial dependence plot ####
imp <- results[[i]]$importance
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(3, 3))

for (i in seq_along(impvar)) {
  partialPlot(rf_model, training, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
              
}

par(op)

par(mfrow = c(1,1))
partialPlot(rf_model, training, x.var = natural_week_number, xlab= "Partial Dependence on natural week number")


# summarise the results ####
summary_results <- data.frame()

# Loop over each location to extract and summarize the model information
for (i in 1:length(results)) {
  
  imp <- results[[i]]$importance
  rf_model <- results[[i]]$model
  importance_df <- as.data.frame(imp)
  importance_df$Variable <- rownames(importance_df)
  importance_df$Location <- as.character(unique_locations[i])
  rownames(importance_df) <- NULL
  
  # Combine the results into a single data frame
  summary_results <- rbind(summary_results, importance_df)
}

# View the summary results
print(summary_results)
nrow(summary_results)


# transform summary data
summary_results <- summary_results[,-2]
colnames(summary_results) <- NULL
colnames(summary_results) <- c("Importance","Variable","Location")
data_wide <- summary_results %>% 
  pivot_wider(names_from = Location, values_from = '%IncMSE')

# Print the reshaped data
View(data_wide)


saveRDS(data_wide, file = "rf_result_wide_2122")




# prediction ####
prediction_results <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.8, 0.2))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  rf_model <- randomForest(weekly_total_tests ~ ., data = training,
                           ntree = 5000, nodesize = 100, importance = TRUE)
  
  
  predictions <- predict(rf_model, newdata = testing)
  
  rmse <- sqrt(mean((predictions - testing$weekly_total_tests)^2))
  
  prediction_results[[i]] <- list(
    model = rf_model,
    importance = importance(rf_model),
    predictions = predictions,
    actuals = testing$weekly_total_tests,
    rmse = rmse
  )
}


for (i in 1:length(unique_locations)) {
  cat("Location:", unique_locations[i], "\n")
  cat("RMSE:", prediction_results[[i]]$rmse, "\n")
  cat("\n")
}

prediction_results


# store the predictions into df
prediction_results_df <- do.call(rbind, lapply(prediction_results, function(x) {
  data.frame(
    location = x$location,
    predictions = predictions,
    actuals = testing$weekly_total_tests,
    rmse = x$rmse
  )
}))

# Print the results data frame
print(prediction_results_df)



# rf interaction ####
library(iml)

results_interaction <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.8, 0.2))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  rf_model <- randomForest(weekly_total_tests ~ ., data = training,
                           ntree = 5000, nodesize = 100, importance = TRUE)
  
  
  # interaction
  training_features <- training[, !colnames(training) %in% "weekly_total_tests"]
  
  predictor <- Predictor$new(rf_model, data = training_features, y = training$weekly_total_tests)
  
  interaction <- Interaction$new(predictor)

  results_interaction[[i]] <- list(
    model = rf_model,
    importance = importance(rf_model),
    interaction = interaction

  )
}

results_interaction[[1]]$interaction
















library(ggplot2)

# Assuming data_IMD_filtered is your filtered dataframe
# Calculate the average weekly_total_tests per natural_week_number across all locations
average_tests_per_week <- data_IMD_2122 %>%
  group_by(natural_week_number) %>%
  summarise(avg_tests = mean(weekly_total_tests, na.rm = TRUE))

# Plot the average weekly_total_tests per natural_week_number
ggplot(average_tests_per_week, aes(x = natural_week_number, y = avg_tests)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Weekly Total Tests per Week Number",
       x = "Week Number",
       y = "Average Weekly Total Tests") +
  theme_minimal()


last_week_data <- data_IMD_2122 %>%
  filter(year == "2022" & natural_week_number == max(natural_week_number))

# Print the total tests for the very last week
print(last_week_data)
min(last_week_data$weekly_total_tests)

min_tests_entry <- data_IMD_2122 %>%
  filter(weekly_total_tests == 283)

# Print the entry with the minimum weekly total tests
print(min_tests_entry)
max(data_IMD_2122$natural_week_number)
