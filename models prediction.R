
data_IMD_2122 <- readRDS("C:/Users/yning/Desktop/data_IMD_2122.rds")
library(randomForest)
unique_locations <- unique(data_IMD_2122$location)


# rf & lm predictions ####

diff <- rep(0, length(unique_locations))
rsq_rflm <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.6, 0.4))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  
  # RF
  rf_model <- randomForest(weekly_total_tests ~ ., data = training,
                           ntree = 5000, nodesize = 100, importance = TRUE)

  predictions <- predict(rf_model, newdata = testing)
  
  rmse <- sqrt(mean((predictions - testing$weekly_total_tests)^2))
  
  # R squared RF
  actuals <- testing$weekly_total_tests
  ss_total <- sum((actuals - mean(actuals))^2)
  rf_residual <- sum((actuals - predictions)^2)
  r_squared_rf <- 1 - (rf_residual / ss_total)
  
  
  # LM 
  lm_model <- lm(weekly_total_tests ~ ., data = training)
  
  predictions_lm <- predict(lm_model, newdata = testing)
  
  rmse_lm <- sqrt(mean((predictions_lm - testing$weekly_total_tests)^2))
  
  # R squared lm
  lm_residual <- sum((actuals - predictions_lm)^2)
  r_squared_lm <- 1 - (lm_residual / ss_total)
  
  
  diff[i] <- rmse - rmse_lm
  
  rsq_rflm[[i]] <- list(
    lm_model = summary(lm_model)$r.squared,
    rf_model = 1 - var(rf_model$y - rf_model$predicted)/var(rf_model$y),
    rmse_rf = rmse,
    rmse_lm = rmse_lm,
    rsq_rf = r_squared_rf,
    rsq_lm = r_squared_lm
  )
  
}
rsq_rflm[[1]]
1 - var(rf_model$y - rf_model$predicted)/var(rf_model$y)

# store results: matrix to df
rsq_rflm_matrix <- matrix(NA, nrow = length(unique_locations), ncol = 7)

colnames(rsq_rflm_matrix) <- c("rsq_lm_model", "rsq_rf_model", "rsq_lm", "rsq_rf", 
                               "rmse_rf", "rmse_lm", "location")

for (i in 1:length(unique_locations)) {
  rsq_rflm_matrix[i, ] <- c(
    rsq_rflm[[i]]$lm_model,
    rsq_rflm[[i]]$rf_model,
    rsq_rflm[[i]]$rsq_lm,
    rsq_rflm[[i]]$rsq_rf,
    rsq_rflm[[i]]$rmse_rf,
    rsq_rflm[[i]]$rmse_lm,
    unique_locations[i]  # Add the corresponding location name
  )
}

View(rsq_rflm_df)
mean(rsq_rflm_df$rsq_lm)
mean(rsq_rflm_df$rsq_rf)
mean(rsq_rflm_df$rmse_lm)
mean(rsq_rflm_df$rmse_rf)
# Convert the matrix to a data frame
rsq_rflm_df <- as.data.frame(rsq_rflm_matrix)

saveRDS(rsq_rflm_df, file = "rflm_results.rds")




# linear regression model ####
library(dplyr)

prediction_results_lm <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.4, 0.6))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  lm_model <- lm(weekly_total_tests ~ ., data = training)
  
  predictions <- predict(lm_model, newdata = testing)
  
  rmse <- sqrt(mean((predictions - testing$weekly_total_tests)^2))
  
  # Calculate R squared
  actuals <- testing$weekly_total_tests
  ss_total <- sum((actuals - mean(actuals))^2)
  ss_residual <- sum((actuals - predictions)^2)
  r_squared <- 1 - (ss_residual / ss_total)

  
  prediction_results_lm[[i]] <- list(
    predictions = predictions,
    actuals = testing$weekly_total_tests,
    rmse = rmse,
    r_squared = r_squared
  )
}


prediction_results_lm[[2]]

summary(lm_model)


# calculate the difference 
rmse_rf <- numeric(length(unique_locations))
rmse_lm <- numeric(length(unique_locations))
rmse_diff <- numeric(length(unique_locations))


for (i in 1:length(unique_locations)) {
  rmse_rf[i] <- prediction_results[[i]]$rmse
  rmse_lm[i] <- prediction_results_lm[[i]]$rmse
  rmse_diff[i] <- rmse_rf[i] - rmse_lm[i]
}


rmse_comparison_df <- data.frame(
  location = unique_locations,
  rmse_rf = rmse_rf,
  rmse_lm = rmse_lm,
  rmse_diff = rmse_diff
)

print(rmse_comparison_df)

mean(rmse_comparison_df$rmse_lm)

#extract r squared of lm and percentage explained variation of rf for training



# neural network ####
library(nnet)
library(vip)
library(NeuralNetTools)
library(ggplot2)

results_nnet <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.6, 0.4))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  nn <- nnet(weekly_total_tests ~ ., data = training, size = 7, decay = 0.1,
             linout = TRUE, trace = FALSE)
  
  predictions_nn <- predict(nn, newdata = testing)
  
  rmse_nnet <- sqrt(mean((predictions_nn - testing$weekly_total_tests)^2))
  
  actuals <- testing$weekly_total_tests
  nn_total <- sum((actuals - mean(actuals))^2)
  nn_residual <- sum((actuals - predictions_nn)^2)
  r_squared_nn <- 1 - (nn_residual / nn_total)

  results_nnet[[i]] <- list(
    model = nn,
    rmse_nnet = rmse_nnet,
    r_squared_nn = r_squared_nn
  )
}

results_nnet


results_df_nn <- data.frame(
  location = character(),
  rmse_nn = numeric(),
  r2 = numeric(),
  stringsAsFactors = FALSE
)


for (i in 1:length(unique_locations)) {
  
  res <- results_nnet[[i]]
  
  results_df_nn <- rbind(results_df_nn, data.frame(
    location = unique_locations[i],
    rmse_nn = res$rmse_nnet,
    r2 = res$r_squared_nn,
    stringsAsFactors = FALSE
  ))
}


mean(results_df_nn$r2)




# neural net + rf 
diff_nn_rf <- rep(0, length(unique_locations))

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.4, 0.6))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  # RF 
  rf_model <- randomForest(weekly_total_tests ~ ., data = training,
                           ntree = 5000, nodesize = 100, importance = TRUE)
  
  
  predictions <- predict(rf_model, newdata = testing)
  
  rmse <- sqrt(mean((predictions - testing$weekly_total_tests)^2))
  
  # neural net
  nn <- nnet(weekly_total_tests ~ ., data = training, size = 7, decay = 0.1,
             linout = TRUE, trace = FALSE)
  
  predictions_nn <- predict(nn, newdata = testing)
  
  rmse_nnet <- sqrt(mean((predictions_nn - testing$weekly_total_tests)^2))
  
  # difference
  diff_nn_rf[i] <- rmse - rmse_nnet
  
}


diff_nn_rf
barplot(diff_nn_rf)
mean(diff_nn_rf)









# boosted tree ML ####
library(xgboost)

results_gbm <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.4, 0.6))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  train_matrix <- data.matrix(subset(training, select = -weekly_total_tests))
  train_labels <- training$weekly_total_tests
  test_matrix <- data.matrix(subset(testing, select = -weekly_total_tests))

  
  gbm <- xgboost(
    data = train_matrix,
    label = train_labels,
    objective = "reg:squarederror",
    nrounds = 100,
    max_depth = 5,
    eta = 0.3,
    verbose = 0)
  
  predictions_gbm <- predict(gbm, newdata = test_matrix)
  
  rmse_gbm <- sqrt(mean((predictions_gbm - testing$weekly_total_tests)^2))
  
  # R squared for GBM
  actuals <- testing$weekly_total_tests
  ss_total <- sum((actuals - mean(actuals))^2)
  ss_residual <- sum((actuals - predictions_gbm)^2)
  r_squared_gbm <- 1 - (ss_residual / ss_total)
  
  results_gbm[[i]] <- list(
    model = gbm,
    rmse_gbm = rmse_gbm,
    r2 = r_squared_gbm
  )
}


results_gbm[[1]]




# gbm + rf ####

diff_gbm_rf <- rep(0, length(unique_locations))

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.8, 0.2))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  # RF 
  rf_model <- randomForest(weekly_total_tests ~ ., data = training,
                           ntree = 5000, nodesize = 100, importance = TRUE)
  
  
  predictions <- predict(rf_model, newdata = testing)
  
  rmse <- sqrt(mean((predictions - testing$weekly_total_tests)^2))
  
  # gbm
  
  gbm <- xgboost(
    data = data.matrix(subset(training, select = -weekly_total_tests)),
    label = training$weekly_total_tests,
    objective = "reg:squarederror",
    nrounds = 100,
    max_depth = 5,
    eta = 0.3,
    verbose = 0)
  
  predictions_gbm <- predict(gbm, newdata = data.matrix(testing))
  
  rmse_gbm <- sqrt(mean((predictions_gbm - testing$weekly_total_tests)^2))
  
  # difference
  diff_gbm_rf[i] <- rmse - rmse_gbm
  
  # R squared for GBM
  actuals <- testing$weekly_total_tests
  ss_total <- sum((actuals - mean(actuals))^2)
  ss_residual <- sum((actuals - predictions_gbm)^2)
  r_squared_gbm <- 1 - (ss_residual / ss_total)
  
}

r_squared_gbm

barplot(diff_gbm_rf)



# nn + gbm ####
rsq_nngbm <- list()

for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  local_data <- local_data[, -c(1, 8, 9, 10, 11, 12, 17, 18)]
  set.seed(123)
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.8, 0.2))
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  
  # neural net
  nn <- nnet(weekly_total_tests ~ ., data = training, size = 7, decay = 0.1,
             linout = TRUE, trace = FALSE)
  
  predictions_nn <- predict(nn, newdata = testing)
  
  rmse_nnet <- sqrt(mean((predictions_nn - testing$weekly_total_tests)^2))
  
  # r squared NN
  actuals <- testing$weekly_total_tests
  ss_total <- sum((actuals - mean(actuals))^2)
  nn_residual <- sum((actuals - predictions_nn)^2)
  r_squared_nn <- 1 - (nn_residual / ss_total)
  
  
  # gbm
  labels <- training$weekly_total_tests
  
  gbm <- xgboost(
    data = data.matrix(subset(training, select = -weekly_total_tests)),
    label = labels,
    objective = "reg:squarederror",
    nrounds = 100,
    max_depth = 5,
    eta = 0.3,
    verbose = 0)
  
  predictions_gbm <- predict(gbm, newdata = data.matrix(testing))
  
  rmse_gbm <- sqrt(mean((predictions_gbm - testing$weekly_total_tests)^2))

  
  # R squared for GBM
  gbm_residual <- sum((actuals - predictions_gbm)^2)
  r_squared_gbm <- 1 - (gbm_residual / ss_total)
  
  rsq_nngbm[[i]] <- list(
    rmse_nn = rmse_nnet,
    rmse_gbm = rmse_gbm,
    rsq_nn = r_squared_nn,
    rsq_gbm = r_squared_gbm
    )
  
}

rsq_nngbm[[1]]


# GBM ricardo ####
library(caret)
set.seed(123)

local_data <- subset(data_IMD_2122, location == unique_locations[4])
ind <- sample(2, nrow(local_data),
              replace = TRUE,
              prob = c(0.8, 0.2))
outcome_training <- local_data[ind == 1, 7]
outcome_testing <- local_data[ind == 2, 7]

local_data <- local_data[, -c(1, 8, 7, 9, 10, 11, 12, 17, 18)]
local_data$year <- as.numeric(local_data$year)
training <- local_data[ind == 1, ]
testing <- local_data[ind == 2, ]

grid_tune <- expand.grid(
  nrounds = c(500, 1000, 1500), #number of trees
  max_depth = c(2, 4, 6),
  eta = 0.3, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 0, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # c(0.4, 0.6, 0.8, 1.0) subsample ratio of columns for tree
  min_child_weight = 1, # c(1,2,3) # the larger, the more conservative the model
  #is; can be used as a stop
  subsample = 1 # c(0.5, 0.75, 1.0) # used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "cv",
                              number = 3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)

xgb_tune <- train(x = training,
                  y = outcome_training,
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method = "xgbTree",
                  verbose = TRUE)

# Prediction:
xgb.pred <- predict(xgb_tune, testing)

mse <- mean((outcome_testing - xgb.pred)^2)
mae <- caret::MAE(outcome_testing, xgb.pred)
rmse <- caret::RMSE(outcome_testing, xgb.pred)
actuals <- outcome_testing
ss_total <- sum((actuals - mean(actuals))^2)
ss_residual <- sum((actuals - xgb.pred)^2)
r_squared_gbm <- 1 - (ss_residual / ss_total)
rmse_gbm <- sqrt(mean((xgb.pred - outcome_testing)^2))




# loop ####

results_gbm <- list()

for (i in 1:length(unique_locations)) {
  
  set.seed(123)
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  ind <- sample(2, nrow(local_data),
                replace = TRUE,
                prob = c(0.6, 0.4))
  outcome_training <- local_data[ind == 1, 7]
  outcome_testing <- local_data[ind == 2, 7]
  
  local_data <- local_data[, -c(1, 8, 7, 9, 10, 11, 12, 17, 18)]
  
  local_data$year <- as.numeric(local_data$year)
  
  training <- local_data[ind == 1, ]
  testing <- local_data[ind == 2, ]
  
  
  train_control <- trainControl(method = "cv",
                                number = 3,
                                verboseIter = TRUE,
                                allowParallel = TRUE)
  
  xgb_tune <- train(x = training,
                    y = outcome_training,
                    trControl = train_control,
                    method = "xgbTree",
                    verbose = TRUE)
  
  # Prediction:
  xgb.pred <- predict(xgb_tune, testing)
  
  mse <- mean((outcome_testing - xgb.pred)^2)
  mae <- caret::MAE(outcome_testing, xgb.pred)
  rmse <- caret::RMSE(outcome_testing, xgb.pred)
  actuals <- outcome_testing
  ss_total <- sum((actuals - mean(actuals))^2)
  ss_residual <- sum((actuals - xgb.pred)^2)
  r_squared_gbm <- 1 - (ss_residual / ss_total)

  
  results_gbm[[i]] <- list(
    model = xgb_tune,
    mse = mse,
    mae = mae,
    rmse_gbm = rmse,
    r2 = r_squared_gbm
  )
}

results_gbm
results_gbm[[1]]



# store as df ####

results_df_gbm <- data.frame(
  location = character(),
  mse = numeric(),
  mae = numeric(),
  rmse_gbm = numeric(),
  r2 = numeric(),
  stringsAsFactors = FALSE
)

view(results_df_gbm)

for (i in 1:length(unique_locations)) {
  
  res <- results_gbm[[i]]
  
  results_df_gbm <- rbind(results_df_gbm, data.frame(
    location = unique_locations[i],
    mse = res$mse,
    mae = res$mae,
    rmse_gbm = res$rmse_gbm,
    r2 = res$r2,
    stringsAsFactors = FALSE
  ))
}

saveRDS(results_df_gbm, file = "gbm_results.rds")


mean(results_df_gbm$r2)
