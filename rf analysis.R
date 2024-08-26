
rf_wide <- readRDS("C:/Users/yning/Desktop/rf_result_wide_2122")

library(ggplot2)
library(reshape2)

# visualization (all) ####
data_melted <- melt(rf_wide, id.vars = 'Variable', variable.name = 'Location', value.name = 'Importance')

colnames(summary_results) <- NULL
colnames(summary_results) <- c("Importance","Variable","Location")


# Heatmap of variable importance
importance_matrix <- as.matrix(rf_wide[,-1])

rownames(importance_matrix) <- rf_wide$Variable

library(RColorBrewer)

heatmap(importance_matrix, Rowv = NA, Colv = NA, col = brewer.pal(9,"PuOr"), scale = "row",
        margins = c(5,10), xlab = "Location", main = "Heatmap of Variable Importance")

# add colour bar??
library(gplots)

heatmap.2(importance_matrix, 
          Rowv = NA, 
          Colv = TRUE, 
          col = heat.colors(256), 
          scale = "row",
          margins = c(5, 10), 
          xlab = "Location", 
          ylab = "Variable", 
          main = "Heatmap of Variable Importance",
          key = TRUE)  



# Load necessary libraries
library(dplyr)

# Create faceted plot
p <- ggplot(summary_results, aes(x = Location, y = Importance)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Importance of Variables Across Locations",
       x = "Location", y = "Importance") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2)

# Display the plot
print(p)




library(plotly)

# Create an interactive plot
p <- ggplot(summary_results, aes(x = Variable, y = Importance, text = Location)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of Variable Importance Across Locations",
       x = "Variable", y = "Importance")

# Convert ggplot to plotly
p_interactive <- ggplotly(p)

# Display the interactive plot
p_interactive





#transpose ####
rf_transpose <- t(rf_wide)
View(rf_transpose)
colnames(rf_transpose) <- NULL
colnames(rf_transpose) <- c("prevalence","prevalence_est","incidence_est","natural_week_number",
                            "year","cumulative_incidence_7","cumulative_incidence_5","cumulative_incidence_10",
                            "cumulative_incidence_14","test_intensiveness")
rf_transpose <- rf_transpose[-1, -10]




# ranking ####

data_long <- melt(rf_wide, id.vars = 'Variable', variable.name = 'Location', value.name = 'Importance')

# Step 2: Calculate ranks for each variable using ties.method = "min"
data_long <- data_long %>%
  group_by(Variable) %>%
  mutate(Rank = rank(-Importance, ties.method = "min")) %>%
  ungroup()

# Step 3: Spread the data back to wide format with ranks included
data_rank_wide_2 <- data_long %>%
  select(-Importance) %>%
  spread(key = Location, value = Rank)


saveRDS(data_rank_wide,"data_rank_wide.rds")




# collinearity ####
# IMD
IMD_collinearity <- merge(summary_results, IMD, by.x = "Location", by.y = "location")

cor_results_IMD <- IMD_collinearity %>%
  group_by(Variable) %>%
  summarise(
    cor_IMD = cor(Importance, IMD_average_score, use = "complete.obs")
  )

print(cor_results_IMD)


# ethnicity
data_excluded <- data_IMD_2122[,c(1,5,8:12)]

first_non_zero <- function(x) {
  return(x[x != 0][1])
}

# Transform data to have a single row per location with ethnicity proportions
ethnicity_collinearity <- data_excluded %>%
  group_by(location) %>%
  summarize(
    proportion_white = first_non_zero(proportion_white),
    proportion_black = first_non_zero(proportion_black),
    proportion_mixed = first_non_zero(proportion_mixed),
    proportion_asian = first_non_zero(proportion_asian),
    proportion_other = first_non_zero(proportion_other)
  )

ethnicity_collinearity <- merge(summary_results, ethnicity_collinearity, by.x = "Location", by.y = "location")

cor_results_ethnicity <- ethnicity_collinearity %>%
  group_by(Variable) %>%
  summarise(
    cor_white = cor(Importance, proportion_white, use = "complete.obs"),
    cor_black = cor(Importance, proportion_black, use = "complete.obs"),
    cor_mixed = cor(Importance, proportion_mixed, use = "complete.obs"),
    cor_asian = cor(Importance, proportion_asian, use = "complete.obs"),
    cor_other = cor(Importance, proportion_other, use = "complete.obs")
  )

print(cor_results_ethnicity)





# summarise dpd for incidence covariates (most significant)
# then you can compare summary metrics across LTLA
# the shape of dpd itself is a form of testing bias - how does testing deviate from
# the expectation of increasing linearly with incidence
# testing bias ####


# most significant 
rf_transpose <- as.data.frame(rf_transpose)

rf_transpose$cumulative_incidence_5 <- as.numeric(rf_transpose$cumulative_incidence_5)
rf_transpose$cumulative_incidence_7 <- as.numeric(rf_transpose$cumulative_incidence_7)
rf_transpose$cumulative_incidence_10 <- as.numeric(rf_transpose$cumulative_incidence_10)
rf_transpose$cumulative_incidence_14 <- as.numeric(rf_transpose$cumulative_incidence_14)

mean_cumulative_incidence_5 <- mean(rf_transpose$cumulative_incidence_5, na.rm = TRUE)
mean_cumulative_incidence_7 <- mean(rf_transpose$cumulative_incidence_7, na.rm = TRUE)
mean_cumulative_incidence_10 <- mean(rf_transpose$cumulative_incidence_10, na.rm = TRUE)
mean_cumulative_incidence_14 <- mean(rf_transpose$cumulative_incidence_14, na.rm = TRUE)

print(paste("Mean of cumulative_incidence_5:", mean_cumulative_incidence_5))
print(paste("Mean of cumulative_incidence_7:", mean_cumulative_incidence_7))
print(paste("Mean of cumulative_incidence_10:", mean_cumulative_incidence_10))
print(paste("Mean of cumulative_incidence_14:", mean_cumulative_incidence_14))


# cumulative incidence 14 across LTLAs ####
location_code <- rownames(rf_transpose)
rf_transpose <- rf_transpose %>% mutate(location = location_code)

dev.off()

ggplot(rf_transpose, aes(x = location, y = cumulative_incidence_14, group = 1)) +
  geom_line() + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  labs(title = "Line Chart of cumulative_incidence_14 by Location",
       x = "Location",  
       y = "Cumulative Incidence 14") 




# finding the area under curve for all LTLA's pdp ####
library(pracma)

unique_locations <- unique(data_IMD_2122$location)

calculate_auc <- function(x,y) {
  auc <- trapz(x,y)
  return(auc)
}

auc_list <- list()


for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  
  pd <- partial(rf_model, pred.var = "cumulative_incidence_14", 
                train = local_data, grid.resolution = 100)
  
  # Calculate AUC1
  x1 <- pd$cumulative_incidence_14
  y1 <- pd$yhat
  auc1 <- calculate_auc(x1, y1)
  
  
  auc_list[[i]] <- list(location = unique_locations[i], AUC1 = auc1)
  
}




for (i in 1:length(unique_locations)) {
  
  local_data <- subset(data_IMD_2122, location == unique_locations[i])
  
  pd <- partial(rf_model, pred.var = "cumulative_incidence_14", 
                train = local_data, grid.resolution = 100)
  
  median_value <- median(pd$cumulative_incidence_14)
  
  lower_half <- pd$cumulative_incidence_14 <= median_value
  upper_half <- pd$cumulative_incidence_14 > median_value
  
  # Calculate AUC1
  x1 <- pd$cumulative_incidence_14[lower_half]
  y1 <- pd$yhat[lower_half]
  auc1 <- calculate_auc(x1, y1)
  
  # Calculate AUC2
  x2 <- pd$cumulative_incidence_14[upper_half]
  y2 <- pd$yhat[upper_half]
  auc2 <- calculate_auc(x2, y2)
  
  
  auc_list[[i]] <- list(location = unique_locations[i], AUC1 = auc1, AUC2 = auc2)
  
}



print(auc_list)
View(auc_list)

# correlation between auc and ethnicity / IMD
# IMD

auc_df <- do.call(rbind, lapply(auc_list, as.data.frame))

auc_IMD <- merge(auc_df, IMD, by = "location")

cor_auc1_imd <- cor(auc_IMD$AUC1, auc_IMD$IMD_average_score, use = "complete.obs")
cor_auc2_imd <- cor(auc_IMD$AUC2, auc_IMD$IMD_average_score, use = "complete.obs")

print(paste("Correlation between AUC1 and IMD_average_score: ", cor_auc1_imd))
print(paste("Correlation between AUC2 and IMD_average_score: ", cor_auc2_imd))

# ethnicity
ethnicity_df <- data_excluded %>%
  group_by(location) %>%
  summarize(
    proportion_white = first_non_zero(proportion_white),
    proportion_black = first_non_zero(proportion_black),
    proportion_mixed = first_non_zero(proportion_mixed),
    proportion_asian = first_non_zero(proportion_asian),
    proportion_other = first_non_zero(proportion_other)
  )

auc_ethnicity <- merge(auc_df, ethnicity_df, by = "location")


cor_auc_ethnicity_1 <- auc_ethnicity %>%
  summarise(
    cor_white = cor(AUC1, proportion_white, use = "complete.obs"),
    cor_black = cor(AUC1, proportion_black, use = "complete.obs"),
    cor_mixed = cor(AUC1, proportion_mixed, use = "complete.obs"),
    cor_asian = cor(AUC1, proportion_asian, use = "complete.obs"),
    cor_other = cor(AUC1, proportion_other, use = "complete.obs")
  )

print(cor_auc_ethnicity_1)

cor_auc_ethnicity_2 <- auc_ethnicity %>%
  summarise(
    cor_white = cor(AUC2, proportion_white, use = "complete.obs"),
    cor_black = cor(AUC2, proportion_black, use = "complete.obs"),
    cor_mixed = cor(AUC2, proportion_mixed, use = "complete.obs"),
    cor_asian = cor(AUC2, proportion_asian, use = "complete.obs"),
    cor_other = cor(AUC2, proportion_other, use = "complete.obs")
  )

print(cor_auc_ethnicity_2)



# prediction analysis
# have the policy timeline - covariate momentum
# regression vs rf 