
getwd()
setwd("C:/Users/yning/Desktop")

##import data####
library(readxl)
daily_incidence_ltla_ethnicity_new_pcrlfd <- readRDS("C:/Users/yning/Desktop/daily_incidence_ltla_ethnicity_new_pcrlfd.rds")
daily_inc_ethnicity <- daily_incidence_ltla_ethnicity_new_pcrlfd
data <- daily_inc_ethnicity
test_report_table5 <- read_excel("test_report_table5.xlsx", 
                                 sheet = "Table2")
test_data <- test_report_table5
IMD <- read_excel("IMD.xlsx", sheet = "IMD")



##load packages####
library(lubridate)
library(tidyr)
library(dplyr)
library(zoo)


# convert the date format in test_data to yyyy/mm/dd for data merging 
test_data$Date <- dmy(test_data$Date)
test_data$Date <- format(test_data$Date, "%Y/%m/%d")


# weekly total test numbers 
test_data$natural_week_number <- lubridate::week(ymd(test_data$Date))
test_data$year <- format(as.Date(test_data$Date, format="%Y/%m/%d"),"%Y")


test_data <- test_data %>%
  group_by(LTLA,natural_week_number,year) %>%
  mutate(weekly_tests= sum(Tests)) %>%
  ungroup()
View(test_data)

# rename columns for merge
colnames(test_data) <- NULL
colnames(test_data) <- c('location','Date','tests','natural_week_number','year','weekly_total_tests')

#splitting 'location_fine' into 'location' and 'ethnicity'
data <- separate(data, location_fine, into = c("location", "ethnicity"), sep = "-")

# merge the test_data with incidence/prevalence/ethnicity data
data <- merge(data, test_data, by = c("location", "Date"))




#ethnicity column 
data <- data %>%
  group_by(location) %>%
  mutate(
    proportion_white = if_else(ethnicity == "White", pop/pop_ltla, 0),
    proportion_black = if_else(ethnicity == "Black; African; Black British or Caribbean", pop/pop_ltla, 0),
    proportion_mixed = if_else(ethnicity == "Mixed or Multiple ethnic groups", pop/pop_ltla, 0),
    proportion_asian = if_else(ethnicity == "Asian or Asian British", pop/pop_ltla, 0),
    proportion_other = if_else(ethnicity == "Other ethnic group", pop/pop_ltla, 0)
  ) %>%
  ungroup()




#create the column: cumulative incidence over past 7 days ####
data <- data %>%
  group_by(location) %>%
  arrange(Date) %>%
  mutate(cumulative_incidence_7 = cumsum(incidence_est) - lag(cumsum(incidence_est), 7, default = 0)) %>%
  ungroup()

# Select the last day of each week to apply the cumulative incidence calculation
data$Date <- as.Date(data$Date)

result_7 <- data %>%
  group_by(natural_week_number, year,location) %>%
  filter(Date == max(Date)) %>%
  ungroup()


#create the column: cumulative incidence over past 5 days ####
data <- data %>%
  group_by(location) %>%
  arrange(Date) %>%
  mutate(cumulative_incidence_5 = cumsum(incidence_est) - lag(cumsum(incidence_est), 5, default = 0)) %>%
  ungroup()

# Select the last day of each week to apply the cumulative incidence calculation
result_5 <- data %>%
  group_by(natural_week_number, year,location) %>%
  filter(Date == max(Date)) %>%
  ungroup()



#create the column: cumulative incidence over past 10 days ####
data <- data %>%
  group_by(location) %>%
  arrange(Date) %>%
  mutate(cumulative_incidence_10 = cumsum(incidence_est) - lag(cumsum(incidence_est), 10, default = 0)) %>%
  ungroup()

# Select the last day of each week to apply the cumulative incidence calculation
result_10 <- data %>%
  group_by(natural_week_number, year,location) %>%
  filter(Date == max(Date)) %>%
  ungroup()



#create the column: cumulative incidence over past 14 days ####
data <- data %>%
  group_by(location) %>%
  arrange(Date) %>%
  mutate(cumulative_incidence_14 = cumsum(incidence_est) - lag(cumsum(incidence_est), 14, default = 0)) %>%
  ungroup()

# Select the last day of each week to apply the cumulative incidence calculation
result_14 <- data %>%
  group_by(natural_week_number, year,location) %>%
  filter(Date == max(Date)) %>%
  ungroup()


# merging IMD ####
IMD <- IMD[,c(1,5)]
colnames(IMD) <- NULL
colnames(IMD) <- c("location", "IMD_average_score")
data_IMD <- merge(result_14, IMD, by ='location')

#adding test intensiveness
data_IMD$test_intensiveness <- (data_IMD$tests/data_IMD$pop_ltla)*1000

# subset data - exclude unnecessary columns 
data_IMD <- data_IMD[,-c(2,4,5:8,11:15)]


saveRDS(data_IMD, file = 'data_IMD_0613')








