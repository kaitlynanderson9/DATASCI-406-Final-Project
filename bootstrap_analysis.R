

library(tidyverse)
library(boot)


# ANOTHER METHOD

shooting_data_summary <- read.csv("school_shooting_db_20200316 (1).csv")

shooting_data_summary_clean <- subset(shooting_data_summary, !is.na(Shooter.Age) & Shooter.Age <= 100)

bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices]
  return(mean(sample_data))
}

set.seed(123)
bootstrap_results <- boot(data = shooting_data_summary_clean$Shooter.Age, 
                          statistic = bootstrap_mean, 
                          R = 10000)

ci <- boot.ci(bootstrap_results, type = "perc")

mean_boot <- mean(bootstrap_results$t)
lower_ci <- ci$percent[4]
upper_ci <- ci$percent[5]

bootstrap_df <- data.frame(Age = bootstrap_results$t)

ggplot(bootstrap_df, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "black") +
  geom_vline(xintercept = lower_ci, color = "red", linetype = "dashed", size = 1, 
             label = paste("Lower CI:", round(lower_ci, 2))) +
  geom_vline(xintercept = upper_ci, color = "red", linetype = "dashed", size = 1, 
             label = paste("Upper CI:", round(upper_ci, 2))) +
  geom_vline(xintercept = mean_boot, color = "blue", linetype = "solid", size = 1, 
             label = paste("Mean Age:", round(mean_boot, 2))) +
  labs(title = "Bootstrap Distribution of Average Shooter Age",
       x = "Average Age", y = "Density") +
  theme_minimal()


regression_data <- shooting_data_summary %>%
  select(Shooter.Age, State, Wounded, Killed..includes.shooter.) %>%
  drop_na()

model <- lm(Killed..includes.shooter. ~ Shooter.Age + Wounded, data = regression_data)
summary(model)

# PREDICTION HEREEEEEEEEE

library(boot)
library(ggplot2)
library(dplyr)

file_path <- "school_shooting_db_20200316 (1).csv"
shooting_data <- read.csv(file_path)

colnames(shooting_data) <- trimws(colnames(shooting_data))

shooting_data_CLEANED <- shooting_data %>%
  filter(!is.na(Shooter.Age) & Shooter.Age <= 100)

east_coast_states_TOTAL <- c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "DE", 
                             "MD", "VA", "NC", "SC", "GA", "FL", "PA")
west_coast_states_TOTAL <- c("CA", "OR", "WA", "AK", "HI", "NV", "ID", "AZ", "MT", "UT", "WY")

east_coast_data_TOTAL <- shooting_data_CLEANED %>% filter(State %in% east_coast_states_TOTAL)
west_coast_data_TOTAL <- shooting_data_CLEANED %>% filter(State %in% west_coast_states_TOTAL)

east_coast_ages <- east_coast_data_TOTAL$Shooter.Age
west_coast_ages <- west_coast_data_TOTAL$Shooter.Age

bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices]
  mean(sample_data)
}

set.seed(123)
east_boot_HMMM <- boot(data = east_coast_ages, statistic = bootstrap_mean, R = 10000)
west_boot_HMMM <- boot(data = west_coast_ages, statistic = bootstrap_mean, R = 10000)

east_ci <- boot.ci(east_boot_HMMM, type = "perc")$percent[4:5]
west_ci <- boot.ci(west_boot_HMMM, type = "perc")$percent[4:5]

bootstrap_df <- data.frame(
  Age = c(east_boot_HMMM$t, west_boot_HMMM$t),
  Region = rep(c("East Coast", "West Coast"), each = 10000)
)

ggplot(bootstrap_df, aes(x = Age, fill = Region)) +
  geom_histogram(aes(y = ..density..), bins = 50, color = "black", alpha = 0.6, position = "identity") +
  geom_vline(xintercept = mean(east_coast_ages), color = "blue", linetype = "solid", size = 1) +
  geom_vline(xintercept = east_ci, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(west_coast_ages), color = "red", linetype = "solid", size = 1) +
  geom_vline(xintercept = west_ci, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Bootstrap Distribution of Shooter Ages by Region",
    x = "Shooter Age", y = "Density", fill = "Region"
  ) +
  theme_minimal()



