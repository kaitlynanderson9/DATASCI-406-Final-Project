
library(ggplot2)
library(tidyverse)
library(dplyr)

set.seed(42)

## load dataset

data <- read.csv("school_shooting_db_20200316.csv")
clean_df <- data

# calculate shooting rate for each state
shooting_counts <- clean_df |>
  group_by(State) |>
  summarise(Count = n(), .groups = "drop")

shooting_counts <- shooting_counts |>
  left_join(state_abbrev_to_full, by = c("State" = "Abbreviation"))

state_population <- data.frame(
  Abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", 
                   "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", 
                   "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", 
                   "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
                   "VT", "VA", "WA", "WV", "WI", "WY"),
  Population = c(5184000, 733500, 7500000, 3100000, 38900000, 5900000, 3600000, 1000000,
                 23000000, 11100000, 1400000, 2050000, 12500000, 6950000, 3200000, 
                 2900000, 4500000, 4700000, 1350000, 6200000, 7000000, 10100000, 
                 5800000, 3000000, 6200000, 1100000, 1960000, 3150000, 1400000, 
                 9000000, 2200000, 19500000, 10600000, 780000, 11700000, 4000000, 
                 4400000, 13000000, 1100000, 5300000, 890000, 6900000, 31000000, 
                 3400000, 640000, 8700000, 7700000, 1800000, 6000000, 590000)
)

# shooting rate = shootings per 100,000 people
shooting_counts <- shooting_counts |>
  left_join(state_population, by = c("State" = "Abbreviation")) |>
  mutate(ShootingRate = (Count / Population) * 100000)


# political affiliation analysis (simulation)

state_vote <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  Vote = c("Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Red", 
           "Red", "Blue", "Red", "Blue", "Red", "Red", "Red", "Red", "Red", 
           "Blue", "Blue", "Blue", "Red", "Blue", "Red", "Red", "Red", "Red", 
           "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", 
           "Blue", "Red", "Blue", "Red", "Red", "Red", "Red", "Red", "Blue", 
           "Blue", "Blue", "Red", "Red", "Red")
)

state_vote <- shooting_counts %>%
  left_join(state_vote, by = c("State" = "State"))

observed_diff <- state_vote %>%
  group_by(Vote) %>%
  summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
  summarise(diff = mean_rate[2] - mean_rate[1]) %>%
  pull(diff)

var_red <- var(state_vote %>% filter(Vote == "Red") %>% pull(ShootingRate), na.rm = TRUE)
var_blue <- var(state_vote %>% filter(Vote == "Blue") %>% pull(ShootingRate), na.rm = TRUE)

mean_red <- mean(state_vote %>% filter(Vote == "Red") %>% pull(ShootingRate), na.rm = TRUE)
mean_blue <- mean(state_vote %>% filter(Vote == "Blue") %>% pull(ShootingRate), na.rm = TRUE)

n_simulations <- 10000
simulated_diffs <- numeric(n_simulations)

for (i in 1:n_simulations) {

  sim_red <- rnorm(n = 25, mean = mean_red, sd = sqrt(var_red))
  sim_blue <- rnorm(n = 25, mean = mean_blue, sd = sqrt(var_blue))
  
  simulated_diffs[i] <- mean(sim_red) - mean(sim_blue)
}

n_permutations <- 10000

perm_diffs <- replicate(n_permutations, {
  
  permuted_labels <- sample(state_vote$Vote)
  permuted_data <- state_vote %>%
    mutate(PermVote = permuted_labels)
  
  perm_diff <- permuted_data %>%
    group_by(PermVote) %>%
    summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
    summarise(diff = mean_rate[2] - mean_rate[1]) %>%
    pull(diff)
  
  perm_diff
})

p_value <- mean(perm_diffs >= observed_diff)


alpha <- 0.05

critical_value <- quantile(perm_diffs, 1 - alpha)

data <- data.frame(
  value = c(simulated_diffs, perm_diffs),
  group = rep(c("Alternative", "Null"),
              times = c(length(simulated_diffs), length(perm_diffs)))
)

ggplot(data, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5, color = NA) +
  geom_vline(aes(xintercept = critical_value), color = "red", linetype = "solid") +
  labs(
    title = "Null and Alternative Distributions of Difference of Mean Shooting Rate",
    subtitle = paste("Observed Difference =", round(observed_diff, 4), 
                     "| p-value =", round(p_value, 4)),
    x = "Difference in Mean Shooting Rate",
    y = "Density", 
    fill = "Distribution",  # Legend title
    caption = "This figure shows that states where the majority of voters voted 
    for the Republican candidate in 2016 tend to have higher school shooting rates than
    states where the majority of voters voted for the Democratic candidate in 2016."
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.5))


cat("p-value for the permutation test: ", p_value, "\n")
power <- mean(simulated_diffs >= critical_value)
cat("Power of the test: ", power, "\n")
type_1_error <- mean(perm_diffs >= critical_value)
cat("Type 1 Error (False Positive Rate): ", type_1_error, "\n")
type_2_error <- mean(simulated_diffs < critical_value)
cat("Type 2 Error (False Negative Rate): ", type_2_error, "\n")
mse_null <- mean((perm_diffs - observed_diff)^2)
cat("MSE for the null hypothesis: ", mse_null, "\n")
mse_alt <- mean((simulated_diffs - observed_diff)^2)
cat("MSE for the alternative hypothesis: ", mse_alt, "\n")
bias_null <- mean(perm_diffs) - observed_diff
cat("Bias for the null hypothesis: ", bias_null, "\n")
bias_alt <- mean(simulated_diffs) - observed_diff
cat("Bias for the alternative hypothesis: ", bias_alt, "\n")


## population density analysis (simulation)

state_data <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", 
            "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", 
            "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", 
            "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
            "VT", "VA", "WA", "WV", "WI", "WY"),
  Density = c(101, 1.3, 65, 59, 250, 57, 747, 529, 422, 192, 
              223, 24, 226, 192, 57, 36, 115, 106, 45, 637, 
              898, 178, 72, 63, 89.52, 7.42, 25.22, 29, 157, 
              1263, 17, 415, 223, 11, 288, 59, 44, 290, 1060, 
              179, 12, 173, 117, 42, 70, 221, 118, 74, 109, 6)
)

state_data <- shooting_counts %>%
  left_join(state_data, by = c("State" = "State"))

low_threshold <- quantile(state_data$Density, 0.45, na.rm = TRUE)
high_threshold <- quantile(state_data$Density, 0.55, na.rm = TRUE)

state_data <- state_data %>%
  filter(Density <= low_threshold | Density >= high_threshold) %>%
  mutate(DensityGroup = ifelse(Density >= high_threshold, "High Density", "Low Density"))

observed_diff <- state_data %>%
  group_by(DensityGroup) %>%
  summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
  summarise(diff = mean_rate[1] - mean_rate[2]) %>%
  pull(diff)

var_high <- var(state_data %>% filter(DensityGroup == "High Density") %>% pull(ShootingRate), na.rm = TRUE)
var_low <- var(state_data %>% filter(DensityGroup == "Low Density") %>% pull(ShootingRate), na.rm = TRUE)

mean_high <- mean(state_data %>% filter(DensityGroup == "High Density") %>% pull(ShootingRate), na.rm = TRUE)
mean_low <- mean(state_data %>% filter(DensityGroup == "Low Density") %>% pull(ShootingRate), na.rm = TRUE)

n_simulations <- 10000
simulated_diffs <- numeric(n_simulations)

for (i in 1:n_simulations) {

  sim_high <- rnorm(n = 25, mean = mean_high, sd = sqrt(var_high))
  sim_low <- rnorm(n = 25, mean = mean_low, sd = sqrt(var_low))
  
  simulated_diffs[i] <- mean(sim_high) - mean(sim_low)
}

n_permutations <- 10000
perm_diffs <- replicate(n_permutations, {
  
  permuted_labels <- sample(state_data$DensityGroup)
  permuted_data <- state_data %>%
    mutate(PermDensityGroup = permuted_labels)
  
  perm_diff <- permuted_data %>%
    group_by(PermDensityGroup) %>%
    summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
    summarise(diff = mean_rate[1] - mean_rate[2]) %>%
    pull(diff)
  
  perm_diff
})

p_value <- mean(perm_diffs >= observed_diff)

alpha <- 0.05

critical_value <- quantile(perm_diffs, 1 - alpha)

data <- data.frame(
  value = c(simulated_diffs, perm_diffs),
  group = rep(c("Alternative", "Null"),
              times = c(length(simulated_diffs), length(perm_diffs)))
)

ggplot(data, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5, color = NA) +
  geom_vline(aes(xintercept = critical_value), color = "red", linetype = "solid") +
  labs(
    title = "Null and Alternative Distributions of Difference of Mean Shooting Rate",
    subtitle = paste("Observed Difference =", round(observed_diff, 4), 
                     "| p-value =", round(p_value, 4)),
    x = "Difference in Mean Shooting Rate",
    y = "Density", 
    fill = "Distribution",
    caption = "This figure shows that states with higher population densities tend 
    to have a higher school shooting rate than states with lower population densities."
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.5))

cat("p-value for the permutation test: ", p_value)
power <- mean(simulated_diffs >= critical_value)
cat("Power of the test: ", power, "\n")
type_1_error <- mean(perm_diffs >= critical_value)
cat("Type 1 Error (False Positive Rate): ", type_1_error, "\n")
type_2_error <- mean(simulated_diffs < critical_value)
cat("Type 2 Error (False Negative Rate): ", type_2_error, "\n")
mse_null <- mean((perm_diffs - observed_diff)^2)
cat("MSE for the null hypothesis: ", mse_null, "\n")
mse_alt <- mean((simulated_diffs - observed_diff)^2)
cat("MSE for the alternative hypothesis: ", mse_alt, "\n")
bias_null <- mean(perm_diffs) - observed_diff
cat("Bias for the null hypothesis: ", bias_null, "\n")
bias_alt <- mean(simulated_diffs) - observed_diff
cat("Bias for the alternative hypothesis: ", bias_alt, "\n")


## Basic Population Density Analysis

state_data <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", 
            "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", 
            "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", 
            "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
            "VT", "VA", "WA", "WV", "WI", "WY"),
  Density = c(101, 1.3, 65, 59, 250, 57, 747, 529, 422, 192, 
              223, 24, 226, 192, 57, 36, 115, 106, 45, 637, 
              898, 178, 72, 63, 89.52, 7.42, 25.22, 29, 157, 
              1263, 17, 415, 223, 11, 288, 59, 44, 290, 1060, 
              179, 12, 173, 117, 42, 70, 221, 118, 74, 109, 6)
)

state_data <- shooting_counts %>%
  left_join(state_data, by = c("State" = "State"))

low_threshold <- quantile(state_data$Density, 0.45, na.rm = TRUE)
high_threshold <- quantile(state_data$Density, 0.55, na.rm = TRUE)

state_data <- state_data %>%
  filter(Density <= low_threshold | Density >= high_threshold) %>%
  mutate(DensityGroup = ifelse(Density >= high_threshold, "High Density", "Low Density"))

observed_diff <- state_data %>%
  group_by(DensityGroup) %>%
  summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) |>
  summarise(diff = abs(mean_rate[1] - mean_rate[2])) |>
  pull(diff)

n_permutations <- 10000
perm_diffs <- replicate(n_permutations, {
  
  permuted_labels <- sample(state_data$DensityGroup)
  permuted_data <- state_data %>%
    mutate(PermDensityGroup = permuted_labels)
  
  perm_diff <- permuted_data %>%
    group_by(PermDensityGroup) %>%
    summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
    summarise(diff = mean_rate[1] - mean_rate[2]) %>%
    pull(diff)
  
  perm_diff
})


p_value <- mean(perm_diffs >= observed_diff)

ggplot() +
  geom_density(aes(x = perm_diffs), fill = "lightblue", color = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = observed_diff), color = "red", linetype = "solid") +
  labs(title = "Null Distribution of Mean Difference of Shooting Rates with Observed Difference",
       subtitle = paste("Observed Difference =", round(observed_diff, 4), 
                        "| p-value =", round(p_value, 4)),
       x = "Difference in Means",
       y = "Density",
       caption = "This figure shows the distribution of the difference of mean school shooting rates between states with higher 
       population densities and lower population densities if there was no true difference. Thus, the observed difference
       suggests that states with higher population densities tend to have higher shooting rates than states with lower population densities.") +
  theme_minimal() +   theme(plot.caption = element_text(hjust = 0.5))


## Basic Political Affiliation Analysis

state_vote <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  Vote = c("Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Red", 
           "Red", "Blue", "Red", "Blue", "Red", "Red", "Red", "Red", "Red", 
           "Blue", "Blue", "Blue", "Red", "Blue", "Red", "Red", "Red", "Red", 
           "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", 
           "Blue", "Red", "Blue", "Red", "Red", "Red", "Red", "Red", "Blue", 
           "Blue", "Blue", "Red", "Red", "Red")
)

state_vote <- shooting_counts %>%
  left_join(state_vote, by = c("State" = "State"))

observed_diff <- state_vote %>%
  group_by(Vote) %>%
  summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
  summarise(diff = mean_rate[2] - mean_rate[1]) %>%
  pull(diff)

n_permutations <- 10000
perm_diffs <- replicate(n_permutations, {
  
  permuted_labels <- sample(state_vote$Vote)
  permuted_data <- state_vote %>%
    mutate(PermDensityGroup = permuted_labels)
  
  perm_diff <- permuted_data %>%
    group_by(PermDensityGroup) %>%
    summarise(mean_rate = mean(ShootingRate, na.rm = TRUE)) %>%
    summarise(diff = mean_rate[1] - mean_rate[2]) %>%
    pull(diff)
  
  perm_diff
})

p_value <- mean(perm_diffs >= observed_diff)

ggplot() +
  geom_density(aes(x = perm_diffs), fill = "lightblue", color = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = observed_diff), color = "red", linetype = "solid") +
  labs(title = "Null Distribution of Mean Difference of Shooting Rates with Observed Difference",
       subtitle = paste("Observed Difference =", round(observed_diff, 4), 
                        "| p-value =", round(p_value, 4)),
       x = "Difference in Means",
       y = "Density",
       caption = "This figure shows the distribution of the difference of mean school shooting rates between Republican-leaning states and 
       Democrat-leaning states if there was no true difference. Thus, the observed difference suggests that Republican-leaning states tend 
       to have higher shooting rates than Democrat-leaning states.") +
  theme_minimal() +   theme(plot.caption = element_text(hjust = 0.5))

