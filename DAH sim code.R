#Jul 20 2024 simulations to study optimal analysis of days alive at home outcome

# Function to check for and install required packages
check_and_install <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
  invisible(lapply(packages, library, character.only = TRUE))
}

# List of required packages
required_packages <- c("ggplot2", "dplyr", "gtsummary")

# Check for and install required packages
check_and_install(required_packages)

# Clear the environment
rm(list = ls())

# Set seed for reproducibility
set.seed(42)

# Function to simulate DAH30 data with baseline covariate
simulate_dah30 <- function(n, prob_zero, shape, rate, max_days = 30, covariate_effect = 0.1) {
  # Generate zero-inflated data
  n_zero <- rbinom(1, n, prob_zero)
  n_gamma <- n - n_zero
  
  # Generate gamma distribution
  gamma_data <- rgamma(n_gamma, shape = shape, rate = rate)
  
  # Scale to 0-30 range
  scaled_data <- gamma_data * (max_days / max(gamma_data))
  
  # Combine zero-inflated and gamma data
  combined_data <- c(rep(0, n_zero), scaled_data)
  
  # Apply transformation: abs(x - 30)
  final_data <- abs(combined_data - max_days)
  
  # Round to integers
  final_data <- round(final_data)
  
  # Generate baseline covariate (e.g., age or health score)
  covariate <- rnorm(n, mean = 50, sd = 10)
  
  # Adjust final_data based on the covariate and round to keep it discrete
  final_data <- round(final_data + covariate_effect * (covariate - mean(covariate)))
  final_data[final_data > max_days] <- max_days  # Cap at max_days
  final_data[final_data < 0] <- 0  # Cap at 0 days
  
  return(list(data = final_data, covariate = covariate))
}

# Parameters
n_patients <- 1000
prob_zero <- 0.7  # Probability of 0 DAH30
shape <- 1      # Shape parameter for gamma distribution
rate <- 1        # Rate parameter for gamma distribution is not important here because of scaling that happens later
covariate_effect <- 0.1  # Effect size of the baseline covariate

# Simulate data for control group
control <- simulate_dah30(n_patients / 2, prob_zero, shape, rate, covariate_effect = covariate_effect)
dah30_data_control <- control$data
covariate_control <- control$covariate

# Simulate data for treatment group with 10% improvement
treatment <- simulate_dah30(n_patients / 2, prob_zero, shape, rate, covariate_effect = covariate_effect)
dah30_data_treatment <- round(treatment$data * 1.05)
dah30_data_treatment[dah30_data_treatment > 30] <- 30  # Cap at 30 days
covariate_treatment <- treatment$covariate

# Combine data
dah30_data <- c(dah30_data_control, dah30_data_treatment)
covariate <- c(covariate_control, covariate_treatment)
group <- c(rep("Control", n_patients / 2), rep("Treatment", n_patients / 2))

# Create a data frame
df <- data.frame(DAH30 = dah30_data, Group = factor(group, levels = c("Control", "Treatment")), Covariate = covariate)

# Plot
ggplot(df, aes(x = DAH30, fill = Group)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(-1, 31)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Simulated Distribution of Days Alive and at Home (DAH30)",
       x = "Days alive and at home during 30 days after surgery (DAH30)",
       y = "Number of patients",
       fill = "Group") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Summary statistics
summary(df)
print(table(df$DAH30, df$Group))

# Create a formatted summary table using gtsummary
summary_table_gtsummary <- df %>%
  select(Group, DAH30, Covariate) %>%
  tbl_summary(
    by = Group,
    type = list(DAH30 ~ 'continuous2', Covariate ~ 'continuous2'),
    statistic = list(
      DAH30 ~ c("mean" = "{mean}", "median" = "{median}", "25th, 75th percentiles" = "{p25}, {p75}"),
      Covariate ~ c("mean" = "{mean}", "median" = "{median}", "25th, 75th percentiles" = "{p25}, {p75}")
    ),
    digits = all_continuous() ~ 2,
    label = list(DAH30 = "Days Alive and at Home (DAH30)", Covariate = "Baseline Covariate")
  ) %>%
  add_p(test = everything() ~ "wilcox.test") %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Group**")

# Print formatted summary table
summary_table_gtsummary