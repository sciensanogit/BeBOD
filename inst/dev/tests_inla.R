library(BeBOD)
library(tidyverse)
library(INLA)

# Simulate a Poisson dataset
set.seed(123)
n <- 100
poisson_data <- data.frame(
  ID = 1:n,
  YEAR = sample(2000:2020, n, replace = TRUE),
  SEX = sample(c("M", "F"), n, replace = TRUE),
  AGEGR = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
  REGION = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  COUNT = rpois(n, lambda = 5)  # Poisson-distributed counts
)

head(poisson_data)

poisson_test <- select_best_inla_model(
  data = poisson_data, 
  model_list = list(
    "FIXED" = COUNT ~ YEAR + SEX + REGION,
    "INTERACTION" = COUNT ~ YEAR*SEX*REGION
  ), likelihood = "xpoisson", metric = "WAIC", verbose = FALSE
)

# Simulate a binary dataset for binomial models
set.seed(123)
n <- 100
binary_data <- data.frame(
  ID = 1:n,
  YEAR = sample(2000:2020, n, replace = TRUE),
  SEX = sample(c("M", "F"), n, replace = TRUE),
  AGEGR = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
  REGION = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  RESPONSE = rbinom(n, size = 1, prob = 0.3)  # Binary response (0/1)
)

head(binary_data)

binomial_test = select_best_inla_model(
  data = binary_data, 
  model_list = list(
    "FIXED" = RESPONSE ~ YEAR + SEX + REGION,
    "INTERACTION" = RESPONSE ~ YEAR*SEX*REGION
  ), likelihood = "binomial", metric = "WAIC", verbose = TRUE
)

# Simulate an aggregated binomial dataset with Ntrials
set.seed(123)
n <- 100
aggregated_binomial_data <- data.frame(
  ID = 1:n,
  YEAR = sample(2000:2020, n, replace = TRUE),
  SEX = sample(c("M", "F"), n, replace = TRUE),
  AGEGR = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
  REGION = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  SUCCESSES = rbinom(n, size = 10, prob = 0.3),  # Number of successes
  N = rep(10, n)  # Number of trials
)

head(aggregated_binomial_data)


aggregated_binomial_test = select_best_inla_model(
  data = aggregated_binomial_data, 
  model_list = list(
    "FIXED" = SUCCESSES ~ YEAR + SEX + REGION,
    "INTERACTION" = SUCCESSES ~ YEAR*SEX*REGION
  ), likelihood = "binomial", metric = "WAIC", verbose = FALSE, Ntrials = "N"
)

# Simulate a gamma dataset
set.seed(123)
n <- 100
aggregated_binomial_data <- data.frame(
  ID = 1:n,
  YEAR = sample(2000:2020, n, replace = TRUE),
  SEX = sample(c("M", "F"), n, replace = TRUE),
  AGEGR = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
  REGION = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  SUCCESSES = rbinom(n, size = 10, prob = 0.3),  # Number of successes
  N = rep(10, n)  # Number of trials
)

head(aggregated_binomial_data)


aggregated_binomial_test = select_best_inla_model(
  data = aggregated_binomial_data, 
  model_list = list(
    "FIXED" = SUCCESSES ~ YEAR + SEX + REGION,
    "INTERACTION" = SUCCESSES ~ YEAR*SEX*REGION
  ), likelihood = "binomial", metric = "WAIC", verbose = FALSE, Ntrials = "N"
)

# Simulate a Beta dataset
set.seed(123)
n <- 100
beta_data <- data.frame(
  ID = 1:n,
  YEAR = sample(2000:2020, n, replace = TRUE),
  SEX = sample(c("M", "F"), n, replace = TRUE),
  AGEGR = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
  REGION = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  PROPORTION = rbeta(n, shape1 = 2, shape2 = 5)  # Beta-distributed proportions
)

head(beta_data)

# Apply the select_best_inla_model function to the Beta data
beta_test <- select_best_inla_model(
  data = beta_data, 
  model_list = list(
    "FIXED" = PROPORTION ~ YEAR + SEX + REGION,
    "INTERACTION" = PROPORTION ~ YEAR*SEX*REGION
  ), likelihood = "beta", metric = "WAIC", verbose = TRUE
)

# Simulate a Gamma dataset
set.seed(123)
n <- 100
gamma_data <- data.frame(
  ID = 1:n,
  YEAR = sample(2000:2020, n, replace = TRUE),
  SEX = sample(c("M", "F"), n, replace = TRUE),
  AGEGR = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
  REGION = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  DURATION = rgamma(n, shape = 2, rate = 1)  # Gamma-distributed duration data
)

head(gamma_data)

# Apply the select_best_inla_model function to the Gamma data
gamma_test <- select_best_inla_model(
  data = gamma_data, 
  model_list = list(
    "FIXED" = DURATION ~ YEAR + SEX + REGION,
    "INTERACTION" = DURATION ~ YEAR*SEX*REGION
  ), likelihood = "gamma", metric = "WAIC", verbose = TRUE
)
