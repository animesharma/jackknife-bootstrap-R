# Import Bootstrap and Jackknife Estimators and Standard Normal Distribution
source("./bootstrap.R")
source("./jackknife.R")
source("./stdnorm.R")

# Generate Random Data from a Log Normal Distribution
generate_rlnorm <- function(num_samples,
                            mean = 0,
                            sd = 1) {
  # Ensure Values Passed to rlnorm Actually Have the Expected Mean and
  # Standard Deviation
  location <- log(mean ^ 2 / sqrt(sd ^ 2 + mean ^ 2))
  shape <- sqrt(log(1 + (sd ^ 2 / mean ^ 2)))
  
  return (rlnorm(num_samples, location, shape))
}

# Simulate Bootstrapping
simulate_resampling <- function(data, num_simulations = 1000) {
  for (i in 1:num_simulations)
    bootstrap_estimator(data, mean, 10000)
}

# Number of Simulations
num_simulations <- 1000

# Parameters Mean and Standard Deviation for generate_rlnorm Function
test_vector_mean <- 10
test_vector_sd <- 50

# Simulate for Sample Sizes of 10, 30 and 100
for (num_samples in c(10, 30, 100)) {
  data <-
    generate_rlnorm(num_samples, test_vector_mean, test_vector_sd)
  
  jackknife_result <- jackknife_estimator(data, mean)
  
  print(jackknife_result)
  
  simulate_resampling(data, num_simulations)
}
