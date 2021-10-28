# Import the bootstrap and jackknife estimators and standard normal distribution
source("./bootstrap.R")
source("./jackknife.R")
source("./stdnorm.R")

# Number of simulations
simulations = 1000

# Function to calculate requisite Jackknife, Standard Normal and Bootstrap values
simulate_resampling = function(num_samples, sample_mean, sample_sd) {

	# Calculate the coverage value to check coverage rate of confidence intervals
	coverage_val = exp(sample_mean + ((sample_sd ^ 2) / 2))

	# Generate a matrix of test vectors with the function parameters using rlnorm
	test_vectors = sapply(1:simulations, function(i) {
		rlnorm(num_samples, sample_mean, sample_sd)
	})

	# Compute the jackknife estimate and save it as a dataframe
	jackknife_result = sapply(1:simulations, function(i) {
		jackknife_estimator(data = test_vectors[,i], stat_func = mean, coverage_val)
	})
	jackknife_df = as.data.frame(t(jackknife_result))
	colnames(jackknife_df) = c(
		"Jack_Estimated_SD",
		"Jack_Estimated_Bias",
		"Jack_Estimated_SE",
		"Jack_CI_Lower_Bound",
		"Jack_CI_Upper_Bound",
		"Jack_Coverage_Rate"
	)

	# Compute the standard normal distribution and save it as a dataframe
	std_norm_result = sapply(1:simulations, function(i) {
		std_norm_dist(data = test_vectors[,i], coverage_val)
	})
	std_norm_df = as.data.frame(t(std_norm_result))
	colnames(std_norm_df) = c(
		"Standard_Normal_LB",
		"Standard_Normal_UB",
		"Standard_Normal_Coverage_Rate"
	)

	# Compute the bootstrap estimate and save it as a dataframe
	bootstrap_result = sapply(1:simulations, function(i) {
		bootstrap_estimator(data = test_vectors[,i], stat_func = mean, coverage_val)
	})
	bootstrap_df = as.data.frame(t(bootstrap_result))
	colnames(bootstrap_df) = c(
		"Boot_Estimated_SE",
		"Boot_Estimated_SD",
		"Boot_Estimated_Bias",
		"Boot_Pivotal_CI_LB",
		"Boot_Pivotal_CI_UB",
		"Boot_Pivotal_Coverage_Rate",
		"Boot_Normal_CI_LB",
		"Boot_Normal_CI_UB",
		"Boot_Normal_Coverage_Rate",
		"Boot_Percentile_CI_LB",
		"Boot_Percentile_CI_UB",
		"Boot_Percentile_Coverage_Rate"
	)

	# Aggregate the jackknife, bootstrap and standard normal dataframes into one dataframe
	aggregate_df = cbind(jackknife_df, bootstrap_df, std_norm_df)
	
	return(aggregate_df)
}

main = function(num_samples, expected_input_mean, expected_input_sd) {
	# Build aggregate dataframe
	result = simulate_resampling(num_samples, expected_input_mean, expected_input_sd)
	# Print output
	cat("\nSimulations: ", simulations, "\n")
	cat("Number of samples: ", num_samples, "\n")
	cat("\n\n*************************************************\n\n")
	cat("Estimated Standard Deviations:\n\n")
	print(result[1:10, c("Boot_Estimated_SD", "Jack_Estimated_SD")])
	cat("\n\n*************************************************\n\n")
	cat("Estimated Standard Errors:\n\n")
	print(result[1:10, c("Boot_Estimated_SE", "Jack_Estimated_SE")])
	cat("\n\n*************************************************\n\n")
	cat("Estimated Bias:\n\n")
	print(result[1:10, c("Boot_Estimated_Bias", "Jack_Estimated_Bias")])
	cat("\n\n*************************************************\n\n")
	cat("Jackknife Confidence Interval:\n\n")
	print(result[1:10, c("Jack_CI_Lower_Bound", "Jack_CI_Upper_Bound")])
	cat("\n\n*************************************************\n\n")
	cat("Bootstrap Pivotal Confidence Interval:\n\n")
	print(result[1:10, c("Boot_Pivotal_CI_LB", "Boot_Pivotal_CI_UB")])
	cat("\n\n*************************************************\n\n")
	cat("Bootstrap Normal Confidence Interval:\n\n")
	print(result[1:10, c("Boot_Normal_CI_LB", "Boot_Normal_CI_UB")])
	cat("\n\n*************************************************\n\n")
	cat("Bootstrap Percentile Confidence Interval:\n\n")
	print(result[1:10, c("Boot_Percentile_CI_LB", "Boot_Percentile_CI_UB")])
	cat("\n\n*************************************************\n\n")
	cat("Standard Normal Confidence Interval:\n\n")
	print(result[1:10, c("Standard_Normal_LB", "Standard_Normal_UB")])
	cat("\n\n*************************************************\n\n")
	cat("Coverage Rates:\n\n")
	cat("Jackknife Coverage Rate: ", sum(result$Jack_Coverage_Rate)/simulations, "\n")
	cat("Bootstrap Pivotal Coverage Rate: ", sum(result$Boot_Pivotal_Coverage_Rate)/simulations, "\n")
	cat("Bootstrap Normal Coverage Rate: ", sum(result$Boot_Normal_Coverage_Rate)/simulations, "\n")
	cat("Bootstrap Percentile Coverage Rate: ", sum(result$Boot_Percentile_Coverage_Rate)/simulations, "\n")
	cat("Standard Normal Coverage Rate: ", sum(result$Standard_Normal_Coverage_Rate)/simulations, "\n")
	cat("\n\n###################################################\n\n")
}

# Parameters passed to rlnorm
expected_input_mean = 0
expected_input_sd = 1

# Run simulations for test vector sizes 10, 30 and 100
num_samples = c(10, 30, 100)
res = sapply(num_samples, function(i) {
	main(i, expected_input_mean, expected_input_sd)
})
