# Import the bootstrap and jackknife estimators and standard normal distribution
source("./bootstrap.R")
source("./jackknife.R")
source("./stdnorm.R")
# Number of simulations
simulations = 100

# Function to calculate requisite Jackknife and bootstrap values
simulate_resampling = function(num_samples, sample_mean, sample_sd) {
	coverage_val = exp(sample_mean + ((sample_sd ^ 2) / 2))
	# Generate a matrix of test vectors with the function parameters using rlnorm
	test_vectors = sapply(1:simulations, function(i) {
		rlnorm(num_samples, sample_mean, sample_sd)
	})
	print(coverage_val)
	# Compute the jackknife estimate and save it as a dataframe
	jackknife_result = sapply(1:simulations, function(i) {
		jackknife_estimator(data = test_vectors[,i], stat_func = mean, coverage_val)
	})
	jackknife_df = as.data.frame(t(jackknife_result))
	colnames(jackknife_df) = c(
		"Jack_Estimated_SD",
		"Jack_Estimated_Bias",
		"Jack_Estimated_SE",
		"Jack_CI_LB",
		"Jack_CI_UB",
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
		"Boot_SD",
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
	# Print head of aggregated dataframe
	print(head(aggregate_df))
	return(aggregate_df)
}


# Ensure that the values passed to rlnorm will actually have the expected mean and sd
#location = log(test_vector_mean^2 / sqrt(test_vector_sd^2 + test_vector_mean^2))
#shape = sqrt(log(1 + (test_vector_sd^2 / test_vector_mean^2)))

####################################
# The results below need to be properly tabulated, I just used the print function to check the values.
# We can create a vector c(10, 30, 100) and then use apply to clear out some of the repeated code.
####################################

main = function(num_samples, expected_input_mean, expected_input_sd) {
	temp = simulate_resampling(num_samples, test_vector_mean, test_vector_sd)
	print(sum(temp$Jack_Coverage_Rate)/simulations)
	print(sum(temp$Boot_Pivotal_Coverage_Rate)/simulations)
	print(sum(temp$Boot_Normal_Coverage_Rate)/simulations)
	print(sum(temp$Boot_Percentile_Coverage_Rate)/simulations)
	print(sum(temp$Standard_Normal_Coverage_Rate)/simulations)
}

# Parameters passed to rlnorm
expected_input_mean = 0
expected_input_sd = 1

num_samples = c(10, 30, 100)
res = sapply(num_samples, function(i) {
	main(i, expected_input_mean, expected_input_sd)
})

