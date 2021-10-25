# Import the bootstrap and jackknife estimators and standard normal distribution
source("./bootstrap.R")
source("./jackknife.R")
source("./stdnorm.R")
# Number of simulations
simulations = 1000

# Function to calculate requisite Jackknife and bootstrap values
simulate_resampling = function(num_samples, sample_mean, sample_sd) {
	# Generate a matrix of test vectors with the function parameters using rlnorm
	test_vectors = sapply(1:simulations, function(i) {
		rlnorm(num_samples, sample_mean, sample_sd)
	})
	# Compute the jackknife estimate and save it as a dataframe
	jackknife_result = sapply(1:simulations, function(i) {
		jackknife_estimator(data = test_vectors[,i], stat_func = mean)
	})
	jackknife_df = as.data.frame(t(jackknife_result))
	colnames(jackknife_df) = c(
		"Jack_Estimated_SD",
		"Jack_Estimated_Bias",
		"Jack_Estimated_SE",
		"Jack_CI_LB",
		"Jack_CI_UB"
	)

	# Compute the standard normal distribution and save it as a dataframe
	std_norm_result = sapply(1:simulations, function(i) {
		std_norm_dist(data = test_vectors[,i])
	})
	std_norm_df = as.data.frame(t(std_norm_result))
	colnames(std_norm_df) = c(
		"Standard_Normal_LB",
		"Standard_Normal_UB"
	)
	# Compute the bootstrap estimate and save it as a dataframe
	bootstrap_result = sapply(1:simulations, function(i) {
		bootstrap_estimator(data = test_vectors[,i], stat_func = mean)
	})
	bootstrap_df = as.data.frame(t(bootstrap_result))
	colnames(bootstrap_df) = c(
		"Boot_SE",
		"Boot_Estimated_Bias",
		"Boot_Pivotal_CI_LB_Book",
		"Boot_Pivotal_CI_UB_Book",
		"Boot_Normal_CI_LB_Book",
		"Boot_Normal_CI_UB_Book",
		"Boot_Percentile_CI_LB_Book",
		"Boot_Percentile_CI_UB_Book",
		"Boot_Pivotal_CI_LB_Prof",
		"Boot_Pivotal_CI_UB_Prof",
		"Boot_Normal_CI_LB_Prof",
		"Boot_Normal_CI_UB_Prof",
		"Boot_Percentile_CI_LB_Prof",
		"Boot_Percentile_CI_UB_Prof"
	)
	# Aggregate the jackknife, bootstrap and standard normal dataframes into one dataframe
	aggregate_df = cbind(jackknife_df, bootstrap_df,std_norm_df)
	# Print head of aggregated dataframe
	print(head(aggregate_df))
}

# Parameters passed to rlnorm
test_vector_mean = 10
test_vector_sd = 50
# Ensure that the values passed to rlnorm will actually have the expected mean and sd
location = log(test_vector_mean^2 / sqrt(test_vector_sd^2 + test_vector_mean^2))
shape = sqrt(log(1 + (test_vector_sd^2 / test_vector_mean^2)))

# 10 samples
num_samples = 10
simulate_resampling(num_samples, location, shape)

# 30 samples
num_samples = 30
simulate_resampling(num_samples, location, shape)

# 100 samples
num_samples = 100
simulate_resampling(num_samples, location, shape)
