std_norm_dist = function(data) {
	# Calculate Sample mean and Sample standard deviation from the data
	sample_mean = mean(data)
	sample_sd = sd(data)
	# Calculate the 95th percentile confidence interval for standard normal distribution
	lb = sample_mean - (1.96 * sample_sd)
	ub = sample_mean + (1.96 * sample_sd)
	# Calulate the coverage rate for the confidence interval
	coverage_rate = compute_coverage_rate(data, lb, ub)
	return (c(lb, ub, coverage_rate))
}
