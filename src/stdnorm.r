std_norm_dist = function(data) {
	# Calculate Sample mean and Sample standard deviation from the data
	sample_mean = mean(data)
	sample_sd = sd(data)
	# Calculate the 95th percentile confidence interval for standard normal distribution
	lb = sample_mean - (1.96 * sample_sd)
	ub = sample_mean + (1.96 * sample_sd)
	return (c(lb, ub))
}
