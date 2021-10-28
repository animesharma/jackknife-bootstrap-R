source("./coverage.R")

bootstrap_operations = function(data, n, data_mean, stat_func) {
	# Sample values with replacement
	sampled_data = sample(data, n, replace = TRUE)
	# Calulcate sample mean, standard deviation and statistic
	sample_mean = mean(sampled_data)
	sample_stat = stat_func(sampled_data)
	sample_sd = sd(sampled_data)
	# Calculate the Pivotal Boot Value
	pivotal_boot_val = (sample_mean - data_mean) / (sample_sd / sqrt(n))
	# Calculate the Boot Bias Value
	boot_bias_val = sample_mean - data_mean
	# Return a vector of calculated values
	return(c(sample_mean, pivotal_boot_val, boot_bias_val, sample_sd, sample_stat))
}

bootstrap_estimator = function(data, stat_func = sd, expected_input_mean, n_boot = 10000) {
	n = length(data)
	data_mean = mean(data)
	data_sd = sd(data)
	stat = stat_func(data)
	# Calculate bootstrap values and store them in a Bootstrap Dataframe
	boot = sapply(1 : n_boot, function(i) {
		bootstrap_operations(data, n, data_mean, stat_func)
	})
	boot_df = as.data.frame(t(boot))
	colnames(boot_df) = c("BootVector", "PivotalBootVector", "BootBiasVector", "SampleSD", "SampleStat")

	# Calculate the estimated standard deviation, bias and statistic
	est_sd = mean(boot_df$SampleSD)
	est_bias = mean(boot_df$BootBiasVector)
	est_stat = mean(boot_df$SampleStat)

	# Calculate bootstrap variance and standard error
	bootstrap_var = mean(sapply(1 : n_boot, function(i) (boot_df$SampleStat[i] - est_stat)^2))
	bootstrap_se = sqrt(bootstrap_var)
	
	# Calulate the lower and upper quantiles for the 95th percentile confidence interval
	lq = quantile(boot_df$BootVector, .025)
	uq = quantile(boot_df$BootVector, .975)

	# Calulate the lower and upper pivotal quantiles for the 95th percentile confidence interval
	pivotal_lq = quantile(boot_df$PivotalBootVector, .025)
	pivotal_uq = quantile(boot_df$PivotalBootVector, .975)

	# Bootstrap normal confidence interval
	boot_norm_conf = c(
		data_mean - 1.96 * bootstrap_se, 
		data_mean + 1.96 * bootstrap_se
	)
	# Coverage rate for the confidence interval
	boot_norm_coverage_rate = compute_coverage_rate(expected_input_mean, boot_norm_conf[1], boot_norm_conf[2])

	# Bootstrap pivotal confidence interval
	boot_piv_conf = c(
		data_mean - (data_sd / sqrt(n)) * pivotal_uq, 
		data_mean - (data_sd / sqrt(n)) * pivotal_lq
	)
	# Coverage rate for the confidence interval
	boot_piv_coverage_rate = compute_coverage_rate(expected_input_mean, boot_piv_conf[1], boot_piv_conf[2])

	# Bootstrap percentile confidence interval
	boot_per_conf = c(lq, uq)
	# Coverage rate for the confidence interval
	boot_per_coverage_rate = compute_coverage_rate(expected_input_mean, boot_per_conf[1], boot_per_conf[2])
	
	# Return a vector of calculated values
	return(c(
		bootstrap_se,
		est_sd, 
		est_bias, 
		boot_piv_conf,
		boot_piv_coverage_rate,
		boot_norm_conf,
		boot_norm_coverage_rate,
		boot_per_conf,
		boot_per_coverage_rate
	))
}
