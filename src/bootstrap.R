bootstrap_operations = function(i, data, n, data_mean, stat_func) {
	sampled_data = sample(data, n, replace = TRUE)
	sample_mean = mean(sampled_data)
	sample_stat = stat_func(sampled_data)
	sample_sd = sd(sampled_data)
	boot_val = (sample_mean - data_mean) / (sample_sd / sqrt(n))
	boot_bias_val = sample_mean - data_mean
	return(c(boot_val, boot_bias_val, sample_sd, sample_stat))
}

bootstrap_estimator = function(data, stat_func = sd, n_boot = 10000) {
	n = length(data)
	data_mean = mean(data)
	data_sd = sd(data)
	stat = stat_func(data)
	# Calculate bootstrap values and store them in a Bootstrap Dataframe
	boot = sapply(1 : n_boot, bootstrap_operations, data, n, data_mean, stat_func)
	boot_df = as.data.frame(t(boot))
	colnames(boot_df) = c("BootVector", "BootBiasVector", "SampleSD", "SampleStat")
	# print(head(boot_df))
	# Calculate the estimated standard deviation, bias and statistic
	est_sd = mean(boot_df$SampleSD)
	est_bias = mean(boot_df$BootBiasVector)
	est_stat = mean(boot_df$SampleStat)
	# Calculate bootstrap variance and standard error
	bootstrap_var = mean(sapply(1 : n_boot, function(i) (boot_df$SampleStat[i] - est_stat)^2))
	bootstrap_se = sqrt(bootstrap_var)
	#bookversion
	lq = quantile(boot_df$BootVector, .025)
	uq = quantile(boot_df$BootVector, .975)
	boot_piv_conf = c(
		2 * data_mean - uq, 
		2 * data_mean - lq
	)
	boot_norm_conf = c(
		data_mean - 2 * bootstrap_se, 
		data_mean + 2 * bootstrap_se
	)
	boot_per_conf = c(lq, uq)
	#professorversion(no percentile)
	boot_piv_conf_2 = c(
		data_mean - (data_sd / sqrt(n)) * uq, 
		data_mean - (data_sd / sqrt(n)) * lq
	)
	boot_norm_conf_2 = c(
		data_mean - (data_sd / sqrt(n)) * qnorm(0.975), 
		data_mean + (data_sd / sqrt(n)) * qnorm(0.975)
	)
	#my guess
	boot_per_conf_2 = c(
		data_mean - (data_sd / sqrt(n)) * uq, 
		data_mean - (data_sd / sqrt(n)) * lq
	)
	return(c(
		bootstrap_se, 
		est_bias, 
		boot_piv_conf,
		boot_norm_conf,
		boot_per_conf,
		boot_piv_conf_2,
		boot_norm_conf_2,
		boot_per_conf_2
	))
}

