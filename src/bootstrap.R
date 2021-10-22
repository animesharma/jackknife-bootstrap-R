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
	boot = sapply(1:n_boot, bootstrap_operations, data, n, data_mean, stat_func)
	boot_df = as.data.frame(t(boot))
	colnames(boot_df) = c("BootVal", "BootBiasVal", "SampleSD", "SampleStat")
	print(head(boot_df))
	est_bias = mean(boot_df$BootBiasVal)
	est_sd = mean(boot_df$SampleSD)
	est_stat = mean(boot_df$SampleStat)
	bootstrap_var = mean(sapply(1 : n_boot, function(i) (boot_df$SampleStat[i] - est_stat)^2))
	bootstrap_se = sqrt(bootstrap_var)
}

print(bootstrap_estimator(1:10))
