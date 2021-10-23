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
	#bookversion
	lq=quantile(bootstrap_var,.025)
	uq=quantile(bootstrap_var,.975)
	boot_piv_conf=(2*samle_mean-uq,2*sample_mean-lq)
	boot_norm_conf=(sample_mean-2*bootstrap_se,sample_mean+2*bootstrap_se)
	boot_per_conf=(lq,uq)
	#professorversion(no percentile)
        #here professor's piv is sample_mean-(sample_sd/sqrt(n))*uq, but according to the book,I think it shoud be 2*sample
	boot_piv_conf=(2*sample_mean-(sample_sd/sqrt(n))*uq,2*sample_mean-(sample_sd/sqrt(n))*lq)
	boot_norm_conf=(sample_mean-(sample_sd/sqrt(n))*qnorm(0.975),sample_mean+(sample_sd/sqrt(n))*qnorm(0.975))
	#my guess
	boot_per_conf=((sample_sd/sqrt(n))*uq,(sample_sd/sqrt(n))*lq)
}

print(bootstrap_estimator(1:10))
