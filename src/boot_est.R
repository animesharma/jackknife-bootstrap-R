source("./coverage.R")
source("./piv.R")
boot_piv_operations = function(data, n, stat_func) {
	sampled_data = sample(data, n, replace = TRUE)
	data_stat = stat_func(data)
	sample_stat = stat_func(sampled_data)
	jack_sd = pivotal_conf_intv_estimator(data)
	boot_val = (sample_stat - data_stat) / (jack_sd / sqrt(n))
	return(c(data_stat, sample_stat, jack_sd, boot_val))
}

boot_piv_estimator=function(data, stat_func = sd, expected_input_mean, n_boot = 10000){
	n = length(data)
	boot = sapply(1 : n_boot, function(i) {
		boot_piv_operations(data, n, stat_func)
	})
	boot_df = as.data.frame(t(boot))
	colnames(boot_df) = c("DataSD", "SampleDataSD", "JackknifeSD", "Bootvec")
	lq = quantile(boot_df$Bootvec, .025)
	uq = quantile(boot_df$Bootvec, .975)
	data_sd_mean = mean(boot_df$DataSD)
	jackknife_sd_mean = mean(boot_df$JackknifeSD)
	Q5_boot_piv_conf = c(
		data_sd_mean - (jackknife_sd_mean / sqrt(n)) * uq, 
		data_sd_mean - (jackknife_sd_mean / sqrt(n)) * lq
	)
	boot_piv_coverage_rate = compute_coverage_rate(expected_input_mean, Q5_boot_piv_conf[1], Q5_boot_piv_conf[2])
	return(c(
			Q5_boot_piv_conf,
			boot_piv_coverage_rate
		))
}
