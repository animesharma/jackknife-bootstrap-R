source("./coverage.R")

pivotal_conf_intv_estimator = function(data, stat_func = var) {
	n = length(data)
	t = stat_func(data)
	# Calculate the jackknife vector by resampling data with one value removed
	jack = sapply(1 : n, function(i) stat_func(data[-i]))
	# Calculate t_bar as the mean of the jackknife vector
	t_bar = mean(jack)
	# print(t_bar)
	# Calculate the estimated standard deviation, bias and standard error
	# est_sd = sd(jack)
	est_bias = (n - 1) * (t_bar - t) 
	est_pv = (n*(t)^2)-((n-1)*jack)
	est_sd_hat=sqrt(var(est_pv)/n)
	return(c(est_sd_hat))
}
