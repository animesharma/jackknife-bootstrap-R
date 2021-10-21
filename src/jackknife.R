jackknife_estimator = function(data, stat_func=sd) {
	n = length(data)
    t = stat_func(data)
    jack = sapply(1 : n, function(i) stat_func(data[-i]))
    t_bar = mean(jack)
    est_sd = sd(jack)
    est_bias = (n - 1) * (t_bar - t) 
    est_se = sqrt((n - 1) * mean((jack - t_bar)^2))
    return(c(est_sd, est_bias, est_se))
}

temp = 1:10
print(jackknife_estimator(temp, mean))
