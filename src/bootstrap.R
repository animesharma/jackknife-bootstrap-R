bootstrap_operations = function(i, data, n, data_mean) {
    sampled_data = sample(data, n, replace = TRUE)
    sample_mean = mean(sampled_data)
    sample_sd = sd(sampled_data)
    boot_val = (sample_mean - data_mean) / (sample_sd / sqrt(n))
    boot_bias_val = sample_mean - data_mean
    return(c(boot_val, boot_bias_val, sample_sd))
}

bootstrap_estimator = function(data, n_boot = 10000) {
    n = length(data)
    data_mean = mean(data)
    data_sd = sqrt(var(data))
    boot = sapply(1:n_boot, bootstrap_operations, data, n, data_mean)
    estimates = apply(boot, 1, mean)
    est_bias = estimates[1]
    est_sd = estimates[2]
    return(c(est_sd, est_bias))
}

print(bootstrap_estimator(1:10))
