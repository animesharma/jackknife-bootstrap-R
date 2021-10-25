bootstrap_operations <- function(i, data, n, data_mean, stat_func) {
  sampled_data <- sample(data, n, replace = TRUE)
  sample_mean <- mean(sampled_data)
  sample_stat <- stat_func(sampled_data)
  sample_sd <- sd(sampled_data)
  boot_val <- (sample_mean - data_mean) / (sample_sd / sqrt(n))
  boot_bias_val <- sample_mean - data_mean
  
  return (
    c(
      boot_val = boot_val,
      boot_bias_val = boot_bias_val,
      sample_sd = sample_sd,
      sample_stat = sample_stat
    )
  )
}

bootstrap_estimator <-
  function(data,
           stat_func = sd,
           n_boot = 10000) {
    n <- length(data)
    data_mean <- mean(data)
    data_sd <- sd(data)
    stat <- stat_func(data)
    
    # Calculate Bootstrap Values
    boot <- data.frame(t(sapply(1:n_boot, bootstrap_operations, data, n, data_mean, stat_func)))
    
    # Estimate Standard Deviation, Bias and Statistic
    est_sd <- mean(boot$sample_sd)
    est_bias <- mean(boot$boot_bias_val)
    est_stat <- mean(boot$sample_stat)

    # Calculate Bootstrap Variance and Standard Error
    bootstrap_var <- mean(sapply(1:n_boot, function(i)
      (boot$sample_stat[i] - est_stat) ^ 2))
    bootstrap_se <- sqrt(bootstrap_var)
    
    lq <- quantile(boot$boot_val, .025)
    uq <- quantile(boot$boot_valr, .975)
    
    boot_norm_conf <- c(data_mean - 1.96 * bootstrap_se,
                        data_mean + 1.96 * bootstrap_se)
    
    boot_piv_conf <- c(data_mean - (data_sd / sqrt(n)) * uq,
                       data_mean - (data_sd / sqrt(n)) * lq)
    
    #boot_norm_conf <- c(
    #	data_mean - (data_sd / sqrt(n)) * qnorm(0.975),
    #	data_mean + (data_sd / sqrt(n)) * qnorm(0.975)
    #)
    
    boot_per_conf <- c(lq, uq)
    
    return (
      list(
        bootstrap_se = bootstrap_se,
        est_bias = est_bias,
        boot_piv_conf = boot_piv_conf,
        boot_norm_conf = boot_norm_conf,
        boot_per_conf = boot_per_conf
      )
    )
  }
