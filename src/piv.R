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
  # est_var = (n - 1) * mean((jack - t_bar)^2)
  # # Calculate the 95th percentile confidence interval for the Jackknife estimator
  # ci_lb = t_bar - (1.96 * est_var)
  # ci_ub = t_bar + (1.96 * est_var)
  # # Calulate the coverage rate for the jackknife confidence interval
  # coverage_rate = compute_coverage_rate(data, ci_lb, ci_ub)
  # # Return a vector of calculated values
  # return(c(
  #   est_sd, 
  #   est_bias, 
  #   est_se, 
  #   ci_lb, 
  #   ci_ub,
  #   coverage_rate
  # ))

