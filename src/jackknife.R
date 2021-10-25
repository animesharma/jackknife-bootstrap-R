jackknife_estimator <- function(data, stat_func = sd) {
  n <- length(data)
  t <- stat_func(data)
  
  # Calculate Jackknife Vector by Resampling Data with One Value Removed
  jack <- sapply(1:n, function(i)
    stat_func(data[-i]))
  
  # Calculate t_bar as Mean of Jackknife Vector
  t_bar <- mean(jack)
  
  # Estimate Bias, Standard Deviation and Standard Error
  est_bias <- (n - 1) * (t_bar - t)
  est_sd <- sd(jack)
  est_se <- sqrt((n - 1) * mean((jack - t_bar) ^ 2))
  
  # Calculate 95% Confidence Interval for the Jackknife Estimator
  ci_lb <- t_bar - (1.96 * est_se)
  ci_ub <- t_bar + (1.96 * est_se)
  
  # Return Results
  return (list(
    est_bias = est_bias,
    est_sd = est_sd,
    est_se = est_se,
    ci_lb = ci_lb,
    ci_ub = ci_ub
  ))
}
