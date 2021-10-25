std_norm_dist <- function(data) {
  # Calculate Sample Mean and Sample Standard Deviation from the Data
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  
  # Calculate 95% Confidence Interval for the Standard Normal Distribution
  # 95% of the Area Under a Normal Distribution Lies Within Roughly 1.96
  # Standard Deviations of the Mean
  ci_lb <- sample_mean - (1.96 * sample_sd)
  ci_ub <- sample_mean + (1.96 * sample_sd)
  
  return (list(ci_lb = ci_lb, ci_ub = ci_ub))
}
