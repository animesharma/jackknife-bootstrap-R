source("coverage.R")
source("piv.R")
bootstrap_operations = function(i, data, n, stat_func) {
  sampled_data = sample(data, n, replace = TRUE)
  data_stat = stat_func(data)
  sample_stat = stat_func(sampled_data)
  jack_sd = pivotal_conf_intv_estimator(data)
  boot_val = (sample_stat - data_stat) / (jack_sd / sqrt(n))
  return(c(data_stat,sample_stat,jack_sd,boot_val))
}

boot_piv_estimator=function(data, stat_func = sd, expected_input_mean, n_boot = 10000){
  n = length(data)
  boot = sapply(1 : n_boot, bootstrap_operations, data, n, stat_func)
  boot_df = as.data.frame(t(boot))
  colnames(boot_df) = c("DataSD", "SampleDataSD", "JackknifeSD", "Bootvec")
  lq = quantile(boot_df$Bootvec, .025)
  uq = quantile(boot_df$Bootvec, .975)
  boot_piv_conf = c(
    boot_df$DataSD - (boot_df$JackknifeSD / sqrt(n)) * uq, 
    boot_df$DataSD - (boot_df$JackknifeSD / sqrt(n)) * lq
  )
  boot_piv_coverage_rate = compute_coverage_rate(expected_input_mean, boot_piv_conf[1], boot_piv_conf[2])
  return(c(boot_piv_conf,
           boot_piv_coverage_rate))
}