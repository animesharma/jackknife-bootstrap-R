std_norm_dist = function(data, stat_func = sd, meann, std_dev){
  n = length(data)
  t = stat_func(data)
  # Calculate the jackknife vector by resampling data with one value removed
  std = sapply(1 : n, function(i) stat_func(data[i]))
  lb=meann-(1.96*std_dev)
  ub=meann+(1.96*std_dev)
  return (c(lb,ub))
}