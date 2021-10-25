is_within_confidence_interval = function(val, lb, ub) {
	return(val >= lb && val <= ub)
}

compute_coverage_rate = function(data, lb, ub) {
	n = length(data)
	coverage_values = sapply(1 : n, function(i) {
		is_within_confidence_interval(data[i], lb, ub)
	})
	return(sum(coverage_values) / n)
}
