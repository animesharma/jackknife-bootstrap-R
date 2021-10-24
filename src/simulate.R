# Import the bootstrap and jackknife estimators
source("./bootstrap.R")
source("./jackknife.R")

simulations = 1000

simulate_resampling = function(test_vector) {
    jackknife_result = sapply(1 : simulations, jackknife_estimator, data = test_vector, stat_func = mean)
    jackknife_df = as.data.frame(t(jackknife_result))
    colnames(jackknife_df) = c(
        "Jack_Estimated_SD",
        "Jack_Estimated_Bias",
        "Jack_Estimated_SE",
        "Jack_CI_LB",
        "Jack_CI_UB"
    )
    bootstrap_result = sapply(1 : simulations, bootstrap_estimator, data = test_vector, stat_func = mean)
    bootstrap_df = as.data.frame(t(bootstrap_result))
    colnames(bootstrap_df) = c(
        "Boot_SE",
        "Boot_Estimated_Bias",
        "Boot_Pivotal_CI_LB_Book",
        "Boot_Pivotal_CI_UB_Book",
        "Boot_Normal_CI_LB_Book",
        "Boot_Normal_CI_UB_Book",
        "Boot_Percentile_CI_LB_Book",
        "Boot_Percentile_CI_UB_Book",
        "Boot_Pivotal_CI_LB_Prof",
        "Boot_Pivotal_CI_UB_Prof",
        "Boot_Normal_CI_LB_Prof",
        "Boot_Normal_CI_UB_Prof",
        "Boot_Percentile_CI_LB_Prof",
        "Boot_Percentile_CI_UB_Prof"
    )
    aggregate_df = cbind(jackknife_df, bootstrap_df)
    print(head(aggregate_df))
}

# 10 samples
test_vector = rlnorm(10, 100, 25)
simulate_resampling(test_vector)

# 30 samples
test_vector = rlnorm(30, 100, 25)
simulate_resampling(test_vector)

# 100 samples
test_vector = rlnorm(100, 100, 25)
simulate_resampling(test_vector)

#jackknife_result = jackknife_estimator(test_vector, mean)
#bootstrap_result = bootstrap_estimator(test_vector, mean)
#print("Jackknife: ", jackknife_result)
#print("Bootstrap: ", bootstrap_estimator)