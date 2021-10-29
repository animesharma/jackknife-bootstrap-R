### Bootstrap
BootBias_sd=function(inp,nsim=1000)
{
	len_inp=length(inp)
	sd_inp=sd(inp)
	bootvec=NULL

	for(i in 1:nsim){
		samp_vec=sample(inp,replace=T)
		n_samp=length(samp_vec)
		sd_samp=sd(samp_vec)

		bootvec = c(bootvec,sd_samp/n_samp)
	}
	final_bias_boot=sd_inp/len_inp-mean(bootvec) 

	return(final_bias_boot)
}


### Jackknife

jackknife_sd=function(inp)
{
	len_inp=length(inp)
	sd_inp=sd(inp)
	jkvec=NULL

	for(i in 1:len_inp){
		samp_vec=inp[-i]
		n_samp=length(samp_vec)
		sd_samp=sd(samp_vec)
		
		#Calculating pseudo values
		jk = (sd_samp/n_samp)
		
		jkvec = c(jkvec,jk)
	}
	final_bias_jk=(len_inp-1)*(sd_inp/len_inp-mean(jkvec)) ###Jackknife estimate

	return(final_bias_jk)
}

inp = rnorm(100)

cat("Estimated Bootstrap Standard Deviation Bias: ", BootBias_sd(inp))
cat("Estimated Jackknife Standard Deviation Bias: ", jackknife_sd(inp))
