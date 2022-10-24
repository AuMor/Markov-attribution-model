# MARKOV CHAIN ATTRIBUTION MODEL
library(ChannelAttribution)
data(PathData)
View(Data)


dat = data.frame(path = Data$path,
                 conv = Data$total_conversions,
                 conv_null = Data$total_null,
                 revenue = Data$total_conversion_value) 

# Applying the Markov chain attribution model
data_driven = markov_model(dat, 
                           var_path = "path", 
                           var_conv = "conv", 
                           var_null = "conv_null", 
                           order =1,
                           out_more=TRUE)

data_driven$removal_effects




# -----------------------------------------
# ROBUSTNESS: BOOTSTRAP ON REMOVAL EFFECTS
# How to measure the robustness of the Markov attribution modeling?
# Try to use bootstrap: it is a resampling method with replacement from a single original sample
# On each bootstrapped sample the removal effect of each channel is computed
# If the model is robust, the removal effects should be quite stable and they


library(boot)


removal_effect_ord1 = function(data,i){
  dat = data[i,]
  mod = markov_model(dat,
                     var_path = "path",
                     var_conv = "conv",
                     var_null = "conv_null",
                     order=1,
                     out_more = TRUE)  #if TRUE returns the transition probabilities between channels and removal effects.
  return(c(mod$removal_effects$removal_effects[1],
           mod$removal_effects$removal_effects[2],
           mod$removal_effects$removal_effects[3],
           mod$removal_effects$removal_effects[4],
           mod$removal_effects$removal_effects[5],
           mod$removal_effects$removal_effects[6],
           mod$removal_effects$removal_effects[7],
           mod$removal_effects$removal_effects[8],
           mod$removal_effects$removal_effects[9],
           mod$removal_effects$removal_effects[10],
           mod$removal_effects$removal_effects[11],
           mod$removal_effects$removal_effects[12]))
}


# Generating 2000 bootstrapped samples with replacement from dat using the "boot" function
bootstrap_test <- boot(dat,removal_effect_ord1, R=2000) #2000
                           # function that returns the statistics to be bootstrapped
bootstrap_test
bootstrap_test$t0 # original removal effects


# TESTING NORMALITY OF THE RESULTS
# The bootstrap distribution should appear to be normal
# If the bootstrap distribution is non-normal, you cannot trust the results


# Channel 1
hist(bootstrap_test$t[,1], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 1')
qqnorm(bootstrap_test$t[,1])
first = bootstrap_test$t[,1]
# The results follow a normal distribution -> OK 

# However the standard error is high (0.124)
# And the confidence interval is large
boot.ci(bootstrap_test, type='norm', index = 1)


# Channel 2
hist(bootstrap_test$t[,2], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 2')
qqnorm(bootstrap_test$t[,2])


# Channel 3
hist(bootstrap_test$t[,3], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 3')
qqnorm(bootstrap_test$t[,3])


# Channel 4
hist(bootstrap_test$t[,4], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 4')
qqnorm(bootstrap_test$t[,4])
qqline(bootstrap_test$t[,5])

# Channel 5
hist(bootstrap_test$t[,5], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 5')
qqnorm(bootstrap_test$t[,5])
qqline(bootstrap_test$t[,5])


# Channel 6
hist(bootstrap_test$t[,6], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 6')
qqnorm(bootstrap_test$t[,6])
qqline(bootstrap_test$t[,6])


# Channel 7
hist(bootstrap_test$t[,7], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 7')
qqnorm(bootstrap_test$t[,7])
qqline(bootstrap_test$t[,7])


# Channel 8
hist(bootstrap_test$t[,8], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 8')
qqnorm(bootstrap_test$t[,8])
qqline(bootstrap_test$t[,8])

# Channel 9
hist(bootstrap_test$t[,9], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 9')
qqnorm(bootstrap_test$t[,9])
qqline(bootstrap_test$t[,9])


# Channel 10
hist(bootstrap_test$t[,10], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 10')
qqnorm(bootstrap_test$t[,10])
qqline(bootstrap_test$t[,10])


# Channel 11
hist(bootstrap_test$t[,11], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 11')
qqnorm(bootstrap_test$t[,11])
qqline(bootstrap_test$t[,11])


# Channel 12
hist(bootstrap_test$t[,12], freq=F, breaks=15, main ='Bootstrap distribution of RE channel 12')
qqnorm(bootstrap_test$t[,12])
qqline(bootstrap_test$t[,12])




# Mean
boot_mean= c()
for (i in c(1:dim(bootstrap_test$t)[2])) {
  boot_mean[i] = mean(bootstrap_test$t[,i])
}
boot_mean


