###############################################
#
# Monte Carlo Simulation of the t-test coverage
# of the mean when samples are from the chi-square
# distribution.
#
# Adam Pennell
# University of South Carolina
#
###############################################

# Set up some parameters that can easily be changed

# Arbitrary sample size for chi-square distribution
sample.size <- 80

# Arbitrary df for chi-square distribution
degrees.1 <- 50

# Set confidence interval
conf.level <- 0.95

# Number of repetitions
num.rep <- 100000

# Replication function 
Monte.chisq <- function(n, df1, ci.level){
  
  mean.chisq = df1
  chisq.sample <- rchisq(n, df1)
  t.result <- t.test(chisq.sample, conf.level = ci.level)
  ci.lb <- t.result$conf.int[1]
  ci.ub <- t.result$conf.int[2]
  
  if ((mean.chisq > ci.lb) & (mean.chisq < ci.ub)) result <- 1 else result <- 0
  
  Monte.chisq.out <- vector()
  
  Monte.chisq.out[1] <- ci.lb
  Monte.chisq.out[2] <- ci.ub
  Monte.chisq.out[3] <- result
  
  return(Monte.chisq.out)
  
}

Monte.chisq.results <- replicate(num.rep,
                           Monte.chisq(sample.size,
                                      degrees.1,
                                      conf.level))

prop.coverage <- sum(Monte.chisq.results[3,])/num.rep
