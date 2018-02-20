
###############################################
#
# Monte Carlo Simulation of the t test coverage
# of the mean when samples are from the F
# distribution.
#
# Michael A. Seaman
# University of South Carolina
#
###############################################

#I'm setting up some parameters that I can easily change,
#so I'm putting those right at the top.

#Sample size parameter

sample.size <- 20

#Degrees of freedom for the F distribution

degrees.1 <- 2
degrees.2 <- 10

#Confidence level

conf.level <- .95

#Number of repetitions

num.rep <- 1000000

#Function for a single replication

MonteF <- function(n, df1, df2, ci.level){

  mean.f = df2/(df2-2)
  f.sample <- rf(n, df1, df2)
  t.result <- t.test(f.sample, conf.level = ci.level)
  ci.lb <- t.result$conf.int[1]
  ci.ub <- t.result$conf.int[2]
  
  if ((mean.f > ci.lb) & (mean.f < ci.ub)) result <- 1 else result <- 0
  
  MonteF.out <- vector()
  
  MonteF.out[1] <- ci.lb
  MonteF.out[2] <- ci.ub
  MonteF.out[3] <- result
  
  return(MonteF.out)
  
}

F.results <- replicate(num.rep,
                       MonteF(sample.size,
                              degrees.1,
                              degrees.2,
                              conf.level))

prop.coverage <- sum(F.results[3,])/num.rep
