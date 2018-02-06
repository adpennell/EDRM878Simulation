
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

myresult <- MonteF(20,2,10,.95)

