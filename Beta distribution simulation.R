#Adam Sokol
#create function 

test_fun <- function(n,alpha,beta,NCP){
betaControl <- rbeta(n,alpha,beta,ncp=NCP)
mean_betas <- 1/(1+(beta/alpha))
beta_T <- t.test(betaControl, alternative = "two.sided", mu=0)
mean_conf<- ifelse(beta_T$conf.int[1]<mean_betas & beta_T$conf.int[2]>mean_betas,1,0)

test_fun.out <- vector()
test_fun.out[1] <- beta_T$conf.int[1] 
test_fun.out[2] <- beta_T$conf.int[2]
test_fun.out[3] <- mean_betas
test_fun.out[4] <- mean_conf

return(test_fun.out)
}



test_dat1 <-replicate(1000000,test_fun(100,1,2,0))

hit_rate <- sum(test_dat1[4,])/1000000
