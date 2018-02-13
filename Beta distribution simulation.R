#Adam Sokol
#create function 

LB_vec=NULL
UB_vec=NULL
mean_beta_vec=NULL
mean_conf=NULL

test_fun <- function(n,alpha,beta,NCP){
betaControl <- rbeta(n,alpha,beta,ncp=NCP)
mean_betas <- 1/(1+(beta/alpha))
beta_T <- t.test(betaControl, alternative = "two.sided", mu=0)

#beta.out <- vector()
#beta.out[1] <- beta_T$conf.int[1]
#beta.out[2] <- beta_T$conf.int[2]
#beta.out[3] <- beta_T$conf.int[3]

LB_vec<- c(LB_vec,beta_T$conf.int[1])
UB_vec <- c(UB_vec,beta_T$conf.int[2])
mean_beta_vec <- c(mean_beta_vec,mean_betas)
mean_conf<- ifelse(beta_T$conf.int[1]<mean_betas & beta_T$conf.int[2]>mean_betas,1,0)

}


test1 <- test_fun(100,1,2,0)
test_dat1 <-replicate(2,test_fun(100,1,2,0))

