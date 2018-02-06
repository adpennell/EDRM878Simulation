betaControl <- rbeta(100,1,2,ncp=0)
mean_beta <- mean(betaControl)
#create function 


x=2000 #How many times it will run
for(i in 1:x){
betaControl <- rbeta(100,1,,ncp=0)
beta_T <- t.test(betaControl, alternative = "two.sided", mu=0)

LB<- c(Wilks.Lamb.vec,test1$wilks)
}