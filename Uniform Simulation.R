## One sample t-test  Ginger Jiang *****
## Draw a random dataset from a uniform distribution ****
## You can change the sample sizes ****

## Set a seed first ***
set.seed(2618)

### Write a functiion to run the one-sample t-test, Here,x is the sample size,
#### c is confidence interval, s is the sided test: one sided or two sided test. 
uniform <- function(x,c,s){
  y <-runif(x)
  mean.unif = 1/2
  p <- t.test(y, mu=0, alternative=s, conf.level=c)
  ## Dr. Seaman add if else command 
  p <- c("Lower"=p$conf.int[1],"Upper"=p$conf.int[2], 
  "Rejection"=if ((mean.unif > p$conf.int[1]) & (mean.unif < p$conf.int[2])) 
  result <- 1 else result <- 0)
  return(p)
}

### replicate 20 times to draw a sample with sample size 100 from a uniform distribution 
set.seed(2618)
mydata <- replicate (1000,uniform(100,.95,"two.sided"))
reject.p <- rowSums(mydata)[3]/1000

## Save my results as a txt file
write.table(mydata, "C:/Users/Ginger Jiang/Desktop/mydata.txt", sep="\t")

# sample size: 10 Confidence level:.95, replication 9604
mydata.1 <- replicate (9604,uniform(10,.95,"two.sided"))
reject.p.1 <- rowSums(mydata.1)[3]/9604

# sample size: 30 Confidence level:.95, replication 9604
mydata.2 <- replicate (9604,uniform(30,.95,"two.sided"))
reject.p.2 <- rowSums(mydata.2)[3]/9604

# sample size: 50 Confidence level:.95, replication 9604
mydata.3 <- replicate (9604,uniform(50,.95,"two.sided"))
reject.p.3 <- rowSums(mydata.3)[3]/9604

# sample size: 100 Confidence level:.95, replication 9604
mydata.4 <- replicate (9604,uniform(100,.95,"two.sided"))
reject.p.4 <- rowSums(mydata.4)[3]/9604

# sample size: 10 Confidence level:.90, replication 9604
mydata.5 <- replicate (9604,uniform(10,.90,"two.sided"))
reject.p.5<- rowSums(mydata.5)[3]/9604

# sample size: 30 Confidence level:.90, replication 9604
mydata.6 <- replicate (9604,uniform(30,.90,"two.sided"))
reject.p.6<- rowSums(mydata.6)[3]/9604

# sample size: 50 Confidence level:.90, replication 9604
mydata.7 <- replicate (9604,uniform(50,.90,"two.sided"))
reject.p.7<- rowSums(mydata.7)[3]/9604

# sample size: 100 Confidence level:.90, replication 9604
mydata.8 <- replicate (9604,uniform(100,.90,"two.sided"))
reject.p.8<- rowSums(mydata.8)[3]/9604

reject.p.1
reject.p.2
reject.p.3
reject.p.4
reject.p.5
reject.p.6
reject.p.7
reject.p.8












