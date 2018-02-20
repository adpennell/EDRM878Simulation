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




