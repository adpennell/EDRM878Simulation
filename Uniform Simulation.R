## One sample t-test  Ginger Jiang *****
## Draw a random dataset from a distribution ****
## You can change the sample sizes ****
## Example: Chi-square distribution ***

## Set a seed first ***
set.seed(2618)

### Write a functiion to run the one-sample t-test
uniform <- function(x){
y <-runif(x)
p <- t.test(y, mu=0, alternative="two.sided", conf.level=.95)
p <- c("Lower"=p$conf.int[1],"Upper"=p$conf.int[2],p$estimate)
return(p)
}

### replicate 10 times to draw a sample with sample size 100 from a uniform distribution 
mydata <- replicate (10,uniform(100))

## Save my results as a txt file
write.table(mydata, "C:/Users/Ginger Jiang/Desktop/mydata.txt", sep="\t")

