#Brad Rogers
#Exponential Distribution T.test Function


s.size<-30
Lambda<-1
c.level<-.95
n.rep<-100

x<-replicate(n.rep,exp.sim(s.size,Lambda,c.level))
Percent<-(sum(x[3,])/n.rep)*100
Percent

?t.test

exp.sim<-function(n,x.rate,c.level){
  x.mean<-1/x.rate
  exp.sample<-rexp(n,rate=x.rate)
  result<-tidy(t.test(exp.sample,mu=x.mean,conf.level = c.level))
  Estimate<-result$estimate
  L.conf<-result$conf.low
  U.conf<-result$conf.high
  Decision<-if(x.mean >= result$conf.low & x.mean <=result$conf.high){decision=1}else{decision=0}
  out=vector()
  out[1] <-L.conf
  out[2] <-U.conf
  out[3] <-Decision
  out}



################################################################################################

#Sim function for use with  Monte Carlo Package

exp.sim.2<-function(n,x.rate){
  x.mean<-1/x.rate
  exp.sample<-rexp(n,rate=x.rate)
  result<-tidy(t.test(exp.sample,mu=x.mean))
  Estimate<-result$estimate
  L.conf<-result$conf.low
  U.conf<-result$conf.high
  Decision<-if(x.mean >= result$conf.low & x.mean <=result$conf.high){decision=1}else{decision=0}
  out=list(L.conf, U.conf, Decision)
  names(out)<-c("L.conf","U.conf","Decision")
  out}

exp.sim.2(s.size,Lambda)
n.monte<-c(30,50,100)
x.rate.monte<-c(1,2)
p_list<-list("n"=n.monte, "x.rate"= x.rate.monte)
result<-MonteCarlo(func = exp.sim.2,nrep=5, param_list = p_list)
summary(result)
MakeTable(output=result, rows="n", cols=c("x.rate"), digits=2, include_meta=FALSE)




n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b) 
