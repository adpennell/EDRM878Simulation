#Brad Rogers
#Exponential Distribution T.test Function


s.size<-30
Lambda<-1
n.rep<-100000

x<-replicate(n.rep,exp.sim(s.size,Lambda))
Percent<-(sum(x[3,])/n.rep)*100
Percent



exp.sim<-function(n,x.rate){
  x.mean<-1/x.rate
  exp.sample<-rexp(n,rate=x.rate)
  result<-tidy(t.test(exp.sample,mu=x.mean))
  Estimate<-result$estimate
  L.conf<-result$conf.low
  U.conf<-result$conf.high
  Decision<-if(x.mean >= result$conf.low & x.mean <=result$conf.high){decision=1}else{decision=0}
  out=vector()
  out[1] <-L.conf
  out[2] <-U.conf
  out[3] <-Decision
  out}




result<-MonteCarlo(func = exp.sim,nrep=2, param_list = p_list)
summary(result)
MakeTable(output=result, rows="n", cols=c("x.rate","x.mean"), digits=2, include_meta=FALSE)
