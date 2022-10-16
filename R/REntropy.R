REntropy<-function(postclassprob){
  classprob<-postclassprob[c(grep("prob|Prob|PROB",names(postclassprob),value = TRUE))]
  logprob<-log(classprob)
  logprob[logprob == '-Inf']<-'0'
  logprob[]<-lapply(logprob,as.numeric)
  num.E<- -sum(classprob * logprob)
  deno.E<- nrow(classprob) * log(ncol(classprob))
  return(round(1-(num.E/deno.E),3))
}
