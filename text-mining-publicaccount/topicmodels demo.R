###############################
计算LDA主题模型主体数
##########################

fold_num = 10  
kv_num = c(5, 10*c(1:5, 10))  
seed_num = 2003  


smp<-function(cross=fold_num,n,seed)  
{  
  set.seed(seed)  
  dd=list()  
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)  
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]  
  return(dd)  
}  

selectK<-function(dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp) # change 60 to 15  
{  
  per_ctm=NULL  
  log_ctm=NULL  
  for (k in kv)  
  {  
    per=NULL  
    loglik=NULL  
    for (i in 1:3)  #only run for 3 replications#   
    {  
      cat("R is running for", "topic", k, "fold", i,  
          as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")  
      te=sp[[i]]  
      tr=setdiff(1:nrow(dtm),te)  
      
      # VEM = LDA(dtm[tr, ], k = k, control = list(seed = SEED)),  
      # VEM_fixed = LDA(dtm[tr,], k = k, control = list(estimate.alpha = FALSE, seed = SEED)),  
      
      CTM = CTM(dtm[tr,], k = k,   
                control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))    
      
      # Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",  
      # control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))  
      
      per=c(per,perplexity(CTM,newdata=dtm[te,]))  
      loglik=c(loglik,logLik(CTM,newdata=dtm[te,]))  
    }  
    per_ctm=rbind(per_ctm,per)  
    log_ctm=rbind(log_ctm,loglik)  
  }  
  return(list(perplex=per_ctm,loglik=log_ctm))  
}  

sp=smp(n=nrow(dtm),seed=seed_num)  

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))  

## plot the perplexity  

m_per=apply(ctmK[[1]],1,mean)  
m_log=apply(ctmK[[2]],1,mean)  

k=c(kv_num)  
df = ctmK[[1]]  # perplexity matrix  
matplot(k, df, type = c("b"), xlab = "Number of topics",   
        ylab = "Perplexity", pch=1:5,col = 1, main = '')         
legend("bottomright", legend = paste("fold", 1:5), col=1, pch=1:5)  