#version2
setwd("C:/Users/River/Desktop/text-mining tianya")
load("gossip news.RData")
document <- df.raw[df.raw$account!="gossipmaker",]$body;
rm(df.raw)

#dividing-name+topics
library(jiebaR)
workdevice <- worker(stop_word = "chinese_stopword.txt",bylines = T)
Mydict <- c(readLines("starname2.txt",encoding="UTF-8"),readLines("topics.txt",encoding="UTF-8"))
new_user_word(workdevice,Mydict)
seg_document <- segment(document,workdevice)

tofilter <- unique(unlist(seg_document)[!unlist(seg_document) %in% Mydict])
stopnew <- c(readLines("chinese_stopword.txt",encoding = "UTF-8"),tofilter)
con <- file("stopword.txt",encoding="UTF-8")
writeLines(stopnew,con)
close(con);rm(stopnew)


workdevice <- worker(stop_word = "stopword.txt",bylines = T)
seg_document <- segment(document,workdevice)
seg_document[30]
seg_document<- lapply(seg_document,paste,collapse=" ")
seg_document[300]

#dividing-topics
library(jiebaR)
workdevice <- worker(stop_word = "chinese_stopword.txt",bylines = T)
Mydict <- readLines("topics.txt",encoding="UTF-8")
new_user_word(workdevice,Mydict)
seg_document <- segment(document,workdevice)

tofilter <- unique(unlist(seg_document)[!unlist(seg_document) %in% Mydict])
stopnew <- c(readLines("chinese_stopword.txt",encoding = "UTF-8"),tofilter)
con <- file("stopword-onlytopics.txt",encoding="UTF-8")
writeLines(stopnew,con)
close(con);rm(stopnew)


workdevice <- worker(stop_word = "stopword-onlytopics.txt",bylines = T)
seg_document <- segment(document,workdevice)
seg_document[30]
seg_document<- lapply(seg_document,paste,collapse=" ")
seg_document[300]

library(lda)
#topic model-lexicalize
corpus <- lexicalize(unlist(seg_document),sep=" ")
corpus$documents[1]
length(corpus[[2]]) 
wordfreq <- word.counts(corpus$documents)
summary(wordfreq)


#NOT RUN#
#topic model-try a model
set.seed(6646)
K <- 10 ## Num clusters
alpha <- 0.1
eta <- 0.1
result <- lda.collapsed.gibbs.sampler(corpus$documents,
                                      K,vocab=corpus$vocab,
                                      300, alpha = alpha,eta=eta,
                                      compute.log.likelihood=TRUE) 
top.words <- top.topic.words(result$topics, 10, by.score=TRUE)
top.words
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)








####################################################
###################换包啦###########################
#######################################################
library(topicmodels)
#该包迭代速度很快但是个黑匣子，具体的后验matrix无法计算。
#我们用它来确定主题数和标记。LDAvis包的可视化仍需依赖lda


corpus.tm <- ldaformat2dtm(corpus$documents,corpus$vocab)#从LDA包到tm包转换数据桥接
seed <- 6646
K <- 25:50
fold <- 10;n <- length(corpus$documents)
alpha=0.1;delta=0.1
perp <- data.frame()
for (k in K)
{
  log.likelihood <- c()
  foldedperp <- c()
  for (f in 1:fold)
    {
    index <- ((f-1)*n%/%10+1):(f*n%/%10)
    corpus.loop <- ldaformat2dtm(corpus$documents[-index],corpus$vocab)
    testset <- ldaformat2dtm(corpus$documents[index],corpus$vocab)
    gibbs <- LDA(corpus.loop,k,method="Gibbs",
                 control = list(alpha=alpha,delta=delta,burnin=100,iter=200,seed=6646))
    log.likelihood <- c(log.likelihood,gibbs@loglikelihood)
    foldedperp <- c(foldedperp,perplexity(gibbs,testset))
    }
  perp <- rbind(perp,data.frame(Num_topics=k,loglikelihood=mean(log.likelihood),perplexity=mean(foldedperp)))
}

perp
k=32
gibbs <- LDA(corpus.tm,k,method="Gibbs",
             control = list(alpha=alpha,delta=delta,burnin=200,iter=2000,seed=6646))
fit.topics <- topics(gibbs)#提出主题
(topwords <- terms(gibbs,threshold=0.03))#提出主题中最可能的关键词



##################################
#if no filter#
###########
#dividing-topics
library(jiebaR)
workdevice <- worker(stop_word = "chinese_stopword.txt",bylines = T)
Mydict <- readLines("topics.txt",encoding="UTF-8")
new_user_word(workdevice,Mydict);rm(Mydict)
seg_document <- segment(document,workdevice)
seg_document<- lapply(seg_document,paste,collapse=" ")
library(lda)
#topic model-lexicalize
corpus <- lexicalize(unlist(seg_document),sep=" ")
corpus$documents[1]
length(corpus[[2]]) 
wordfreq <- word.counts(corpus$documents)
summary(wordfreq)

#######################Num of k####################################
corpus.tm <- ldaformat2dtm(corpus$documents,corpus$vocab)#从LDA包到tm包转换数据桥接
K <- 10:50
fold <- 10;n <- length(corpus$documents)
alpha=0.1;delta=0.1
perp <- data.frame()
for (k in K)
{
  log.likelihood <- c()
  foldedperp <- c()
  for (f in 1:fold)
  {
    index <- ((f-1)*n%/%10+1):(f*n%/%10)
    corpus.loop <- ldaformat2dtm(corpus$documents[-index],corpus$vocab)
    testset <- ldaformat2dtm(corpus$documents[index],corpus$vocab)
    gibbs <- LDA(corpus.loop,k,method="Gibbs",
                 control = list(alpha=alpha,delta=delta,burnin=500,iter=500,seed=6646))
    log.likelihood <- c(log.likelihood,gibbs@loglikelihood)
    foldedperp <- c(foldedperp,perplexity(gibbs,testset))
  }
  perp <- rbind(perp,data.frame(Num_topics=k,loglikelihood=mean(log.likelihood),perplexity=mean(foldedperp)))
}

perp
k=32
gibbs <- LDA(corpus.tm,k,method="Gibbs",
             control = list(alpha=alpha,delta=delta,burnin=200,iter=2000,seed=6646))
fit.topics <- topics(gibbs)#提出主题
(topwords <- terms(gibbs,5))#提出主题中最可能的关键词




#plot
#doc-topic theta matrix(posterior)
theta <- t(apply(Gibbs@+alpha,2,function(x) x/sum(x)))
#topic-word phi matrix
phi <- t(apply(t(result$topics)+eta,2,function(x) x/sum(x)))
wordfreq <- as.integer(word.counts(corpus$documents))
names(wordfreq) <- corpus$vocab
doc.length <- sapply(corpus$documents,length)




library(LDAvis)
json <- createJSON(phi=phi,theta=theta,vocab=corpus$vocab,
                   doc.length =doc.length,term.frequency = wordfreq)

dir.path <- "./LDAvis-gossip-version3"
serVis(json,out.dir = dir.path,open.browser=F)


writeLines(iconv(readLines(paste(dir.path,"/lda.json",sep="")), from = "GBK", to = "UTF8"),
           
           file(paste(dir.path,"/lda.json",sep=""), encoding="UTF-8"))