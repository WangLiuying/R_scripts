##整理数据
setwd("C:/Users/River/Desktop/text-mining tianya")

library(data.table)
shenye <- as.data.table(df)
realmovie <- as.data.table(df)
yansu <- as.data.table(df)


shenye$account <- "shenyebagua818"
realmovie$account <- "realmovie520"
yansu$account <- "yansubagua"

df.raw <- rbind(shenye,realmovie,yansu)
rm(df,shenye,realmovie,yansu)

View(df.raw)
save(df.raw,file="gossip news.RData")


##############################################
#load
library(data.table)
load("gossip news.Rdata")
class(df.raw)
document <- as.character(df.raw$body)
rm(df.raw)

##dividing-topics##
library(jiebaR)
workdevice <- worker(stop_word = "chinese_stopword-stars.txt",bylines = T)
Mydict <- c(readLines("topics.txt",encoding="UTF-8"))
#,readLines("starname2.txt",encoding = "UTF-8")
new_user_word(workdevice,Mydict)
seg_document <- segment(unlist(document),workdevice)

# #filter
# tofilter <- unique(unlist(seg_document)[!unlist(seg_document) %in% Mydict])
# stopnew <- c(readLines("chinese_stopword.txt",encoding = "UTF-8"),tofilter)
# con <- file("stopword-onlytopics.txt",encoding="UTF-8")
# writeLines(stopnew,con)
# close(con);rm(stopnew)
# workdevice <- worker(stop_word = "stopword-onlytopics.txt",bylines = T)
# seg_document <- segment(document,workdevice)
# seg_document[30]
# 
seg_document<- lapply(seg_document,paste,collapse=" ")
seg_document[300]


##form corpus##

library(lda)
#topic model-lexicalize
corpus <- lexicalize(unlist(seg_document),sep=" ")
corpus$documents[1]
length(corpus[[2]]) 
wordfreq <- word.counts(corpus$documents)
summary(wordfreq)

to.keep <- corpus$vocab[wordfreq>=50]
corpus <- lexicalize(unlist(seg_document),sep=" ",vocab=to.keep,lower=F)
deleterecord <- which(sapply(corpus,length)==0)
corpus <- corpus[sapply(corpus,length)!=0]

##train##
library(topicmodels)
#该包迭代速度很快但是个黑匣子，具体的后验matrix无法计算。
#我们用它来确定主题数和标记。LDAvis包的可视化仍需依赖lda


corpus.tm <- ldaformat2dtm(corpus,to.keep)#从LDA包到tm包转换数据桥接

K <- 27:50#computed to 26
fold <- 5;n <- length(corpus)
alpha=0.1;delta=0.1
#perp <- data.frame()
for (k in K)
{
  log.likelihood <- c()
  foldedperp <- c()
  for (f in 1:fold)
  {
    cat("training:k=",k,",fold=",f,"\n")
    index <- ((f-1)*n%/%fold+1):(f*n%/%fold)
    corpus.loop <- ldaformat2dtm(corpus[-index],to.keep)
    testset <- ldaformat2dtm(corpus[index],to.keep)
    gibbs <- LDA(corpus.loop,k,method="Gibbs",
                 control = list(alpha=alpha,delta=delta,burnin=300,iter=500,seed=6646))
    log.likelihood <- c(log.likelihood,gibbs@loglikelihood)
    foldedperp <- c(foldedperp,perplexity(gibbs,testset))

  }
  perp <- rbind(perp,data.frame(Num_topics=k,loglikelihood=mean(log.likelihood),perplexity=mean(foldedperp)))
  save.image("lda_ver2.RData")
  Sys.sleep(2)
}
system('cmd /c D:\\River\\Music\\"Trouble Maker-Trouble Maker.mp3"')
# perp
#
library(ggplot2)
ggplot(data=perp,mapping=aes(x=Num_topics,y=perplexity))+geom_line()+
  geom_point()+labs(xlab="Num of topics",ylab="Perplexity")
k=20#choose a proper k which minimizes perplexity.
alpha=0.1;delta=0.1
gibbs <- LDA(corpus.tm,k,method="Gibbs",
             control = list(alpha=alpha,delta=delta,burnin=200,iter=2000,seed=6646))
table(fit.topics <- topics(gibbs))#提出主题
(topwords <- terms(gibbs,5))#提出主题中最可能的关键词
#system('cmd /c D:\\River\\Music\\"Trouble Maker-Trouble Maker.mp3"')

topic


#plot
#doc-topic theta matrix(posterior)
theta <- gibbs@gamma
#topic-word phi matrix
phi <- exp(gibbs@beta)
wordfreq <- as.integer(word.counts(corpus,to.keep))
names(wordfreq) <- to.keep
doc.length <- sapply(corpus,length)





library(LDAvis)
json <- createJSON(phi=phi,theta=theta,vocab=to.keep,
                   doc.length =doc.length,term.frequency = wordfreq)

dir.path <- "./LDAvis-gossip-versionfinla"
serVis(json,out.dir = dir.path,open.browser=F)


writeLines(iconv(readLines(paste(dir.path,"/lda.json",sep="")), from = "GBK", to = "UTF8"),
           
           file(paste(dir.path,"/lda.json",sep=""), encoding="UTF-8"))
dev.off()


system('cmd /c D:\\River\\Music\\"Trouble Maker-Trouble Maker.mp3"')
