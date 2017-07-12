##data
setwd("C:/Users/River/Desktop/text-mining tianya")

gossip <- df
realmovie <- df
yansu <- df


gossip$account <- "gossipmaker"
realmovie$account <- "realmovie520"
yansu$account <- "yansubagua"

df.raw <- rbind(gossip,realmovie,yansu)
rm(df,gossip,realmovie,yansu)

View(df.raw)
save(df.raw,file="gossip news.RData")
########################################################

load("gossip news.RData")
document <- df.raw[df.raw$account!="gossipmaker",]$body;
rm(df.raw)

###################################
#dividing
library(jiebaR)
workdevice <- worker("keywords",stop_word = "chinese_stopword.txt",user="topics.txt",bylines = T,topn=20)
seg_document <- vector_keywords(unlist(document),workdevice)

seg_document<- lapply(seg_document,paste,collapse=" ")
seg_document[300]
###################################
#dividing
library(jiebaR)
workdevice <- worker(dict="self-dict.utf8",stop_word = "chinese_stopword.txt",bylines = T)
seg_document <- lapply(document,keywords,workdevice)

seg_document<- lapply(seg_document,paste,collapse=" ")
seg_document[300]
######################################
library(lda)
#topic model-lexicalize
corpus <- lexicalize(unlist(seg_document),sep=" ",lower=F)
corpus$documents[1]
length(corpus[[2]]) 
wordfreq <- word.counts(corpus$documents)
summary(wordfreq)
to.keep <- corpus$vocab[wordfreq>=4]
corpus <- lexicalize(unlist(seg_document),sep=" ",vocab=to.keep,lower=F)
rm(document,seg_document)


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

#sampling 10 to display
topic.proportions <-
  topic.proportions[sample(1:dim(topic.proportions)[1], 10),]

topic.proportions
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:10)),
                             variable.name="topic",
                             id.vars = "document")  
str(topic.proportions.df)
head(topic.proportions.df)
library(ggplot2)
ggplot(aes(x=topic,y=value,fill=document),data=topic.proportions.df)+
  geom_col(position="dodge")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+coord_flip()+
  facet_wrap(~document)

#LDAvis

#doc-topic theta matrix(posterior)
theta <- t(apply(result$document_sums+alpha,2,function(x) x/sum(x)))
#topic-word phi matrix
phi <- t(apply(t(result$topics)+eta,2,function(x) x/sum(x)))
wordfreq <- as.integer(word.counts(corpus$documents))
names(wordfreq) <- corpus$vocab
doc.length <- sapply(corpus$documents,length)

library(LDAvis)
json <- createJSON(phi=phi,theta=theta,vocab=corpus$vocab,
                   doc.length =doc.length,term.frequency = wordfreq)

serVis(json,out.dir = "./LDAvis-gossip2",open.browser=F)

corpus$vocab

setwd("C:/Users/River/Desktop/text-mining tianya")