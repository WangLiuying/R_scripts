setwd("D:/DataAnalysis/R's workingspace/hotpot data")
library(data.table)
dt.raw <- fread("comments.csv",encoding = "UTF-8")
names(dt.raw)
# "V1"       "index"    "kouwei"   "huanjing" "fuwu"     "comment"  "date"   
dt <- dt.raw[,-c("V1")]
dt[nchar(comment)>3,.N]#9040 non-empty comment


#topic model-pretreatment
rm(dt)
cm <- dt.raw[nchar(comment)>3,"comment"] #delete na comment
head(cm)
library(jiebaR) #jiebaR divide words
help(package="jiebaR")
workdevice <- worker(bylines = T,stop_word ="chinese_stopword.txt" )
textword <- segment(unlist(cm),workdevice)
textword[sample(1:9040,10)]
textword <- lapply(textword,paste,collapse=" ")
textword[sample(1:9040,10)]


library(lda)
#topic model-lexicalize
corpus <- lexicalize(unlist(textword),sep=" ")
corpus$documents[1]
length(corpus[[2]]) #20093
wordfreq <- word.counts(corpus$documents)
summary(wordfreq)
to.keep <- corpus$vocab[wordfreq>=4]
corpus <- lexicalize(unlist(textword),sep=" ",vocab=to.keep)
#temporary cleaning
ls()
rm("cm","textword","wordfreq")

#topic model-try a model
set.seed(6646)
K <- 10 ## Num clusters
alpha <- 0.1
eta <- 0.1
result <- lda.collapsed.gibbs.sampler(corpus,
                                      K,
                                      to.keep,
                                      500, alpha = alpha,eta=eta,
                                      compute.log.likelihood=TRUE) 
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)
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
wordfreq <- as.integer(word.counts(corpus))
names(wordfreq) <- to.keep
doc.length <- sapply(corpus,length)
Encoding(to.keep)
library(LDAvis)
json <- createJSON(phi=phi,theta=theta,vocab=to.keep,
                   doc.length =doc.length,term.frequency = wordfreq)
serVis(json,out.dir = "./LDAvis-hotpot",open.browser=F)
