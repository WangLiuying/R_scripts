#learn lda & slda. 
#related packages:"lda"



##学习lda模型

data(cora.documents)
data(cora.vocab)
data(cora.cites)
data(cora.titles)

cora.documents[1]
str(cora.vocab)
str(cora.cites);length(cora.cites)
str(cora.titles)

#
data(cora.documents)

data(cora.vocab)

theme_set(theme_bw())

set.seed(8675309)

K <- 10 ## Num clusters

result <- lda.collapsed.gibbs.sampler(cora.documents,
                                       K,  ## Num clusters
                                       cora.vocab,
                                       25,  ## Num iterations
                                       0.1,
                                       0.1,
                                      compute.log.likelihood=TRUE) 

## Get the top words in the cluster
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)

## Number of documents to display
N <- 10

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)

topic.proportions <-
     topic.proportions[sample(1:dim(topic.proportions)[1], N),]

topic.proportions[is.na(topic.proportions)] <-  1 / K

colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")

topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                              variable.name="topic",
                              id.vars = "document")  

qplot(topic, value, fill=document, ylab="proportion",
      data=topic.proportions.df, geom="bar") +
      theme(axis.text.x = element_text(angle=90, hjust=1)) +
      coord_flip() +
      facet_wrap(~ document, ncol=5)

#2 slda
demo(slda)
set.seed(8675309)

## Use the political blogs data set.
data(poliblog.documents)

data(poliblog.vocab)

data(poliblog.ratings)#dependent variable

num.topics <- 10

## Initialize the params
params <- sample(c(-1, 1), num.topics, replace=TRUE)

result <- slda.em(documents=poliblog.documents,
                                       K=num.topics,
                                       vocab=poliblog.vocab,
                                       num.e.iterations=10,  #the # of sweeps of gibbs sampling
                                       num.m.iterations=4,   #the # of EM iterations
                                       alpha=1.0, eta=0.1,
                                       poliblog.ratings / 100, #规范化
                                       params,
                                       variance=0.25,
                                       lambda=1.0,
                                       logistic=FALSE,
                                       method="sLDA")
 ## Make a pretty picture.
 require("ggplot2")

 Topics <- apply(top.topic.words(result$topics, 5, by.score=TRUE),
                                  2, paste, collapse=" ")

 coefs <- data.frame(coef(summary(result$model)))

 theme_set(theme_bw())

 coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))

 coefs <- coefs[order(coefs$Estimate),]

 qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs) +
    geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error,
                                                  ymax=Estimate+Std..Error)) + coord_flip()
 
#mmsb & rtm 待学


######################################################3
# fliter.word
 data(cora.documents)
 corpus <- cora.documents[1:6]
 wc <- word.counts(corpus)
 ## Only keep the words which occur more than 4 times.
 filtered <- filter.words(corpus,
                          as.numeric(names(wc)[wc <= 4]))#to.remove=integer vector
 ## Shift the second half of the corpus.
 shifted <- shift.word.indices(filtered[4:6], 100)
 ## Combine the unshifted documents and the shifted documents.
 concatenate.documents(filtered[1:3], shifted)
 
 
 #lexicalize
 example <- c("I am the very model of a modern major general",
              "I have a major headache")
 corpus <- lexicalize(example, lower=TRUE)
 ## Only keep words that appear at least twice:
 to.keep <- corpus$vocab[word.counts(corpus$documents, corpus$vocab) >= 2]
 ## Re-lexicalize, using this subsetted vocabulary
 documents <- lexicalize(example, lower=TRUE, vocab=to.keep)
 
 #link.as.edgelist
 #used in RTM model
 ## Take the citations for the first few documents of Cora.
data(cora.cites)
links <- cora.cites[1:5]
links
links.as.edgelist(links)

#nubbi.collapsed.gibbs.sampler
# Collapsed Gibbs Sampling for the Networks Uncovered By Bayesian
# Inference (NUBBI) Model.
demo(nubbi)

# predictive.distribution
# Compute predictive distributions for fitted LDA-type models
## Fit a model (from demo(lda)).
data(cora.documents)
data(cora.vocab)
K <- 10 ## Num clusters
result <- lda.collapsed.gibbs.sampler(cora.documents,
                                      K, ## Num clusters
                                      cora.vocab,
                                      25, ## Num iterations
                                      0.1,
                                      0.1)
## Predict new words for the first two documents
predictions <- predictive.distribution(result$document_sums[,1:2],
                                       result$topics,
                                       0.1, 0.1)
## Use top.topic.words to show the top 5 predictions in each document.
top.topic.words(t(predictions), 5)

# predictive.link.probability() used in rtm

#slda.predict
?slda.predict


#top.topic.words
# Get the Top Words and Documents in Each Topic
# check demo(lda)