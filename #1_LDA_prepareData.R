#### #######################################################

# Prepare document
# This file process the subsetted data that starts with and end with 'risk'


#### #######################################################
library(dplyr);library(ggplot2);library(reshape)
setwd("~/Dropbox/2016_project/16-Risk Ngram revisted")

# Set hyper-parameter 
timeStart <- 1850
timeEnd <- 2008

# Load data
data <- read.table('./data/risk_LDA.txt',sep='\n',colClasses = 'character')
colnames(data)<-'gram'
data$year <- c(1850:2008)
data <- data[data$year>=timeStart,]
corpora_temp <- data$gram

###### Clean and data preparation #####
corpora_temp <- data$gram
corpora_temp<-gsub("[[:punct:]]","",corpora_temp) # remove all puncutation
corpora_temp<-gsub('[[:digit:]]+', "", corpora_temp)
rmWord = c('willing','will','would','without','with','could','can','due','may','even','must','rather','upon',
           'every','always','much','many','everything','might','one','though','frank','however','wallach','but','also',
           'le','ing','ts','et','b',
           'great','greater','little','include',
           'take','run','put','incur','go','get','involve','use','make','carry',
           'increase','reduce','associate','factor','low','high','higher','lower',
           'risk','oversimplification')

rmWordRex = as.character()
for (x in rmWord){
  rmWordRex <- c(rmWordRex,paste('\\b',x,'\\b',sep=''))
}
for (x in rmWordRex){
  corpora_temp<-gsub(x, "", corpora_temp)
}
data$gram<-corpora_temp
save(data, file = "./data/processed.RData") 


#### ########################################################### ########################################################### ########################################################### ########################################################### #######################################################

# Get Code

#### ########################################################### ########################################################### ########################################################### ########################################################### #######################################################

setwd("~/Dropbox/2016_project/16-Risk Ngram revisted")
# Set hyper-parameter 
timeStart <- 1850;timeEnd <- 2008
load("./data/processed.RData")

#data <- data[c(1:5,35:38,130),]

#### #######################################################

#corpora<-data$gram
datalist <- list()
for (i in 1:dim(data)[1]){
  print (i)
  temp <- strsplit(data$gram[i]," ")
  datalist[i] <- temp
}

#LDA create term table 
#doc.list <- strsplit(corpora, "[[:space:]]+")
doc.list<-lapply(datalist,unlist)
term.table<-table(unlist(doc.list))   
term.table<-term.table[-1]
#term.table <- sort(term.table, decreasing = TRUE)
# test: which words are in the vocabulary
anyW <- 'would';anyW %in% names(term.table)
anyW <- 'aids' ;anyW %in% names(term.table)
term.table['war'];term.table['finance'];term.table['transmitted'];term.table['hiv'];term.table['aids'];term.table['acquired'];term.table['immunodeficiency']
term.table['sex'];term.table['virus'];term.table['transmit']

# Set threshold = n; remove all words with total frequency less than n
n = 50
1-sum(term.table[term.table<=n]) /sum(term.table) # [1] 7803
sum(term.table<n)/length(term.table)

del <- term.table < n
length(term.table) #3800
sum(del) #[1] 65
term.table <- term.table[!del]
term.table<-term.table[which(nchar(names(term.table))!=0)]
term.table <- sort(term.table,decreasing = TRUE)
vocab <- names(term.table)

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)




#wordNum <- as.numeric()
#for (i in 1:dim(data)[1]){
#  wordNum<-c(wordNum,length(datalist[[i]]))
#}
#year <- c((timeStart+1):timeEnd)
#plot(year[1:150],wordNum[1:150],type='l')

# remove terms that occur fewer than n times:
#int <- seq(1,100000,100)
#y=as.numeric()
#for (i in (int)){
#  temp<-sum(term.table[term.table<i])/sum(term.table)
#  y<-c(y,temp)
#}
#yy=as.numeric()
#for (i in (int)){
#  temp<-sum(term.table<i)/length(term.table)
#  yy<-c(yy,temp)
#}
#plot(int[1:600],y[1:600],main = 'Proportion of terms that occur fewer than n times',type='l',ylim=c(0,1))
#lines(int[1:600],yy[1:600],type='o')


#plot(log(1:7733),log(term.table),xlim=c(0,10),type='l',
#     main='log plot of rank vs frequenct of words co-occur with RISK',
#     xlab='log rank',ylab='log frequency')
#abline(v=log(7573))


#### #######################################################

# train on LDA

#### #######################################################

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (158)
W <- length(vocab)  # number of terms in the vocab (9,027)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (7,388,564)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [1] 971528 444609 2844

# MCMC and model tuning parameters:
K <- 15 ################################### Change this!
alpha = 0.01
G <- 1000
eta <- 0.01

# Fit the model:
set.seed(3000)
# fit data
library(lda)
t1 <- Sys.time()

fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 45 minutes on laptop

result<-fit


####################################################################################

# alpha_pool: try topicmodel with different alpha value

####################################################################################


alpha_pool <-seq(0.00,0.4,by=0.05)
alpha_pool <- alpha_pool[-1];alpha_pool
alpha_pool <- c(0.01,alpha_pool);alpha_pool
alpha_pool <- c(alpha_pool,50/K)
alpha_pool

library(lda)
for (alpha in alpha_pool){
  print (alpha)
  G <- 1000
  #alpha <- 50/K
  eta <- 0.01
  
  # Fit the model:
  set.seed(3000)
  # fit data
  library(lda)
  t1 <- Sys.time()
  
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  t2 <- Sys.time()
  t2 - t1  # about 45 minutes on laptop

  result<-fit
  
  #Visualize
  theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
  RiotTweets <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
  library(LDAvis)
  
  # create the JSON object to feed the visualization:
  json <- createJSON(phi = RiotTweets$phi, 
                     theta = RiotTweets$theta, 
                     doc.length = RiotTweets$doc.length, 
                     vocab = RiotTweets$vocab, 
                     term.frequency = RiotTweets$term.frequency,
                     mds.method = jsPCA)
  library(servr)
  dirName = paste('risk_combined_topic_15',K,sep='')
  fileName = paste(dirName,'/08_02_risk_combined_0.01_0.01.RData',sep='')
  serVis(json, out.dir = dirName, open.browser = FALSE)
  save(fit,file=fileName)
  
}

# Select optimal number of topics gt5


K_pool <- seq(10,35,by=5)
print(K)
dirName = paste('risk_combined_topicNum_',K,sep='')
fileName = paste(dirName,'/risk_combined_50k_0.01.RData',sep='')
load (fileName)
log <- fit['log.likelihoods'][[1]]; log <- data.frame(log); log <- t(log);log <- data.frame(log)
log['num'] <- c(1:1000)
colnames(log) <- c('v1','v2','num')
log <- melt(log,id.vars='num',measure.vars = c('v1','v2'))
ggplot(log,aes(num,value,group=variable,color = variable)) + geom_line()


K_pool <- seq(10,30,by=5)
logVector <- as.numeric()
for (K in K_pool){
  print(K)
  dirName = paste('risk_combined_topicNum_',K,sep='')
  fileName = paste(dirName,'/risk_combined_50k_0.01.RData',sep='')
  load (fileName)
  log <- fit['log.likelihoods'][[1]]
  logVector<- c(logVector,log[2,100])
}
plot(logVector)








