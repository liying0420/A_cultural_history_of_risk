# LDA vis Plot 

# Load Library, set working directory, and set hyper-parameter
library(dplyr);library(ggplot2);library(reshape); library(lda)

setwd("~/Dropbox/2016_project/16-Risk Ngram revisted")
timeStart <- 1850; timeEnd <- 2008
K <- 15; G <- 1000; alpha <- 0.01 ; eta <- 0.01

folderName <- '08_02_risk_combined_0.01_0.01'
fileName <- paste('./#0_LDA_vis_ngram/',folderName,'/',folderName,  '.RData',sep='')
fileName
load(fileName)

########### prepare for visualize ###########
result <- fit
reorder_Topic_by_size <- order(result$topic_sums,decreasing = T)
terms <- colnames(fit$topics)
freq <- apply(fit$topics,2,sum)
print (freq['hiv'])


########## Specificity of each topic 

dim(result$topics)

abs_freq <- apply(result$topics,2,sum)

dis_topics_words <- result$topics
for (i in 1:K){
  dis_topics_words [i,] <- dis_topics_words [i,]/result$topic_sums[i]
}

lambda = 0.4
return_relevant_words <- function(number){
  relevantWords <- as.character()
  for (i in 1:K){
    temp <- lambda * log(dis_topics_words[i,]) + (1-lambda) * log(dis_topics_words[i,]/abs_freq)
    relevantWords <- rbind(relevantWords, names(head(sort(temp,decreasing = T),number)))
  }
  relevantWords <- relevantWords[reorder_Topic_by_size, ] 
  relevantWords
}
return_relevant_words(10)


# load general word frequency
library(readr)
random <- read.csv('data/frequency50000.csv');
random <- as.matrix(random)
random <- random[,2:210]
random <- apply(random,1,mean)

random[10000:10010]

word <- read_file('data/wordIndex.txt');
word <- strsplit(word,' ');word <- word[[1]]

freq_random <- as.numeric()
for (i in names(freq)){
  index = which (word ==i)
  if (sum(index) != 0){
    temp = random[index]
    freq_random <- c(freq_random, temp)
  }else{
    freq_random <- c(freq_random, 0)
  }
}
length(freq_random)

# load words' frequency in risk-5grams

com_freq <- cbind(freq,freq_random)
com_freq <- as.data.frame(com_freq)
com_freq['word'] <- row.names(com_freq)
colnames(com_freq) <- c('ngram','random','word')
com_freq$ngram <-com_freq$ngram/sum(com_freq$ngram) 
com_freq$ratio <- com_freq$ngram/com_freq$random
com_freq$word[com_freq$random==0]=random[20000]

specificity <- as.numeric()
wordNum=20
topicNum <- K
for (i in 1:topicNum){
  relevant_words <- return_relevant_words(wordNum)[i,]
  temp <- com_freq[com_freq$word %in% relevant_words,]
  temp$value <- temp$ngram/sum(temp$ngram)
  specificity <<- c(specificity,sum(temp$value *temp$ratio))
}
specificity <<- data.frame('topic'=paste('Topic',1:topicNum,sep='.'),'specificity'=specificity)
specificity <- cbind(specificity,return_relevant_words(10))
specificity <- specificity[order(specificity$specificity,decreasing = T),]
index_heat <- as.numeric(rownames(specificity))

specificity 


########### heatMap ###########
topic_index <- 1:20

topic_index <- c(12,7,9,13,############################################
                 1,5,8,6,
                 10,
                 15,11,
                 4,
                 14,3,
                 2)

topic_name <- c('war','war','war','nuclear',
                'disease','disease','disease','disease',
                'disease',# diabets
                'AIDS','HIV',
                'social',
                'economics','finance',
                'unknown')

col_old  <- paste(topic_name,topic_index,sep='_')
col <- topic_index

topicWord<-data.frame(result$topics)

topicWord<-topicWord[reorder_Topic_by_size,] # arrange by topic size

row.names(topicWord)<-c(1:K)
topicWord<-topicWord[topic_index,]
row.names(topicWord)<-col

general_w <- names(term.table[1:20])
print (general_w)

general_w <- c('safe','danger','death','life','blood','lose')
war_w <-     c('war','world','army','battle','liberty','invasion','fight')
international_w <- c('germany','japan','italy','american','soviet')
#international_w_t <- c('Germany','Japan','Italy','American','Soviet')
nuclear_w <- c('atomic','leak','nuclear','radiation','carcinogenic')
social_w <- c('family','child','violence','education','abuse','crime','food','poverty')
cancer_w  <- c('cancer','lung', 'bowel', 'breast', 'prostate','liver','stomach','kidney','leukemia')
hiv_w <-     c('transmit','infection','aids','hiv','gay','sexually','immunodeficiency','condom','virus','herpes','syphilis')
#hiv_w_t <-     c('transmit','infection','AIDS','HIV','gay','sexually','immunodeficiency','condom','virus','herpes','syphilis')
heart_w <-   c('heart','stroke','vascular','cardiovascular','artery','coronary')
diabetes_w <- c('diabetes','obesity','glucose','type','insulin','bmi','overweight')
#diabetes_w_t <- c('diabetes','obesity','glucose','type','insulin','BMI','overweight')
finance_w <- c('invest','return','portfolio','interest','asset','rate')
economics_w <- c('preference','assumption','equilibrium','journal','uncertainty')
#poverty_w <- c('social','economic','abuse','poverty','drug','violence','malaria','tuberculosis')
#naturalDisaster_w <- c('earthquake','flood','fire')
word <- c(war_w,international_w ,nuclear_w,cancer_w,heart_w,diabetes_w,hiv_w,social_w,economics_w,finance_w)#,poverty_w)

environment_w <- c('environment','climate','pollution')
food_w <- c('food','fat','diet','nutrition')
law_w <- c('law','court','justice','legal','smoke','abortion')
flight_w <- c('flight','delay','crash','air')
company_w <-c ('company','revenue','industry','market','customer')
fraud_w <- c('fraud','enron','andersen')

word <- c(environment_w,food_w ,law_w,flight_w,company_w,fraud_w)#,poverty_w)
length(word)
word_full <- word
########################################

########################################
word [!word %in% terms]
word <- word [word %in% terms]
heatData <- topicWord[,word]

f <- function(x){x/sum(x)}
heatData <- apply(heatData,2,f)
sequence <- row.names(heatData)
row.names(heatData) <- as.character(c(1:K)) ##############!!!!!!!!!!!!!################

# cbind rows of 3 of enron, accounting, andersen
heatData <- cbind(heatData,matrix(rep(0,15*2),15))
colnames(heatData)<-word_full

heatData <- melt(heatData)
#heatData$value[heatData$value<=0.05] <- 0
colnames(heatData) <- c('Var1','Var2','value')
heatData$Var2 <- as.character(heatData$Var2 )
heatData$Var2 <-factor (heatData$Var2, levels = rev(unique(heatData$Var2)))

heatData$Var1 <- as.character(heatData$Var1 )
heatData$Var1 <-factor (heatData$Var1, levels = unique(heatData$Var1))


## set color representation for specific values of the data distribution
## use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)


myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral"))) # 11 is maxium number, break spectral colour into 11 range
sc <- scale_fill_gradientn(colours = myPalette(100)[10:100], # break range into 100 colours 
                           limits=c(0, 1)) 

p1 <- ggplot(heatData, aes(as.factor(Var1), Var2, group=Var2)) +
  labs(title = 'Google Ngram Corpus',x='topic index')+
  geom_tile(aes(fill = value),colour='lightgrey') +
  sc+
  #scale_fill_gradient(low = '#deebf7', high = "#de2d26") +
  labs(x=NULL,y=NULL)+#,title='Word/topic probability')+
  theme(plot.title=element_text(size = 12,hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.x=element_text(size = 10,angle=0,vjust=0),
        axis.text.y=element_text(size = 10),
        legend.title=element_text(size = 10),
        legend.text=element_text(size = 10))
p1

#save(heatData,file='#4_figure/data_to_plot/heatNgram.RData')

specificityP <- specificity[sequence,]

specificityP['index'] <- factor( as.character(1:K) ,levels =  as.character(1:K) )
specP <- ggplot(data = specificityP, aes(x=index,y=specificity)) + geom_bar(stat='identity')+
  geom_hline(yintercept = 1,colour='red')+labs(y='Topic specificity',x='Topic index')+
  theme_bw()+
  theme(
    axis.title = element_text(size=12),axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

specP
specificity[match(as.character(topic_index),row.names(specificity)),]

# save heatmap
ggplot2::ggsave(filename='./#4_figure/risk_ngram_heatmap_1_colourful.jpg',
                plot = p1,
                path = NULL, 
                scale = 1, 
                width = 18, height =24, 
                units = c( "cm"), dpi = 1000)


# save heatmap
ggplot2::ggsave(filename='./#4_figure/risk_ngram_heatmap_2_colourful.jpg',
                plot = p1,
                path = NULL, 
                scale = 1, 
                width = 18, height = 10, 
                units = c( "cm"), dpi = 1000)



ggplot2::ggsave(filename='./#4_figure/risk_ngram_specificity.jpg',
                plot = specP,
                path = NULL, 
                scale = 1, 
                width = 18, height = 5, 
                units = c( "cm"), dpi = 1000)
  
  
  
  
  
  
########### Trend ######################### Trend ######################### Trend ######################### Trend ######################### Trend ##############  
  
########### Trend ##############

########### Trend ######################### Trend ######################### Trend ######################### Trend ##############
trend <- as.data.frame(result['document_sums'])
colnames(trend)<- c(1850:2008)
temp <- apply(trend,2,sum)
for (i in 1:K){
  trend[i,] <- trend[i,]/temp}
trend <- trend[reorder_Topic_by_size,]
trend <- trend [topic_index,]
trend_relative<-as.data.frame(trend)

trend['topic'] <- paste('Topic',1:15,sep=' ')
trend <- melt(trend); colnames(trend) <- c('topic','year','value'); trend$year <- as.numeric(as.character(trend$year))

trend$topic <- factor (trend$topic,levels = paste('Topic',1:15,sep=' '))
p0 <- ggplot(data = trend, aes(x=year,y=value)) +  geom_line(aes(colour = topic)) + 
  scale_x_continuous(breaks = seq(1850,2008,by=10),expand=c(0,0))+
  labs (x='Year',y='Topic prevalence')+
  theme_bw()+
  theme(#plot.title=element_text(size=rel(2)),
    #axis.title.x=element_blank(),
    axis.text.x =element_text(size=8,colour='black'),
    axis.text.y=element_text(size=8,colour='black'),
    #legend.position='none',
    legend.title=element_blank(),
    legend.text=element_text(size=10,angle = 0),
    legend.direction='horizontal',
    legend.position = c(0.5,0),
    legend.key.size = unit(0.5, "cm"),
    panel.grid.minor= element_blank(),
    panel.grid.major.x= element_blank(),
    axis.line = element_line(colour = "grey40"),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p0
ggplot2::ggsave(filename= paste('./#4_figure/','trend_overall_legend','.jpg',sep=''),plot = p0,
                path = NULL, scale = 1, 
                width = 24, height = 8, units = c( "cm"), dpi = 1000)



for (i in 1:K){
  trend_relative[i,] = trend_relative[i,]/ max( trend_relative[i,])
}

#trend_relative<-trend_relative[topic_index,]#########################
trend_relative['topic'] <- paste('Topic_',1:15,sep='')
trend_relative <- melt(trend_relative); colnames(trend_relative) <- c('topic','year','value'); trend_relative$year <- as.numeric(as.character(trend_relative$year))
trend_relative$topic <- factor (trend_relative$topic,levels = paste('Topic_',1:15,sep=''))
ggplot(data = trend_relative, aes(x=year,y=value, colour=topic)) + geom_line()



############################## sub plot  #########
savesub<-function(name,plotP,height){
  ggplot2::ggsave(filename= paste('./#4_figure/sub_',name,'.jpg',sep=''),plot = plotP,
                  path = NULL, scale = 1, 
                  width = 24, height = height, units = c( "cm"), dpi = 1000)
}



myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral"))) # 11 is maxium number, break spectral colour into 11 range

plotHeatBar <- function(topicIndex,startYear = 1850,last=FALSE,name = 'unamed',height=2,input=trend){
  height = height
  #topicIndex <- c(10,12)
  tempTopic <- paste('Topic_', topicIndex, sep='')
  tempData <- input[input$topic %in% tempTopic ,]
  tempData <- tempData[tempData$year >=startYear,]
  tempData$topic <- factor(tempData$topic, levels = rev(tempTopic) )
  maxP <- round(max(tempData $value),2)+0.01
  print (maxP)
  if (maxP>=1){maxP <- 1}
  # colour setting !!!!
  #myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral"))) # 11 is maxium number, break spectral colour into 11 range
  sc <- scale_fill_gradientn(colours = myPalette(100), # break range into 100 colours 
                             limits=c(0, maxP)) 
  # plot
  
  tempP <- ggplot(tempData, aes(year, topic)) +
    geom_tile(aes(fill = value)) +
    sc+
    #scale_fill_gradient(low = '#deebf7', high = "#de2d26",na.value = "transparent",breaks=c(0,maxP),labels=c("0",maxP),
                        #limits=c(0,maxP)) +
    scale_x_continuous(breaks=seq(min(tempData$year),max(tempData$year),by=10),expand = c(0,0) )+
    scale_y_discrete (expand=c(0,0))+
    labs(x=NULL,y=NULL)+
    # set theme
    theme(#plot.title=element_text(size=rel(2)),
      #axis.title.x=element_blank(),
      axis.text.x =element_text(size=8,colour='black'),
      axis.text.y=element_text(size=8,colour='black'),
      #legend.title = element_text(size=4,colour='black'),
      #legend.position='none',
      #legend.title=element_blank(),
      #legend.text=element_text(size=10,angle = 0),
      legend.direction='horizontal',
      legend.key.size = unit(0.5, "cm"),
      panel.background=element_blank(),
      axis.line = element_line(colour = "grey40"),
      panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  #savesub(name,tempP,height)
  return (tempP)
}

print(col_old)
trend_type <- trend_relative # trend_type <- trend
print (topic_index)
war <- plotHeatBar(c(1,2,3),name='war_legend',height = 2,input=trend_relative); war
nuclear <- plotHeatBar(c(4),startYear = 1950, height = 1.2,input=trend_relative); nuclear
health <- plotHeatBar(c(5,6,7,8,9),name = 'health',startYear = 1950,height=3,input=trend_relative); health
hiv <- plotHeatBar(c(10,11),name = 'hiv',startYear = 1950,height = 1.5,input=trend_relative); hiv
finance <- plotHeatBar(c(13,14),name = 'finance',startYear = 1950,height = 1.5,input=trend_relative); finance
social <- plotHeatBar(c(12),name = 'social issue',startYear = 1950,height = 1.5,input=trend_relative); social
unidentified <- plotHeatBar(c(12),name = 'unidentified',startYear = 1950,height = 1.5,input=trend_relative); unidentified

t15 <- plotHeatBar(c(15),name='war_legend',height = 2,input=trend_relative); t15

###### AIDS stats #####
aids <- read.csv('./data/AIDS DEATH RATE.csv')
aids <- aids[,c('Year','total')]

add <- data.frame(seq(1950,1980),rep(0,length( seq(1950,1980))))
colnames(add)<-c('Year','total')
aids <- rbind(add,aids)
sc <- scale_fill_gradientn(colours = myPalette(100), # break range into 100 colours 
                           limits=c(0, maxP)) 
aidsP <- ggplot(aids, aes(Year,total)) + 
  #geom_bar(stat='identity',position = position_dodge(width = 0))+
  geom_bar(stat='identity')+
  theme_bw() + labs(x='Number',y=NULL,title='')+
  labs(x=NULL)+
  scale_x_continuous(breaks = seq(min(nuke_sum$year), max(nuke_sum$year), by = 5),expand=c(0,0))+
  #scale_fill_gradientn(colours = myPalette(10), # break range into 100 colours 
  #                     limits=c(0, max(nuke_sum$inv))) +
  theme(plot.title = element_text(size=8,hjust=0),
        panel.grid.minor= element_blank(),
        panel.grid.major.x= element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        #panel.grid.major.y = element_line( size=.1, color="black" ) ,
        axis.text = element_text(size=8),
        legend.direction='horizontal',
        legend.title = element_blank(),
        legend.text = element_text(size=8))
aidsP

ggplot2::ggsave(filename= paste('./#4_figure/sub_','aids','.jpg',sep=''),plot = aidsP,
                path = NULL, scale = 1, 
                width = 24, height = 3, units = c( "cm"), dpi = 1000)


######## Nuclear Stats ########
nuke <- read.csv('./data/nuclear_inventory_1945-2014.csv')
nukeTest <- read.csv('./data/nuclear_test_1945-1998.csv')
str(nuke)
colnames(nuke) <- c('country','year','code','inventory')
colnames(nukeTest) <- c('country','year','code','test')
nuke <- nuke[nuke$year<=2008,]
nuke <- nuke[nuke$year>=1950,]
library(plyr)
nuke_sum <- ddply(nuke,c('year'),summarise,inv=sum(inventory))

#nuke <- nuke[nuke$code %in% c('RUS','USA'),]
#nuke$code <- factor(nuke$code,labels = c('USSR','USA'))
#nuke_sum <- ddply(nuke,c('year','code'),summarise,inv=sum(inventory))
#nuke_sum <- ddply(nukeTest,'year',summarise,inv=sum(test))

sc <- scale_fill_gradientn(colours = myPalette(100), # break range into 100 colours 
                           limits=c(0, maxP)) 
nukeP <- ggplot(nuke_sum, aes(year,inv)) + 
  #geom_bar(stat='identity',position = position_dodge(width = 0))+
  geom_bar(stat='identity')+
  theme_bw() + labs(x='Number',y=NULL,title='Nuclear Weapon Inventory')+
  labs(x=NULL)+
  scale_x_continuous(breaks = seq(min(nuke_sum$year), max(nuke_sum$year), by = 5),expand=c(0,0))+
  #scale_fill_gradientn(colours = myPalette(10), # break range into 100 colours 
  #                     limits=c(0, max(nuke_sum$inv))) +
  theme(plot.title = element_text(size=8,hjust=0),
        panel.grid.minor= element_blank(),
        panel.grid.major.x= element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        #panel.grid.major.y = element_line( size=.1, color="black" ) ,
        axis.text = element_text(size=8),
        legend.direction='horizontal',
        legend.title = element_blank(),
        legend.text = element_text(size=8))
nukeP

ggplot2::ggsave(filename= paste('./#4_figure/sub_','nuke','.jpg',sep=''),plot = nukeP,
                path = NULL, scale = 1, 
                width = 24, height = 3, units = c( "cm"), dpi = 1000)
