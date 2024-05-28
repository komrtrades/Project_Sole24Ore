
# ------------- sentiment analysis -------------

library(tidyverse)
library(udpipe)
library(tm)
library(SentimentAnalysis)

source("./functions/utility_updated.R")
source("./functions/utility_sentiment.R")

load("data/Data_Preparation_Image.RData")

# emotions

clemo <- myClassEmotion(textColumns = df_clean$text,
                        algorithm = "bayes",lexicon = "functions/sentiment/emotions_it_lem.csv")
str(clemo)
clemo$documents %>% as_tibble()

table(clemo$documents$best_fit)

clemo$documents%>% group_by(emotion=best_fit) %>% summarise(n=n()) %>% 
mutate(perc.=n/sum(n)*100) %>%  arrange(-n)

clemo$documents%>%  filter(best_fit!= "NC") %>% 
group_by(emotion=best_fit) %>% summarise(n=n()) %>% 
mutate(perc=n/sum(n)*100) %>%
 ggplot(aes(x=reorder(emotion,n),y=perc,fill=emotion))+
 geom_bar(stat= "identity")+coord_flip()+
 theme_light()+xlab("emotions")+ylab("%")+
 theme(legend.position= "none")+
 labs(title= "emotionsdistribution",
 subtitle= "Not Classifiedexcluded")

# use for texts in italian

clpol.it <- myClassPolarity(textColumns = df_clean$text,
                         algorithm = "bayes",
                         lexicon = "functions/sentiment/subjectivity_it_lem.csv")
str(clpol.it)
clpol.it$documents %>% as_tibble()
table(clpol.it$documents$best_fit)

 clpol.it$documents%>% group_by(polarity=best_fit) %>% count() %>% 
ggplot(aes(x=polarity,y=n,fill=polarity))+
 geom_bar(stat= "identity",color="gray50")+
 theme_light()+
 scale_fill_manual(values= c("red","gray30","green"))+
 theme(legend.position= "none")+
 ggtitle("Sentiment polaritydistribution")


 cl.Pol_words<-clpol.it$words%>% group_by(category) %>% 
summarise(txtL=paste(word,collapse= " "))
 corpP<-Corpus(VectorSource(cl.Pol_words$txtL))
 tdmP<-TermDocumentMatrix(corpP)
 tdmP<-as.matrix(tdmP)
 colnames(tdmP) <-cl.Pol_words$category
 library(wordcloud)
 par(mar=c(0,0,0,0))
 comparison.cloud(tdmP,max.words= 120,colors = c("darkorange","darkgreen"),
 scale = c(4,0.3),title.size= 1.5,match.colors = T)
 text(0.5,1,"comparison cloud most important terms in polarity classification")

# ---------- SentimentAnalysis ----------
library(SentimentAnalysis)

snt <- analyzeSentiment(x = df_clean$text)
snt %>% as_tibble()
snt %>% head()

snt.s <- convertToDirection(sentiment = snt)
snt.s %>% as_tibble()
snt.s.Q <- convertToDirection(sentiment = snt$SentimentQDAP)
snt.s.Q %>% as_tibble()
snt.s %>% head()

snt.s$SentimentGI %>% table()
snt.s$SentimentLM %>% table()
snt.s$SentimentQDAP %>% table()
