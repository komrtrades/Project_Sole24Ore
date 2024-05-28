library(dplyr)
library(ggplot2)
library(udpipe)
library(wordcloud)
library(RColorBrewer)



load("data/Data_Preparation_Image.RData")


# QUali sono i termini più utilizzati?
mtdm %>% rowSums() %>% sort(decreasing = TRUE) %>% head(10)

df_words %>% arrange(-freq) %>% slice(1:20) %>% 
    ggplot(aes(x = reorder(word, freq), y = freq)) +
    geom_col() +
    coord_flip() +
    theme_light() +
    labs(x = "Word", y = "Frequency")

# Quali sono i termini più significativi?
mtdm.w %>% rowSums() %>% sort(decreasing = TRUE) %>% head(10)

df_words %>% arrange(-weight) %>% slice(1:20) %>% 
    ggplot(aes(x = reorder(word, weight), y = weight)) +
    geom_col() +
    coord_flip() +
    theme_light() +
    labs(x = "Word", y = "Weight")


# Find Associations
library(tm)
findFreqTerms(tdm,50)
findAssocs(tdm, "banca", corlimit = 0.5)

## Wordcloud
par(mfrow=c(1,2))

wordcloud(words=df_words$word,
 freq=df_words$freq,
 scale=c(2.3,0.4),
 max.words =80,
 random.order =FALSE,
 colors= brewer.pal(8,"Dark2"))
 text(0.5,1,"Wordcloud of Sole 24 Ore Financial News - Freq",cex=1,font= 2)

wordcloud(words=df_words$word,
 freq=df_words$weight,
 scale=c(1.5,0.3),
 max.words =80,
 random.order =FALSE,
 colors= brewer.pal(8,"Dark2"))
 text(0.5,1,"Wordcloud of Sole 24 Ore Financial News - Weight",cex=1,font= 2)

