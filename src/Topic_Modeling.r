library(tidyverse)
library(tm)
library(proxy)
library(cluster)
library(udpipe)

load("data/Data_Preparation_Image.RData")

## Clustering
library(proxy)
library(cluster)

inspect(tdm)

par(mfrow=c(5,2))
par(mar=c(2.5,2.5,2.5,2.5))
for (k in seq(0.8,0.98,0.02)) {
    dst.tdm <- dist(as.matrix(removeSparseTerms(tdm, k)), method = "cosine")
    n_lemmas <- attr(dst.tdm, "Size")
    hcl.tdm <- hclust(dst.tdm, method = "ward.D2") # HERE WE CAN ALSO CHOOSE THE METHOD.
    vsilIn <- c()
    for (i in 2:20) {
        vsilIn <- c(vsilIn, mean(silhouette(x = cutree(hcl.tdm, i), dist = dst.tdm)[, 3]))
    }

    plot(x = seq(2:20), y = vsilIn,   
        type = "b",
        xlab = "Number of clusters", 
        ylab = "Silhouette", 
        main = paste0("k = ", k, " - Number of total lemmas: ", n_lemmas)) 
}

attr(dst.tdm, "Size")

k <- 0.96 # Sparsity threshold. Only consider terms with frequency > n_documents * (1 - k). n_documents should also be the number of columns in the matrix.
nclu <- 10
tdm.r <- removeSparseTerms(tdm, k)
dst.tdm <- dist(as.matrix(tdm.r), method = "cosine")
hcl.tdm <- hclust(dst.tdm, method = "ward.D2") # HERE WE CAN ALSO CHOOSE THE METHOD.
hcl.class <- cutree(hcl.tdm, k = nclu)
table(hcl.class)

par(mar=c(0,0,0,0))
par(mfrow=c(1,1))

plot(hcl.tdm, hang= -1,sub = "", cex=0.5, main="HierarchicalCluster\ncosine-Ward",
    cex.main=0.9, xlab="", cex.axis=0.6, cex.lab=0.2)
    rect.hclust(hcl.tdm,k = nclu)


df_words.r <- df_words %>% filter(word %in% rownames(tdm.r))

# Frequenza delle parole in ogni cluster
df_words.r$cluster <- hcl.class

# Stampa le parole più caratterizzanti per ogni cluster
characterizing_words


# Cluster documents
dst.d <- dist(mdtm, method = "cosine")
hcl.d <- hclust(dst.d, method = "ward.D2")
plot(hcl.d, hang = -1, cex = 0.8, labels = FALSE)
rect.hclust(hcl.d, k = 3) #cluster of the documents

my.silhouette.score <- function(hcl, dist, min.clust=2, max.clust=20){
    require(cluster)
    vSilIn <- c(); vnclus <- c()
    for (i in min.clust:max.clust){
        vSilIn <- c(vSilIn, silhouette(cutree(hcl,i), dist = dist)[,3] %>% mean())
        vnclus <- c(vnclus,i)
    }
    res = data.frame(n.clusters = vnclus, silhouette = vSilIn)
    return(res)
}

my.silhouette.score(hcl.d, dst.d, 2, 20) %>%
    ggplot(aes(x = n.clusters, y = silhouette)) +
    geom_line() +
    geom_point() +
    theme_light()

table(cutree(hcl.d, 6))

# we can use a comparison cloud to understand which are the terms characterizing two different clusters

df_clean <- df_clean %>% mutate(cluster = cutree(hcl.d, 6))

df_cluster <- df_clean %>% select(cluster, text) %>% group_by(cluster) %>% summarise(text = paste(text, collapse = " "))

crp_2 <- Corpus(VectorSource(df_cluster$text))
tdm_2 <- tm::TermDocumentMatrix(crp_2)
mtdm_2 <- as.matrix(tdm_2)

library(wordcloud)
par(mar=c(2.5,2.5,2.5,2.5))
comparison.cloud(mtdm_2, scale = c(2, 0.5), max.words = 50, colors = RColorBrewer::brewer.pal(6, "Set2"), title.size = 1, match.colors = T)


mtdm_2.idf <- as.matrix(tm::weightTfIdf(tdm_2))
par(mar=c(0,0,0,0))
comparison.cloud(mtdm_2.idf, scale = c(3, 0.3), max.words = 80, colors = RColorBrewer::brewer.pal(6, "Set1"), title.size = 1, match.colors = T) # we can use the tf-idf matrix to get more meaningful results

# if a word is very frequent in a document, but not in the corpus, it is probably a word that characterizes that document, and it will have a high tf-idf score.
# if a word is very frequent in the corpus, but not in a document, it is probably a word that does not characterize that document, and it will have a low tf-idf score.



################### SENTIMENT ANALYSIS #######################

source("utility_sentiment.R")

txt_it <- c("bello testo prova veloce analisi sentimento",
 "questo brutto cattivo testo")
 load("mySntIT.RData")
 syu_it <- my_get_nrc_sentiment(char_v = txt_it, lexicon = mySntIT)
