#There are three ways to find associations between words: cluster analysis, word networks, and topic modelling.

# 1. cluster analysis
# We compute the distances between the terms, and group units that are close to each other.
# The choice of distance measure can significantly influence the outcome of the clustering algorithms. 
# The most used distance measures in text mining are: Cosine, Pearson, and Jaccard.
# dist function in the proxy library calculates distances.

load("data/Data_Preparation_Image.RData")

library(tidyverse)
library(udpipe)
library(tm)
library(proxy)
library(cluster)
library(lsa)
library(textmineR)
# library(NMF)
library(EFA.dimensions)
library(FactoMineR)
library(topicmodels)
library(LDAvis)
library(BTM)
library(textplot)

source("./functions/utility_topics.R")

par(mfrow=c(1,1))

inspect(tdm) # Distance Matrix

# dst.T <- dist(mtdm, method = "cosine")

# # total number of terms
# as.matrix(dst.T) %>% dim()


tdm.r <- removeSparseTerms(tdm, 0.95) # Only consider terms with frequency > n_documents * (1 - 0.95). n_documents should also be the number of columns in the matrix.
mtdmr <- as.matrix(tdm.r)

dst.r <- dist(mtdmr, method = "cosine")
hcl.r <- hclust(dst.r, method = "ward.D2")
plot(hcl.r, hang = -1, cex = 0.8, labels = FALSE)

# an empirical way to choose the number of clusters is to cut the dendogram at the end of the longest vertical line from the origin.
rect.hclust(hcl.r, k = 5)


# a better way is using silhouette score
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

my.silhouette.score(hcl.r, dst.r, 2, 50) %>%
    ggplot(aes(x = n.clusters, y = silhouette)) +
    geom_line() +
    geom_point() +
    theme_light()

plot(hcl.r, hang = -1, cex = 0.8, labels = FALSE)
rect.hclust(hcl.r, k = 14)


clu.c <- cutree(hcl.r, k = 14)
table(clu.c)

vtop = c()
for (i in 1:max(clu.c)){
    vtop = c(vtop, paste(rownames(mtdmr)[clu.c == i], collapse = ", "))
} # This shows cluster of words
vtop %>% as_tibble()

dst.d <- dist(mdtm, method = "cosine")
hcl.d <- hclust(dst.d, method = "ward.D2")
plot(hcl.d, hang = -1, cex = 0.8, labels = FALSE)
rect.hclust(hcl.d, k = 4) #cluster of the documents

my.silhouette.score(hcl.d, dst.d, 2, 20) %>%
    ggplot(aes(x = n.clusters, y = silhouette)) +
    geom_line() +
    geom_point() +
    theme_light()

nclu_doc <- 4
table(cutree(hcl.d, nclu_doc))

# we can use a comparison cloud to understand which are the terms characterizing two different clusters

df_clu <- df_clean %>% 
    mutate(cluster = cutree(hcl.d, nclu_doc), 
           text = strsplit(text, " ")) %>% 
    unnest(text) %>% 
    filter(text != "") %>% 
    select(cluster, text) %>% 
    group_by(cluster) %>% 
    summarise(text = paste(text, collapse = " "))


df_clu <- df_clu %>% group_by(cluster) %>% summarise(text = paste(ifelse(!is.na(text),text)))
df_clu

crpC <- Corpus(VectorSource(df_clu$text))
tdmC <- tm::TermDocumentMatrix(crpC)
mtdmC <- as.matrix(tdmC)

library(wordcloud)
par(mar=c(0,0,0,0))
comparison.cloud(mtdmC, scale = c(3, 0.3), max.words = 80, colors = RColorBrewer::brewer.pal(6, "Set1"), title.size = 1, match.colors = T)


mtdmC.idf <- as.matrix(tm::weightTfIdf(tdmC))
par(mar=c(0,0,0,0))
comparison.cloud(mtdmC.idf, scale = c(3, 0.3), max.words = 80, colors = RColorBrewer::brewer.pal(6, "Set1"), title.size = 1, match.colors = T) # we can use the tf-idf matrix to get more meaningful results

# if a word is very frequent in a document, but not in the corpus, it is probably a word that characterizes that document, and it will have a high tf-idf score.
# if a word is very frequent in the corpus, but not in a document, it is probably a word that does not characterize that document, and it will have a low tf-idf score.


# Another way is to use cooccurrence networks, where the nodes are the words, and the edges are the cooccurrences between words in the same document.
# We can use the cooccurrence networks to find the most important words in a document, or to find the most important documents in a corpus.
# Cooccurrency means that two words appear in the same document.

df_l <- x %>% 
        select(doc_id, sentence_id, lemma) %>% 
        group_by(doc_id, sentence_id)

cooc <- udpipe::cooccurrence(df_l$lemma)

cooc %>% head(20)
textplot::textplot_cooccurrence(cooc)


library(igraph)
library(ggraph)
library(ggplot2)
wordnet <- head(cooc,300)
wordnet <- graph_from_data_frame(wordnet, directed = FALSE)
par(mar=c(0,0,0,0))

ggraph(wordnet, layout = "kk") + 
    geom_edge_link(aes(width = cooc, alpha = cooc), edge_colour = "grey50") + 
    geom_node_point(size = 1) + 
    geom_node_text(aes(label = name), size = 3) + 
    theme_graph(base_family = "sans", title_size = 12, background = "gray99") +
    theme(legend.position = "none") + 
    labs(title = "Cooccurrence network") +
    theme_void()


V(wordnet)$size <- (degree(wordnet)/max(degree(wordnet))) * 20
cl.LO2 <- cluster_louvain(as.undirected(wordnet))
plot(wordnet, layout = layout.fruchterman.reingold(wordnet), vertex.label.cex = 0.6, vertex.color = cl.LO2$membership, vertex.size = V(wordnet)$size)



# LDA
library(LDAvis)
lda1 <- LDA(mdtm, k = 20, method = "Gibbs") # Applico la LDA
str(lda1) # Visualizzo la struttura dell'oggetto

post.lda1 <- posterior(lda1) # Estraggo le probabilità posteriori
str(post.lda1)
post.lda1$terms[1:10,1:5] # Probabilità delle prime 10 parole di appartenere a uno dei primi 5 topic
post.lda1$topics[1:10,1:5] # Probabilità dei primi 10 documenti di appartenere a uno dei primi 5 topic

textmineR::GetTopTerms(phi = post.lda1$terms, M = 10)
# Questi sono i termini più probabili per i primi 10 topic