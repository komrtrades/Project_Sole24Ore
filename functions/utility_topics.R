# my.TopTermsLSA
# my.descfreq
# topicmodels_json_ldavis

vPackg <- c("EFA.dimensions","FactoMineR","topicmodels","stringi","tm","LDAvis","dplyr")
if(sum(vPackg %in% as.data.frame(installed.packages(.libPaths()))[,1]) != length(vPackg)){
  manc=vPackg[vPackg %in% as.data.frame(installed.packages(.libPaths()))[,1]==F]
  warning(paste("deve essere installata la libreria:",manc))
}

my.TopTermsLSA <- function(x = NULL, M = 5, doc.id = NULL){
  # extract terms per topic
  # assign topic to documets
  # return a list with a matrix with terms per topic and a data.frame with the topic per document
  # x:      LSAspace object (from lsa::lsa)
  # M:      number of terms to return per topic
  # doc.id: documents identifier
  require(EFA.dimensions)
  if(is.null(x) | !class(x) %in% c("LSAspace","lsa_topic_model")){stop("not LSAspace")}
  if(is.null(doc.id)){stop("missing doc.id")}
  if(M>50){message("maximum value of M = 50");M=50}
  if(class(x)=="LSAspace"){
    U <- x$tk
    V <- x$dk
    S <- diag(x$sk)
  }
  if(class(x)=="lsa_topic_model"){
    U <- x$theta
    V <- t(x$phi)
    S <- diag(x$sv)
  }
  if(length(doc.id) != nrow(U)){stop("invalid doc.id")}
  # calcolo loadings
  lsa.term.loadings <- V %*% S
  lsa.doc.loadings <- U %*% S
  # rotazione matrici
  lsa.term.loadings.rot <- VARIMAX(loadings = lsa.term.loadings, normalize = T,verbose = F)
  lsa.doc.loadings.rot <- VARIMAX(loadings = lsa.doc.loadings, normalize = T,verbose = F)
  for(i in 1:ncol(lsa.term.loadings.rot$loadingsV)){
    bb <- boxplot(lsa.term.loadings.rot$loadingsV[,i],range=2,plot=F)
    tmpt <- bb$out %>% as.data.frame() %>% tibble::rownames_to_column("term") %>% mutate(topic=i)
    colnames(tmpt)[2] <- "loading"
    if(i==1){tpc.lsa1 <- tmpt} else {tpc.lsa1 <- bind_rows(tpc.lsa1,tmpt)}
  }
  # seleziono gli M termini più rilevanti, in termini assoluti, per topic
  lsa.term.topic.30 <- tpc.lsa1 %>% mutate(assval=abs(loading)) %>% group_by(topic) %>% top_n(50,assval) %>% arrange(topic,-assval)
  mTopLSA <- matrix(nrow = M,ncol = max(lsa.term.topic.30$topic))
  for(i in 1:max(lsa.term.topic.30$topic)){
    tmp <- lsa.term.topic.30 %>% filter(topic==i) %>% slice(1:M)
    mTopLSA[,i] <- tmp$term
  }
  # documents
  tmp.doc.ld <- lsa.doc.loadings.rot$loadingsV
  rownames(tmp.doc.ld) <- doc.id
  # assegno i documenti al topic con il loading maggiore
  lsa.doc.topic <- apply(abs(tmp.doc.ld),1,which.max) %>% as_tibble() %>% mutate(id=doc.id,.before=1)
  colnames(lsa.doc.topic)[2] <- "topic.lsa"
  
  return(list(lsa.term.topic=mTopLSA,lsa.doc.topic=lsa.doc.topic))
}

my.descfreq <- function(x=NULL,cluster=NULL,prob=0.01){
  # funzioni per calcolo e lettura dei risultati test.value
  # x        data.frame con la matrice documenti-termini
  # cluster  vettore con i cluster di appartenenza delle unità di x
  # prob     livello di probabilità rispetto a cui selezionare i termini
  require(FactoMineR)
  if(is.null(x)|is.null(cluster)){stop("paramentri mancanti")}
  tmp.clu <- aggregate(x,by = list(cluster), sum)
  t_tmp.clu <- t(tmp.clu[,-1])
  colnames(t_tmp.clu) <- paste("clu",str_pad(tmp.clu$Group.1, 2, pad = "0"),sep="")
  df.t_tmp.clu <- as.data.frame(t_tmp.clu)
  y = descfreq(donnee = df.t_tmp.clu,proba = prob)
  pb <- txtProgressBar(min = 1,max = length(x),style = 3)
  for(i in 1:length(y)){
    setTxtProgressBar(pb, i)
    # print(i)
    tmp <- as.data.frame(y[[i]]) %>% rownames_to_column("clust") %>% mutate(term=names(y)[i])
    if(i==1){
      dfx=tmp
    } else {
      dfx=rbind(dfx,tmp)
    }
  }
  close(pb)
  dfx <- dfx %>% mutate(cluster=as.numeric(substr(clust,4,15))) %>% 
    select(term,cluster,v.test,p.value,Inter.prc='Intern %',Glob.prc='glob %',Inter.freq='Intern freq',Glob.freq='Glob freq ')
  return(dfx)
}


topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # prepares LDA output for LDAvis
  # fitted: topicmodel's LDA function output
  # corpus: object created using the tm package's Corpus function
  # doc_term: document term matrix created using DocumentTermMatrix function
  # return an object to use with function serVis
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  # Find required quantities
  phi <- topicmodels::posterior(fitted)$terms %>% as.matrix
  theta <- topicmodels::posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  #  temp_frequency <- inspect(doc_term)
  temp_frequency <- as.matrix(doc_term) 
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}
