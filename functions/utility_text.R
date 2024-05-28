
# my.encoding.conv to encode text to UTF-8
# visNGram      display and save n-grams
# corMultWord   read the xlsx file produced by visNGram function and replace multiwords
# corFrmComp    correzione forme composte
# multWordPOS   identification of multiwords using pos-tagging
# lemmaUDP      lemmatization with UDpipe

my.encoding.conv <- function(x=NULL, enc.to="UTF-8"){
  # under development
  # encoding text
  # x       text vector
  # enc.to  destination encoding
  library(stringi,quietly = T,warn.conflicts = F)
  if(is.null(x)) stop("missing x")
  tmp <- x
  enc.lst <- stri_enc_detect(x)
  for(i in 1:length(enc.lst)){
    if(enc.lst[[i]][1,1] != enc.to){
      tmp[i] <- stri_conv(x[i],from = enc.lst[[1]][1,"Encoding"],to = enc.to)
    }
  }
  txt.out <- gsub("\031","'",tmp)
  txt.out <- gsub("\023","-",txt.out)
  txt.out <- gsub("\034","\"",txt.out)
  txt.out <- gsub("\035","\"",txt.out)
  return(txt.out)
}


visNGram <- function(x = NULL, ngrI = 2, ngrF = 3 ,nn = 20, 
                     show.table = TRUE, save.xlsx = TRUE, xlsx.name = NULL){
  # Function to display and save n-grams
  # x = vector of texts
  # ngrI = minimum value n-gram length (default = 2)
  # ngrF = maximum value of length n-grams (default = 3)
  # nn = number of n-grams to display for each length
  # show.table = logical, if TRUE display the results
  # save.xlsx = logical, if TRUE the results will be saved in a xlsx file
  # xlsx.name = if save.xlsx = TRUE, the name of the xlsx file in which to save the results, 
  #             if not indicated the default file is outNgram.xlsx
  #  require(ngram); require(dplyr); require(xlsx)
  require(ngram); require(dplyr); require(openxlsx)
  if(is.null(x) | class(x) != "character"){stop("invalid text vector")}
  if(ngrI < 2){stop("invalid n-gram initial value")}
  if(ngrF < ngrI){stop("invalid n-gram values: ngrF < ngrI")}
  ngr <- seq(from = ngrI, to = ngrF, by = 1)
  vnWrd <- sapply(x,FUN = wordcount)
  ngr <- rev(ngr)
  dfNGram <- data.frame(ngrams=as.character(),freq=as.numeric(),prop=as.numeric())
  for(i in 1:length(ngr)){
    minc <- ngr[i]-1
    ng <- ngram(x[vnWrd>minc], n=ngr[i])
    ngt <- get.phrasetable(ng)
    ngt <- ngt %>% arrange(-freq, ngrams)
    ngt$ngrams <- iconv(ngt$ngrams,from="UTF-8",to = "latin1",sub = "byte")
    ngt$ngrams <- trimws(ngt$ngrams)
    dfNGram <- bind_rows(dfNGram,ngt)
    dfNGram$edit <- NA
    if(show.table==TRUE){
      if(nrow(ngt)>=nn){nnr=nn} else {nnr=nrow(ngt)}
      cat("--------------------","\n")
      cat("n-gram n = ",ngr[i],"\n")
      print(ngt[1:nnr,])
    }
  }
  if(save.xlsx == TRUE){
    if(is.null(xlsx.name)==TRUE){xlsx.name <- "outNgram.xlsx"}
    write.xlsx(dfNGram,file = xlsx.name,row.names = F,showNA = FALSE)
  }
}

corMultWord <- function(x = NULL, xlsx.file = NULL, replace.char = "_"){
  # read the xlsx file produced by visNGram function and
  # replace multiwords defined in the field 'edit' of the xlsx file
  # if in edit "=" the spaces will be replaced with the replace.char
  # otherwise with the string reported in edit
  # x = vector of texts
  # xlsx.file = xlsx file produced by visNGram function with edit to multiwords
  # replace.char = replacement chararacter to use 
  if(is.null(x) | is.null(xlsx.file)){stop("invalid argument")}
  if(class(x) != "character" | class(xlsx.file) != "character"){stop("invalid argument")}
  require(openxlsx)
  require(dplyr)
  corrw <- read.xlsx(xlsx.file,sheet = 1)
  if(class(corrw) != "data.frame"){stop("invalid xlsx file")}
  corr1 <- corrw %>% filter(edit=="=")
  if(nrow(corr1)>0){
    for(i in 1:nrow(corr1)){
      xcor <- gsub(" ",replace.char,corr1[i,1])
      x <- gsub(corr1[i,1],xcor,x)
    }
  }
  corr2 <- corrw %>% filter(!is.na(edit) & edit!="=")
  if(nrow(corr2)>0){
    for(i in 1:nrow(corr2)){
      x <- gsub(corr2[i,1],corr2$edit[i],x)
    }
  }
  return(x)
}

corFrmComp <- function(vText=NULL, correzioni=NULL){ # from=NULL, to=NULL
  # funzione per correzione forme composte
  # sostanzialmente effettua i gsub di correzione dopo visNGram
  # vText = vettore di testi su cui effettuare la correzione
  # correzioni = vettore di lunghezza pari in cui si susseguono le correzioni da fare
  #              forma.da.correggere.1, forma.corretta.1, forma.da.correggere.2, forma.corretta.2,...
  #              se la forma.corretta richiede semplicemente la sostiuzione degli spazi con _
  #              la forma.corretta può essere indicata con NA
  # esempio: correz <- c("buona salute",NA, "carta di credito","carta_credito","partita iva", NA)
  # tw$txt <- corFrmComp(vText = tw$txt, correzioni = correz)
  if(is.null(vText) | is.null(correzioni)){
    message("mancano vettore testi o vettere forme da correggere")
    return()
  }
  if(length(correzioni) %% 2 != 0){
    message("la lunghezza del vettore correzioni non è corretta")
    return()
  }
  from <- correzioni[seq(from=1,to=length(correzioni),by = 2)]
  to <- correzioni[seq(from=2,to=length(correzioni),by = 2)]
  for(i in 1:length(from)){
    if(to[i]=="" | is.na(to[i])){
      to[i] <- gsub(" ","_",from[i])
    }
    vText <- gsub(from[i],to[i],vText)
  }
  return(vText)
}

multWordPOS <- function(x = NULL, model = "english-ewt", n = 100, save.xlsx = TRUE, xlsx.name = NULL){
  # function to identify multiwords using pos-tagging
  # x = vector of texts
  # model = udpipe Pre-trained model 
  #         english models: english-ewt, english-gum, english-lines, english-partut,
  #         italian models: italian-isdt, italian-partut, italian-postwita, italian-twittiro, italian-vit
  # save.xlsx = logical, if TRUE the results will be saved in a xlsx file
  # xlsx.name = if save.xlsx = TRUE, the name of the xlsx file in which to save the results, 
  #             if not indicated the default file is outNgramPOS.xlsx
  require(udpipe); require(dplyr); require(openxlsx)
  if(is.null(x) | is.null(model)){stop("invalid argument")}
  if(class(x) != "character" | class(model) != "character"){stop("invalid argument")}
  out <- udpipe(x, object = model)
  AA2 <- bind_cols(tok=txt_nextgram(x=out$token, n = 2, sep = " "),
                   upo=txt_nextgram(x=out$upos, n = 2, sep = " ")) %>% 
    filter(upo %in% c("NOUN NOUN","ADJ NOUN","PROPN PROPN")) %>% 
    group_by(tok,upo) %>% summarise(nn=n(), .groups = 'drop') %>% arrange(-nn)
  AA3 <- bind_cols(tok=txt_nextgram(x=out$token, n = 3, sep = " "),
                   upo=txt_nextgram(x=out$upos, n = 3, sep = " ")) %>% 
    filter(upo %in% c("NOUN NOUN NOUN","ADJ NOUN NOUN","ADJ ADJ NOUN","PROPN PROPN PROPN",
                      "NOUN ADP NOUN","PROPN ADP PROPN")) %>% 
    group_by(tok,upo) %>% summarise(nn=n(), .groups = 'drop') %>% arrange(-nn)
  results <- bind_rows(AA2,AA3) %>% arrange(-nn) %>% slice_head(n=n) 
  colnames(results) <- c("tokens","pos","freq")
  results$edit <- NA
  if(save.xlsx==TRUE){
    if(is.null(xlsx.name)==TRUE){xlsx.name <- "outNgramPOS.xlsx"}
    write.xlsx(as.data.frame(results),file = xlsx.name,row.names = F,showNA = FALSE)
  }
  return(results)
}

lemmaUDP <- function(x = NULL, 
                     model = NULL, 
                     doc_id = NULL, 
                     stopw = tm::stopwords("english"), 
                     userstopw=NULL){
  # function for lemmatization with UDpipe.
  # returns a data frame in CoNLL-U format. 
  # with the addition of the STOP field identifying stopwords.
  # x = vector of texts/documents in UTF-8 format
  # model = lemmatization model
  # doc_id = document identifier
  # stopw = language stopwords list
  #         stopw = NULL to not report stopwords
  # userstopw = user-defined stopwords list.
  require(udpipe)
  if(is.null(x)){message("missing text vector");return()}
  if(is.null(model)){message("missing model");return()}
  if(class(x) != "character"){message("vector x is not character type");return()}
  if(class(model) != "udpipe_model"){message("invalid model");return()}
  if(is.null(doc_id)){doc_id <- 1:length(x)}
  if(!is.null(userstopw)){
    stopw <- c(stopw,userstopw)
  }
  xx <- udpipe_annotate(model, x = x, doc_id = doc_id,trace = F)
  xx <- as.data.frame(xx)
  xx$STOP <- ifelse(tolower(xx$lemma) %in% tolower(stopw) | tolower(xx$token) %in% tolower(stopw),TRUE,FALSE)
  return(xx)
}

