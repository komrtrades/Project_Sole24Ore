
vPackg <- c("svMisc","tm","syuzhet","dplyr")
if(sum(vPackg %in% as.data.frame(installed.packages(.libPaths()))[,1]) != length(vPackg)){
  manc=vPackg[vPackg %in% as.data.frame(installed.packages(.libPaths()))[,1]==F]
  warning(paste("deve essere installata la libreria:",manc))
}

# create_matrix        creazione matrice TermDocuments per funzioni myClassEmotion e myClassPolarity
# myClassEmotion       calcolo emotions con metodo NaiveBayes
# myClassPolarity      calcolo polarity con metodo NaiveBayes
# my_get_nrc_sentiment calcolo emotions e polarity con algoritmo syuzhet
# sentiMediaDiz        calcolo polarità con funzione txt_sentiment di udpipe su una lista di dizionari definiti dall'utente
# require(tm)
# require(svMisc)
# require(syuzhet)
# require(udpipe)
# require(dplyr)
# options(dplyr.summarise.inform = FALSE)


create_matrix <- function(textColumns, language="italian", minDocFreq=1, minWordLength=3, 
                          removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, 
                          removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, 
                          weighting=weightTf) {
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(split[[1]],language=language))
  }
  require(tm)
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  gc()
  return(matrix)
}


myClassEmotion <- function (textColumns, algorithm = "bayes", prior = 1, lexicon = NULL, verbose = FALSE,...) 
{
#  print("creazione DocumentTermMatrix")
#  cat("\n")
  require(svMisc)
  matrix <- create_matrix(textColumns, ...)
  if(is.null(lexicon)){
    lexicon <- read.csv("emotions_en.csv", header = FALSE)
  } else {
    lexicon <- read.csv(lexicon, header = FALSE)
  }
  ll <- table(lexicon[,2])
  lemo <- dimnames(ll)[[1]]
  counts <- list(length(which(lexicon[, 2] == lemo[1])), 
                 length(which(lexicon[, 2] == lemo[2])), 
                 length(which(lexicon[, 2] == lemo[3])), 
                 length(which(lexicon[, 2] == lemo[4])), 
                 length(which(lexicon[, 2] == lemo[5])), 
                 length(which(lexicon[, 2] == lemo[6])), 
                 nrow(lexicon))
  names(counts) <- c(lemo,"total")
  documents <- c()
  documenti <- data.frame(numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),character(),stringsAsFactors = F)
  colnames(documenti) <- c(lemo,"numParole","best_fit")
  parole <- data.frame(document=numeric(),word=character(),category=character(),score=numeric(),stringsAsFactors = F)
  for (i in 1:nrow(matrix)) {
    if(verbose == TRUE){
      progress(i)
    }
    scores <- list(0, 0, 0, 0, 0, 0)
    names(scores) <- lemo
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    tro <- 0
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[, 2] == key),]
        index <- match(word, emotions[, 1], nomatch = 0)
        if (index > 0) {
          tro <- tro+1
          entry <- emotions[index, ]
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          score <- 1
          if (algorithm == "bayes") 
            score <- abs(log(score * prior/count))
          nr <- dim(parole)[1]+1
          parole[nr,1] <- i
          parole[nr,2] <- word
          parole[nr,3] <- category
          parole[nr,4] <- score
          
          scores[[category]] <- scores[[category]] + score
        }
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    nscores <- unlist(scores)
    if(tro>0){
      mmx <- nscores[which.max(nscores)]
    if (algorithm == "bayes") {
      best_fit <- ifelse(abs((max(nscores[-which.max(nscores)])/mmx)-1)<0.001,"mix",names(scores)[which.max(unlist(scores))])
    } else {
      if(sum(nscores) < 0.0001){
        best_fit <- "NC" 
      } else {
        best_fit <- ifelse(abs((max(nscores[-which.max(nscores)])/mmx)-1)<0.001,"mix",names(scores)[which.max(unlist(scores))])
      }
    }
    } else {
      best_fit <- "NC"
    }
    documenti[i,1] <- scores[[1]]
    documenti[i,2] <- scores[[2]]
    documenti[i,3] <- scores[[3]]
    documenti[i,4] <- scores[[4]]
    documenti[i,5] <- scores[[5]]
    documenti[i,6] <- scores[[6]]
    documenti[i,7] <- tro
    documenti[i,8] <- best_fit
  }
  risult <- list(documents=documenti,words=parole)
  return(risult)
}


myClassPolarity <- function (textColumns, algorithm = "bayes", pstrong = 0.5, pweak = 1, 
                              prior = 1,  lexicon = NULL, verbose = FALSE, ...)   
{
  require(svMisc)
  matrix <- create_matrix(textColumns, ...)
  if(is.null(lexicon)){
    lexicon <- read.csv("subjectivity_en.csv", header = FALSE,stringsAsFactors = T)
  } else {
    lexicon <- read.csv(lexicon, header = FALSE,stringsAsFactors = T)
  }
  lexicon <- lexicon[lexicon$V3 %in% c("negativo","positivo","negative","positive"),]
  lexicon$V3 <- droplevels(lexicon$V3)
  ll <- table(lexicon[,3])
  lemo <- dimnames(ll)[[1]]
  lemo <- c(lemo[which(regexpr("pos",lemo)==1)], lemo[which(regexpr("neg",lemo)==1)])
  counts <- list(length(which(lexicon[, 3] == lemo[1])), 
                 length(which(lexicon[, 3] == lemo[2])), 
                 nrow(lexicon))
  names(counts) <- c(lemo,"total")
  documenti <- data.frame(POS=numeric(),NEG=numeric(),Ratio=numeric(),best_fit=character(),stringsAsFactors = F)
  esteso <- data.frame(document=numeric(),word=character(),category=character(),polarity=character(),score=numeric(),stringsAsFactors = F)
  for (i in 1:nrow(matrix)) {
    if(verbose == TRUE){
      progress(i)
    }
    scores <- list(0, 0)
    names(scores) <- lemo
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    tro <- 0
    for (word in words) {
      index <- match(word, lexicon[, 1], nomatch = 0)
      if (index > 0) {
        tro <- 1
        entry <- lexicon[index, ]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        if (algorithm == "bayes"){ 
          score <- pweak
          if (polarity == "strongsubj"){ 
            score <- pstrong}
          score <- abs(log(score * prior/count))
        } else {
          score <- 1
        }
        
        nr <- dim(esteso)[1]+1
        esteso[nr,1] <- i
        esteso[nr,2] <- word
        esteso[nr,3] <- category
        esteso[nr,4] <- polarity
        esteso[nr,5] <- score
        scores[[category]] <- scores[[category]] + score
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- round(abs(scores[[1]]/scores[[2]]),1)
    if(tro == 0) ratio <- 1
    if (ratio == 1) 
      best_fit <- "neutral"
    documenti[i,1] <- scores[[1]]
    documenti[i,2] <- scores[[2]]
    documenti[i,3] <- ratio # abs(scores[[1]]/scores[[2]])
    documenti[i,4] <- best_fit
  }
  risult <- list(documents=documenti,words=esteso)
  return(risult)
}

# -----------------------------------------------------------------------------------------
# Funzione sentiment da libreria syuzhet per sentiment su dizionario di poalrità personale
#
my_get_nrc_sentiment <- function (char_v, cl = NULL, language = "english", lexicon=NULL){
  require(syuzhet)
  if (!is.character(char_v)) 
    stop("Data must be a character vector.")
  if (!is.null(cl) && !inherits(cl, "cluster")) 
    stop("Invalid Cluster")
  if(is.null(lexicon)){
    lexicon <- dplyr::filter_(nrc, ~lang == language)
  }
  word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
  if (is.null(cl)) {
    nrc_data <- lapply(word_l, get_nrc_values, lexicon = lexicon)
  }
  else {
    nrc_data <- parallel::parLapply(cl = cl, word_l, lexicon = lexicon, 
                                    get_nrc_values)
  }
  result_df <- as.data.frame(do.call(rbind, nrc_data), stringsAsFactors = F)
  df <- data.frame(anger=numeric(),anticipation=numeric(),disgust=numeric(),fear=numeric(),joy=numeric(),
                   sadness=numeric(),surprise=numeric(),trust=numeric(),negative=numeric(),positive=numeric())
  # my_col_order <- c("anger", "anticipation", "disgust", 
  #                   "fear", "joy", "sadness", "surprise", 
  #                   "trust", "negative", "positive")
  # result_df[, my_col_order]
  df_result <- bind_rows(df,result_df)
  return(df_result)
}

sentiMediaDiz <- function(x = NULL,
                          dict = NULL,
                          negators = polarityShifter,
                          amplifiers = intensifier,
                          deamplifiers = weakener){
  # ---------------------------------------------------------------------------------
  # funzione per il calcolo della sentiment su un oggetto prodotto da udpipe_annotate
  # utilizzando una lista di dizionari di sentiment nel formato richiesto dalla
  # funzione txt_sentiment
  # x            data frame ottenuto da una funzione udpipe_annotate o lemma UDP
  # dict         lista contenente i dizionari da utilizzare, ogni elemento della lista
  #              deve avere un nome, ad esempio: mylist=list(Openr=dOpenR,Syuz=dSyuzB)
  # negators     vettore di parole con negazioni
  # amplifiers   vettore di parole con intensificatori positivi
  # deamplifiers vettore di parole con intensificatori negativi
  # questi ultimi tre elementi presentano come default i vettori presenti in IT_stopwwords.RData
  # se non si dispone di questi vettori, o di altri, indicare NULL (ad esempio; negators=NULL)
  #
  # l'output è una lista in cui ci sono i seguenti data frame:
  # data     corrisponde al $data di txt_sentiment, in cui e aggiunta la colonna con il nome del dizionario
  #          ha un numero di righe uguale al numero di righe di x per il numero di dizionari
  # overall  uguale a $overall di txt_sentiment in cui sono aggiunte le colonne con la classe di polarità e il nome del dizionario
  #          il numero di righe è uguale al numero di documenti per il numero di dizionari
  # DizioPol con un numero di righe pari al numero di documenti, presenta una colonna per ogni dizionario utilizzato con i corrispondente valore di sentiment
  # MediaPol con un numero di righe pari al numero di documenti, una colonna: mediaSent con la media dei punteggi di sentiment dei dizionari utilizzati 
  # ---------------------------------------------------------------------------------
  #
  # controllo presenza data frame con pos tagging e lista dizionari
  if(is.null(x) | is.null(dict)){
    message("mancano informazioni")
    return()
  }
  # controllo presenza nomi elementi lista dizionari
  if(length(names(dict)) != length(dict)){
    message("mancano i nomi dei dizionari")
    return()
  }
  esclus <- c(negators,amplifiers,deamplifiers)
  for(i in 1:length(dict)){
    print(names(dict)[i])
    tmp <- txt_sentiment(x = x[,c(1:3,5,6,7,8)],
                         term = "lemma",
                         polarity_terms = dict[[i]] %>% filter(!term %in% esclus),
                         polarity_negators = negators,
                         polarity_amplifiers = amplifiers,
                         polarity_deamplifiers = deamplifiers,
                         constrain = T)
    tmp$data$dizionario <- names(dict)[i]
    tmp$overall$dizionario <- names(dict)[i]
    if(i == 1){
      dfoutD = tmp$data
      dfoutO = tmp$overall
    } else {
      dfoutD = rbind(dfoutD,tmp$data)
      dfoutO = rbind(dfoutO,tmp$overall)
    }
  }
  dfoutO$classPol <- ifelse(dfoutO$sentiment_polarity<0,"Negativo",
                            ifelse(dfoutO$sentiment_polarity>0,"Positivo","Neutro"))
  dfoutO <- as_tibble(dfoutO)
  dfoutD <- as_tibble(dfoutD)
  dfMediaPol <- dfoutO %>% 
    group_by(doc_id) %>% 
    summarise(mediaSent=mean(sentiment_polarity)) %>% 
    mutate(doc_id=as.numeric(doc_id))
  dfDizioPol <- dfoutO %>% 
    select(doc_id,dizionario,sentiment_polarity) %>%
    spread(dizionario,sentiment_polarity) %>% 
    mutate(doc_id=as.numeric(doc_id))
  out <- list(data=dfoutD,overall=dfoutO,DizioPol=dfDizioPol,MediaPol=dfMediaPol)
  return(out)
}
