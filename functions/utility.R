
# cleanText
# search_reddit
# users.reddit.network
# reddit.users.info


cleanText <- function(xtxt = NULL, 
                      hashtag = TRUE, 
                      mention = TRUE, 
                      numbers = FALSE, 
                      punctation = FALSE,
                      lowercase = TRUE){
  # function parameter description
  # xtxt = vector of texts (tweets)
  # hashtag = logical, if TRUE removes the entire hashtags
  # mention = logical, if TRUE removes all mentions @
  # numbers = logical, if TRUE removes all numbers from messages
  # punctation = logical, if TRUE removes punctation
  # lowercase = logical, if TRUE changes all texts to lowercase
  #
  # check if x is defined and is an object of type character
  if(is.null(xtxt) | class(xtxt) != "character"){stop("invalid character vector")}
  # check if the other arguments are logical
  if(!is.logical(hashtag) | !is.logical(hashtag) | !is.logical(numbers) | !is.logical(punctation) | !is.logical(lowercase)){
    stop("invalid argument")}
  # html symbols vector
  htmlsymbols <- c("&copy;","&reg;","&trade;","&ldquo;","&lsquo;","&rsquo;","&bull;",
                   "&middot;","&sdot;","&ndash;","&mdash;","&cent;","&pound;","&euro;",
                   "&ne;","&frac12;","&frac14;","&frac34;","&deg;","&larr;","&rarr;",
                   "&hellip;","&nbsp;","&lt;","&gt;","&amp;","&quot;")
  htmlsymbolsU <- paste(htmlsymbols,collapse = "|")
  
  # remove links
  xtxt = gsub("(f|ht)(tp)(s?)(://)(.\\S+)[.|/](.\\S+)", " ", xtxt)
  # remove references in retweets
  xtxt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  xtxt = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  # html symbols
  xtxt = gsub(htmlsymbolsU," ",xtxt)
  # punctation
  if(punctation == TRUE){
    xtxt = gsub("([#@])|[[:punct:]]", " \\1", xtxt)
  }
  # control characters
  xtxt = gsub('[[:cntrl:]]', ' ', xtxt)
  # those that are not graphic characters (what is not [[:alnum:][:punct:]])
  xtxt = gsub('[^[:graph:]]', ' ', xtxt)
  # hashtag
  if(hashtag == TRUE) xtxt = gsub("#\\S+", " ", xtxt)
  # mention
  if(mention == TRUE) xtxt = gsub("@\\S+", " ", xtxt)
  # numbers
  if(numbers == TRUE) xtxt = gsub("[[:digit:]]", "", xtxt)
  # tabulations and more spaces in the middle of the text
  xtxt = gsub("[ \t]{2,}", " ", xtxt)
  xtxt = gsub('\\s+',' ',xtxt)
  # spaces at the beginning and end of texts
  xtxt = gsub("^\\s+|\\s+$", "", xtxt)
  # turns everything into lowercase
  if(lowercase == TRUE) xtxt = tolower(xtxt)
  return(xtxt)
}


search_reddit <- function(q = NA, subreddit = NA, sortby = "top",
                          period = "month", lang = "it", lang.det.level = 1, 
                          lang.check.comm = TRUE){
  # download threads and comments based on a query
  # q               search query, the query must have the structure provided in the find_thread_urls function
  # subreddit       if not NA, limits the search to the indicated subreddit
  # sortby          sorting of threads ("relevance", "comments", "new", "hot", "top", "all")
  # period          period of interest ("hour", "day", "week", "month", "year", "all")
  # lang            language to be selected ("it", "en", "de" , "fr"), if NULL select all threads
  # lang.det.level  level for language match check (1, 2, 0 selects all threads)
  # lang.check.comm language check for comments
  # requires that the cleanText function (found in utility.R) has been loaded
  require(dplyr,quietly = T,warn.conflicts = F)
  options(dplyr.summarise.inform = FALSE)
  require(RedditExtractoR,quietly = T,warn.conflicts = F)
  require(cld2,quietly = T,warn.conflicts = F)
  require(cld3,quietly = T,warn.conflicts = F)
  if(is.na(q) & is.na(subreddit)) stop("missing query and subreddit")
  if(!sortby %in% c("relevance", "comments", "new", "hot", "top", "all")) stop("invalid sortby")
  if(!period %in% c("hour", "day", "week", "month", "year", "all")) stop("invalid period")
  if(!lang.det.level %in% 0:2) stop("invalid lang.det.level")
  # ------ searching threads ------
  if(sortby != "all"){
    thre <- find_thread_urls(keywords = q ,
                             subreddit=subreddit,
                             sort_by=sortby,
                             period=period) 
  } else {
    srt.t <- c("top","new","hot","relevance", "comments")
    for(i in 1:length(srt.t)){
      print(srt.t[i])
      f.tmp <- find_thread_urls(keywords = q, sort_by = srt.t[i], period = period,subreddit = subreddit)
      if(i==1){out=f.tmp} else {out=bind_rows(out,f.tmp)}
    }
    out <- out %>% mutate(id=1:nrow(out),.before=1)
    th.ok <- out %>% group_by(title,subreddit) %>% summarise(mm=max(id))
    thre <- out %>% filter(id %in% th.ok$mm)
  }
  if(is.null(nrow(thre))) stop("no thread found")
  cat(paste("threads found:",nrow(thre)),"\n")
  thre <- thre %>% mutate(ID=1:nrow(thre),.before=1)
  # ------ thread selection as a function of text language ------
  if(!is.null(lang)){
    dfThre2 <- thre %>% mutate(txt=ifelse(is.na(title),text,
                                          ifelse(is.na(text),title,paste(title,text,sep = " "))))
    # identify the language of the texts using the two libraries
    cld2_vec = cld2::detect_language(text = cleanText(dfThre2$txt))
    cld3_vec = cld3::detect_language(text = cleanText(dfThre2$txt))
    dfl = data.frame(id=1:length(cld2_vec),cl2=cld2_vec,cl3=cld3_vec)
    dfl <- dfl %>% 
      mutate(cl2=ifelse(is.na(cl2),0,cl2),
             cl3=ifelse(is.na(cl3),0,cl3)) %>% 
      mutate(ll1=ifelse(cl2==lang,1,0),
             ll2=ifelse(cl3==lang,1,0)) %>% 
      mutate(lang.lev=ll1+ll2)
    dfl <- dfl %>% filter(lang.lev >= lang.det.level)
    thre <- thre %>% filter(ID %in% dfl$id)
    cat(paste("selected by language =",lang,":",nrow(thre)),"\n")
  }
  nt = nrow(thre)
  pb <- txtProgressBar(min = 0, max = nt, style = 3)
  # ------ downloading of threads and comments ------
  for(i in 1:nrow(thre)){
    setTxtProgressBar(pb, i)
    tmp.cmmRDT <- tryCatch(get_thread_content(thre$url[i]),
                           error = function(e) e,
                           warning = function(w) w)
    if(!"condition" %in% class(tmp.cmmRDT)){
      # if there are comments
      if(!is.null(tmp.cmmRDT$comments)){
        dfcommenti <- left_join(tmp.cmmRDT$comments,
                                tmp.cmmRDT$threads %>% 
                                  arrange(timestamp) %>% 
                                  select(url,subreddit) %>% 
                                  mutate(id_thread=i),
                                by="url")
        dftmp <- bind_rows(tmp.cmmRDT$threads %>% 
                             mutate(tipo="thread",.before=1) %>% 
                             mutate(id_thread=i,.after=1),
                           dfcommenti %>% 
                             rename(text=comment) %>% 
                             mutate(tipo="comment",.before=1)) %>% 
          as_tibble() %>% 
          mutate(id=1:(nrow(tmp.cmmRDT$threads)+nrow(dfcommenti)),.before=1) %>% 
          mutate(created_at=as.POSIXct(timestamp,origin = "1970-01-01"),.after=timestamp) %>% 
          mutate(comment_id=as.character(comment_id)) %>% 
          mutate(txt=paste(title,text,collapse = " "))
      } else {
        # if there are no comments
        dftmp <- tmp.cmmRDT$threads %>% mutate(tipo="thread",.before=1) %>% 
          mutate(id_thread=i,.after=1) %>% 
          as_tibble() %>% 
          arrange(id_thread,timestamp) %>% 
          mutate(id=1:(nrow(tmp.cmmRDT$threads)),.before=1) %>% 
          mutate(created_at=as.POSIXct(timestamp,origin = "1970-01-01"),.after=timestamp)
      }
      if(i==1){
        dfThCm2 <- dftmp
      } else {
        dfThCm2 <- bind_rows(dfThCm2,dftmp)
      }
    }
  }
  dfThCmL2 <- dfThCm2 %>% mutate(id=1:nrow(dfThCm2),.before = 1) %>% 
    mutate(txt=ifelse(is.na(title),text,
                      ifelse(is.na(text),title,paste(title,text,sep = " "))))
  dfThCmL2$txt <- gsub("\\[removed\\]","",dfThCmL2$txt)
  dfThCmL2$txt <- gsub("\\[deleted\\]","",dfThCmL2$txt)
  dfThCmL2 <- dfThCmL2 %>% filter(txt != "")
  if(lang.check.comm == TRUE & !is.null(lang)){
    cld2_vec = cld2::detect_language(text = cleanText(dfThCmL2$txt))
    cld3_vec = cld3::detect_language(text = cleanText(dfThCmL2$txt))
    dfl = data.frame(id=dfThCmL2$id,cl2=cld2_vec,cl3=cld3_vec)
    dfl <- dfl %>% 
      mutate(cl2=ifelse(is.na(cl2),0,cl2),
             cl3=ifelse(is.na(cl3),0,cl3)) %>% 
      mutate(ll1=ifelse(cl2==lang,1,0),
             ll2=ifelse(cl3==lang,1,0)) %>% 
      mutate(lang.lev=ll1+ll2)
    dfl <- dfl %>% filter(lang.lev >= lang.det.level)
    dfThCmL2 <- dfThCmL2 %>% filter(id %in% dfl$id)
    
  }
  return(dfThCmL2)
}


users.reddit.network <- function(res=NULL){
  # Retrieve data to know which users are connected to which users
  # res  dataframe created with search.reddit
  require(dplyr,quietly = T,warn.conflicts = F)
  if(is.null(res)) stop("missing object res")
  dfrel <- data.frame(id_thread=as.numeric(),url=as.character(),title=as.character(),author.thread=as.character(),
                      comment_id=as.character(),From=as.character(),To=as.character())
  pttt <- res %>% filter(tipo=="thread") %>% select(id_thread,url,title,author)
  pppu <- res %>% filter(tipo=="thread") %>% group_by(id_thread,url) %>% count()
  ppp <- res %>% filter(tipo=="comment") %>% as_tibble() %>% select(url,id_thread,author,comment_id)
  for(i in 1:nrow(pppu)){
    qqq <- ppp %>% filter(url==pppu$url[i]) %>% select(url,author,comment_id) %>% 
      mutate(nnn=nchar(comment_id)) %>% 
      mutate(pred=ifelse(nnn>1,substr(comment_id,1,nnn-2),""))
    if(nrow(qqq) > 0){
      qqqp <- qqq %>% filter(pred!="") %>% select(pred) %>% pull()
      qqqp2 <- qqq %>% filter(comment_id %in% qqqp)
      qqqp3 <- full_join(qqq %>% select(1,2,3,5),
                         qqqp2 %>% select(To=2,3),by=c("pred"="comment_id")) %>% select(-pred)
      out <- inner_join(pttt,qqqp3,by="url") %>% 
        mutate(To=ifelse(is.na(To),author.x,To)) %>% 
        select(id_thread, url, title, author.thread = author.x, comment_id, From = author.y, To)
      # if(i==1){
      #   dfrel=out
      # } else {
        dfrel=bind_rows(dfrel,out)
      # }
    }
  }
  if(nrow(dfrel)==0){
    warning("No connections found")
  }
  return(dfrel)
}

reddit.users.info <- function(users=NULL){
  if(is.null(users)) stop("missing users")
  flagini = 1
  nu <- 0
  user_content <- list()
  for(j in 1:length(users)){
    x <- try(get_user_content(users = users[j]))
    if(class(x)!="try-error"){
      nu <- nu+1
      for(i in 1:length(x)){
        tmp <- as.data.frame(x[[i]]$about)
        if(!is.null(x[[i]]$threads)) {n.thread=nrow(x[[i]]$threads)} else {n.thread=0}
        if(!is.null(x[[i]]$comments)) {n.comments=nrow(x[[i]]$comments)} else {n.comments=0}
        tmp <- tmp %>% mutate(n.thread=n.thread,n.comments=n.comments)
        if(flagini==1){
          dfAuths=tmp
          flagini=0
          comments <- x[[i]]$comments
          threads <- x[[i]]$threads
          user_content <- x
        } else {
          dfAuths=bind_rows(dfAuths,tmp)
          comments <- bind_rows(comments,x[[i]]$comments)
          threads <- bind_rows(threads,x[[i]]$threads)
          user_content[length(user_content)+1] <- x
        }
      }
    } else {
      print(paste("error",users[j]))
    }
  }
  dfAuths <- dfAuths %>% 
    mutate(created_at=as.POSIXct(timestamp,origin="1970-01-01"),.after=timestamp)
  names(user_content) <- dfAuths$name
  res <- list(user_content = user_content, 
              threads=threads, 
              comments=comments, 
              user_info = dfAuths)
  return(res)
}
