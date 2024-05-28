library(rvest)
library(tidyverse)
library(tidytext)
library(tm)
library(readtext)
library(SnowballC)
library(widyr)
library(ggraph)
library(igraph)
library(udpipe)

urlbase <- "https://www.ilsole24ore.com/archivi/"

links <- c()
page_links <- c()
for (i in 1:20) {
    url <- paste0(urlbase, i)
    hh <- read_html(url)
    print(url)
    page_links <- hh %>% 
        html_nodes(".col-lg-8") %>%
        html_nodes("a") %>%
        html_attr("href") %>% 
        .[grepl("^/art/", .)] %>%
        unique()
    links <- c(links, page_links)
}

links <- unique(links)


# create empty dataframe
df_raw <- data.frame(title = character(),
                 subtitle = character(),
                 date = character(),
                 author = character(),
                 category = character(),
                 text = character(),
                 stringsAsFactors = FALSE)

urlbase <- "https://www.ilsole24ore.com"

# library(httr)
#   url <- paste0(urlbase,links[1])
# response <- GET(url)
#            response$status_code
#            response$headers$`retry-after`

# scrape articles
for (i in 1:length(links)) {
    url <- paste0(urlbase,links[i])
    hh <- read_html(url)
    response <- GET(url)

    title <- hh %>% 
        html_nodes("h1") %>% 
        html_text()
    
    subtitle <- hh %>% 
        html_nodes("h2.asummary") %>% 
        html_text()
    
    date <- hh %>%
        html_nodes("time") %>% 
        html_attr("datetime")
    
    text <- hh %>% 
        html_nodes("p.atext") %>% 
        html_text()
    
    # all text in one string
    text <- paste(text, collapse = " ")
    
    author <- hh %>% 
        html_nodes(".auth") %>% 
        html_elements("a") %>% 
        html_text()

    author <- paste(author, collapse = ", ")
    
    category <- hh %>% 
        html_nodes("p.meta > span.meta-part.subhead") %>% 
        html_text()

    category <- category[1]
    
    tryCatch({
        df_raw <- rbind(df_raw, data.frame(title = title,
                                   subtitle = subtitle,
                                   date = date,
                                   author = author,
                                   category = category,
                                   text = text,
                                   stringsAsFactors = FALSE))
    }, error = function(e) {
        print(e)
    })

    print(i)
}    

save(df_raw, file = "data/df_raw.RData")


df <- df_raw
df$doc_id <- paste0("doc",1:nrow(df))

stopwords_italian <- data.frame(word = stopwords("italian"), stringsAsFactors = FALSE)
df$text <- tolower(df$text)


udmodel <- udpipe_load_model(file = 'functions/italian-isdt-ud-2.5-191206.udpipe')

s <- udpipe_annotate(udmodel, x = df$text)

# recognize multiwords with udpipe
colloc <- keywords_collocation(x = x, term = "lemma", group = c("doc_id", "sentence_id"), ngram_max = 3, n_min = 5, sep = " ")

# Tokenized
x <- data.frame(s)
# Correzioni manuali ai lemmi verbali
x$lemma[x$token == "opa"] <- "opa"
x$lemma[x$token == "alfasigma"] <- "alfasigma"
x$lemma[x$token == "aliquota"] <- "aliquota"
x$lemma[x$token == "meloni"] <- "meloni"
x$lemma[x$token == "venezia"] <- "venezia"
x$lemma[x$token == "giulia"] <- "giulia"
x$lemma[x$token == "emilia"] <- "emilia"
x$lemma[x$token == "romagna"] <- "romagna"
x$lemma[x$token == "reggio"] <- "reggio"
x$lemma[grepl("^adjust", x$token)] <- "adjust"
x$lemma <- ifelse(!is.na(x$lemma), x$lemma, gsub(".+rl[aeiou]\\b", "re", x$token))
x$lemma <- ifelse(!is.na(x$lemma), x$lemma, gsub(".+rsi\\b", "re", x$token))
x$lemma <- ifelse(!is.na(x$lemma), x$lemma, gsub(".+rne\\b", "re", x$token))
x$lemma <- ifelse(!is.na(x$lemma), x$lemma, gsub(".+tosi\\b", "re", x$token))
x$lemma <- ifelse(!is.na(x$lemma), x$lemma, gsub(".+ndo[clmnstv][aeio]\\b", "re", x$token))

x$STOP <- ifelse(x$lemma %in% stopwords_italian$word | x$token %in% stopwords_italian$word,1,0)

# Count how many lemmas have been recognized as stocks
sum(x$token %in% stocks$Ticker)
sum(x$token %in% stocks$Company)

x <- x %>% 
    filter(x$STOP == 0) %>%
    # ONLY NOUNS AND VERBS
    filter(upos %in% c("NOUN","VERB","X")) %>% 
    as_tibble()


df_clean <- x %>%
    group_by(doc_id) %>%
    summarise(text = paste(lemma, collapse = " ")) %>%
    as_tibble()

df$text <- df_clean$text[match(df$doc_id, df_clean$doc_id)]

df_clean <- df


crp <- Corpus(VectorSource(df_clean$text))
tdm <- tm::TermDocumentMatrix(crp, control = list(wordLengths = c(2, Inf))) # create a term-document matrix
mtdm <- as.matrix(tdm)
# TF-IDF 1: W = TF * log(N/DF)
tdm.w <- weightTfIdf(tdm)
mtdm.w <- as.matrix(tdm.w)
# TF-IDF 2: W = TF * IDF
dtm <- DocumentTermMatrix(crp, control = list(wordLengths = c(2, Inf)))
mdtm <- as.matrix(dtm)
dtm.w <- weightTfIdf(dtm, normalize = F)
mdtm.w <- as.matrix(dtm.w)

# Dataframe with corpus lemmas, frequencies from mtdm, and weights from mtdm.w
# We will use df_words to create the wordclouds
df_words <- data.frame(word=rownames(mtdm.w), freq=rowSums(mtdm), weight=rowSums(mtdm.w), stringsAsFactors = FALSE)
rownames(df_words) <- NULL




# remove unused objects
rm(stopwords_italian, author, category, date, hh, i, links, page_links, subtitle, text, title, url, urlbase, s,
    udmodel, df)

# save.image(file = "data/Data_Preparation_Image.RData")
