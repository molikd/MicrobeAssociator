# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

tm.divisiveWords <- function(dataset,clus_num,species=NULL) {
  #dataset<-tm.ecology; clus_num<-5; species<-"Acidithiobacillus thiooxidans"
  revs <- tm::tm_map(dataset, tm::content_transformer(tolower))
  revs <- tm::tm_map(revs, tm::removeWords, tm::stopwords("english"))
  revs <- tm::tm_map(revs, tm::removePunctuation)
  revs <- tm::tm_map(revs, tm::removeNumbers)
  revs <- tm::tm_map(revs, tm::stripWhitespace)

  dtm <- tm::DocumentTermMatrix(revs)
  dtm_m <- tm::removeSparseTerms(dtm, 0.91)

  m_dtm <- as.matrix(dtm_m)
  m_dtm[is.na(m_dtm)] <- 0
  m_dtm[is.null(m_dtm)] <- 0
  m_dtm <- m_dtm[ rowSums(m_dtm)!=0, ]

  res <- NMF::nmf(m_dtm,clus_num)

  dims <- list( features=rownames(m_dtm), samples=colnames(m_dtm), basis=paste('', 1:NMF::nbasis(res), sep='') )

  dimnames(res) <- dims
  res.basis <- NMF::basis(res)
  res.coef <- NMF::coef(res)

  res.basis.temp <- cbind(res.basis,apply(res.basis, 1, max))
  rownames(res.basis.temp) <- sub(".txt", "", rownames(res.basis.temp))
  res.assign <- colnames(res.basis.temp)[apply(res.basis.temp,1,which.max)]

  res.assign <- data.frame(res.assign)
  rownames(res.assign) <- rownames(res.basis.temp)

  res.quant <- stats::quantile( res.coef, p = 0.75 )
  res.report <- apply(res.assign, 1, function(x){ colnames(res.coef)[res.coef[x,] > res.quant] })
  #if species then return species, else return whole thing.
  if(!is.na(species)){
    return(res.report[species])
  } else {
    return(res.report)
  }
}

tm.keywords.across <- function(dataset) {
  dataset <- tm.ecology
  out <- sapply(dataset$content, function(x){x$content})
  out2 <- data.frame(out, stringsAsFactors=FALSE)
  out <- data.frame()
  out <- t(out2[1,])
  colnames(out) <- c("text")

  ud_model <- udpipe::udpipe_download_model(language = "english")
  ud_model <- udpipe::udpipe_load_model(ud_model$file_model)
  x <- udpipe::udpipe_annotate(ud_model, x = out[,1] )
  x <- as.data.frame(x)
  stats <- textrank::textrank_keywords(x$lemma,
                             relevant = x$upos %in% c("NOUN", "ADJ"),
                             ngram_max = 8, sep = " ")
  stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
  wordcloud(words = stats$keyword, freq = stats$freq)
  return(stats)
}

corpus.similarDocs <- function(dataset,species) {
  comparisons <- textreuse::pairwise_compare(dataset, textreuse::ratio_of_matches, directional=T)
  comparisons[is.na(comparisons)] <- 0
  return(utils::head(comparisons[species,]))
}

corpus.clustGuessing <- function(dataset) {
  comparisons <- textreuse::pairwise_compare(dataset, textreuse::ratio_of_matches, directional=T)
  comparisons[is.na(comparisons)] <- 0
  factoextra::fviz_nbclust(comparisons, stats::kmeans,  k.max = 15, method = "wss")
  factoextra::fviz_nbclust(comparisons, stats::kmeans,  k.max = 15, method = "silhouette")
  gap_stat <- cluster::clusGap(comparisons, FUN = stats::kmeans, nstart = 25, K.max = 15, B = 10)
  print(gap_stat, method = "firstmax")
  factoextra::fviz_gap_stat(gap_stat)
}


