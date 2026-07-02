##########################################################################################
# FUNCTIONS_NLP.R
#
# Small collection of natural-language-processing helpers built on top of the
# NLP-related functions exported by the "rwf" package (clear_text, tag_pos, ...).
# The four functions below are:
#   - remove_misspelled : strip words flagged as misspelled from a character vector
#   - word_frequency     : summary statistics of word frequency for each text
#   - tag_pos_df         : part-of-speech tag counts per text, computed in parallel
#   - compute_cllsa      : hierarchical clustering + Latent Semantic Analysis (LSA)
#
# Required packages: future.apply, spelling, tm, foreach, doSNOW, parallel, plyr,
#                     NLP, openNLP, lsa, LSAfun, and rwf (for clear_text/tag_pos).
##########################################################################################

##########################################################################################
# LOAD
##########################################################################################
# Toy corpus reused by the examples below: text1 is a paragraph made up of
# sentences text2-text6, provided both as one block and split sentence by sentence.
text1<-"There are many variations of passages of Lorem Ipsum available, but the majority have suffered alteration in some form, by injected humour, or randomised words which don't look even slightly believable.
        If you are going to use a passage of Lorem Ipsum, you need to be sure there isn't anything embarrassing hidden in the middle of text.
        All the Lorem Ipsum generators on the Internet tend to repeat predefined chunks as necessary, making this the first true generator on the Internet.
        It uses a dictionary of over 200 Latin words, combined with a handful of model sentence structures, to generate Lorem Ipsum which looks reasonable.
        The generated Lorem Ipsum is therefore always free from repetition, injected humour, or non-characteristic words etc."
text2<-"There are many variations of passages of Lorem Ipsum available, but the majority have suffered alteration in some form, by injected humour, or randomised words which don't look even slightly believable."
text3<-"If you are going to use a passage of Lorem Ipsum, you need to be sure there isn't anything embarrassing hidden in the middle of text."
text4<-"All the Lorem Ipsum generators on the Internet tend to repeat predefined chunks as necessary, making this the first true generator on the Internet."
text5<-"It uses a dictionary of over 200 Latin words, combined with a handful of model sentence structures, to generate Lorem Ipsum which looks reasonable."
text6<-"The generated Lorem Ipsum is therefore always free from repetition, injected humour, or non-characteristic words etc."
text<-c(text1,text2,text3,text4,text5,text6)
##########################################################################################
# REMOVE MISPELLED WORDS
##########################################################################################
#' @title Remove misspelled words
#'
#' @description For each element of \code{text}, flags misspelled words with
#' \code{spelling::spell_check_text()} (dictionary \code{"en_US"}) and removes them
#' with \code{tm::removeWords()}. Texts are processed in parallel with
#' \code{future.apply::future_lapply()}.
#'
#' @param text character vector, one document per element.
#'
#' @return A character vector the same length as \code{text}, with misspelled
#' words stripped out (the surrounding whitespace/punctuation is left as-is).
#'
#' @note Uses the "en_US" dictionary, so British spellings (e.g. "randomised")
#' and domain-specific terms (e.g. "Lorem", "Ipsum") are treated as misspelled
#' and removed. \code{plan(multisession)} starts background worker processes;
#' call \code{future::plan(future::sequential)} afterwards to shut them down.
#'
#' @keywords NLP
#' @examples
#' remove_misspelled(text)
remove_misspelled<-function(text) {
  require(future.apply)
  future::plan(future::multisession)
  on.exit(future::plan(future::sequential),add=TRUE)
  text<-as.character(text)
  result<-future_lapply(text,function(x) {
       tm::removeWords(x,as.character(spelling::spell_check_text(x,ignore=character(),lang="en_US")["word"][,1]))
    },future.seed=TRUE)
  return(unlist(result))
}
##########################################################################################
# WORD FREQUENCY
##########################################################################################
#' @title Word frequency summary statistics
#'
#' @description For each element of \code{text}, looks up the words it contains
#' in a pre-computed corpus-wide frequency table (\code{text_frequency}) and
#' summarises their frequencies (mean, sd, min, max).
#'
#' @param text character vector, one document per element.
#'
#' @return A data frame with one row per element of \code{text} and columns
#' \code{word_frequency_mean}, \code{word_frequency_sd}, \code{word_frequency_min},
#' \code{word_frequency_max}.
#'
#' @note Requires a \code{text_frequency} data frame with columns \code{Var1}
#' (word) and \code{Freq} (corpus-wide count) to already exist in the calling
#' environment - it is typically built once from the full corpus with
#' \code{as.data.frame(table(clear_text(unlist(strsplit(corpus," ")))))}, as in
#' the example below. Also requires \code{clear_text()} (exported by the "rwf"
#' package) to normalise words before matching.
#'
#' @keywords NLP
#' @examples
#' # build the corpus-wide frequency table the function looks words up in
#' words<-clear_text(unlist(strsplit(text," ")))
#' words<-words[words!=""]
#' text_frequency<-as.data.frame(table(words))
#' names(text_frequency)<-c("Var1","Freq")
#' word_frequency(text)
word_frequency<-function(text) {
  result<-data.frame(word_frequency_mean=numeric(),word_frequency_sd=numeric(),word_frequency_min=numeric(),word_frequency_max=numeric())
  for (i in 1:length(text)){
    choose<-unique(intersect(text_frequency$Var1,clear_text(unlist(strsplit(text[i]," ")))))
    word_frequency<-text_frequency[text_frequency$Var1 %in% choose,]$Freq
    wf<-data.frame(word_frequency_mean=mean(word_frequency),
                   word_frequency_sd=sd(word_frequency),
                   word_frequency_min=min(word_frequency),
                   word_frequency_max=max(word_frequency))
    result<-rbind(result,wf)
  }
  return(result)
}
##########################################################################################
# PART OF SPEECH TAG COUNTS (PARALLEL)
##########################################################################################
#' @title Part-of-speech tag counts per text (parallel)
#'
#' @description Tags each element of \code{text} with \code{tag_pos()} (exported
#' by the "rwf" package) and tabulates how many words fall into each
#' part-of-speech category, one row per text. Texts are distributed across a
#' parallel cluster with \code{foreach}/\code{doSNOW}, showing a progress bar.
#'
#' @param text character vector, one document per element.
#'
#' @return A data frame with one row per element of \code{text} and one column
#' per part-of-speech tag encountered anywhere in \code{text} (e.g. \code{NN},
#' \code{VB}, \code{JJ}), containing the tag's count in that text. Cells are
#' \code{NA} where a tag never occurs in that particular text.
#'
#' @note Requires the "rwf" package to be installed (for \code{tag_pos()}, used
#' inside the parallel workers) as well as NLP, openNLP and plyr. Starts a
#' cluster sized to \code{parallel::detectCores()}, used only for the duration
#' of the call.
#'
#' @keywords NLP
#' @examples
#' tag_pos_df(text)
tag_pos_df<-function(text){
  library(foreach)
  library(doSNOW)
  cl<-makeCluster(parallel::detectCores())
  registerDoSNOW(cl)
  pb<-txtProgressBar(max=length(text),style=3)
  progress<-function(n) setTxtProgressBar(pb,n)
  opts<-list(progress=progress)
  df_pos<-foreach(i=1:length(text),.combine=plyr::rbind.fill,.packages=c("rwf","NLP","openNLP","plyr"),.options.snow=opts) %dopar% {
    tagged_text<-tag_pos(text[i])
    pos_tags<-gsub("[[:punct:]]","",tagged_text$POStags)
    pos_tag_table<-table(pos_tags[!pos_tags==""])
    df_pos_tags<-data.frame(pos_tag_table,row.names=names(pos_tag_table))
    df_pos_tags$Var1<-NULL
    data.frame(t(df_pos_tags))
  }
  stopCluster(cl)
  return(df_pos)
}
# tag_pos_df(text)

##########################################################################################
# LSA PCA
##########################################################################################
#' @title Cluster documents and score them against a Latent Semantic Analysis space
#'
#' @description Builds a TF-IDF document-term matrix from \code{df$text}, hierarchically
#' clusters the documents (Euclidean distance, Ward's method) into \code{k} groups, fits
#' a Latent Semantic Analysis (LSA) space over the same document-term matrix, and for
#' each document computes the mean pairwise cosine similarity (via
#' \code{LSAfun::multicos()}) between the LSA term vectors of the words it contains.
#'
#' @param df data frame with a \code{text} character column, one document per row.
#' @param dimensions dimensionality-reduction rule passed to \code{lsa::lsa()};
#' defaults to \code{lsa::dimcalc_share()}.
#' @param k integer, number of clusters passed to \code{stats::cutree()}. Defaults to 9,
#' which requires at least 9 documents in \code{df} - lower it for smaller corpora.
#' @param name currently unused; accepted for backwards compatibility with callers
#' that pass a dataset label.
#'
#' @return A data frame with one row per document in \code{df} and columns:
#' \itemize{
#'   \item \code{cluster_groups} - the hierarchical cluster assignment (1..k)
#'   \item \code{mcs} - mean cosine similarity of the document's words in LSA space
#'   \item \code{tvectors.*} - the document's coordinates in the reduced LSA space
#' }
#'
#' @note Requires the "LSAfun" package (used inside the parallel workers, not
#' loaded automatically otherwise) in addition to tm, lsa, foreach and doSNOW.
#' Starts a cluster sized to \code{parallel::detectCores()} and stops it before
#' returning. \code{bounds=list(global=c(5,Inf))} is hardcoded in the
#' document-term matrix construction and drops terms occurring in fewer than
#' 5 documents, so \code{df} needs enough documents sharing vocabulary for any
#' terms to survive - on a corpus this small, \code{dimensions=dimcalc_share()}
#' (the default) can reduce the LSA space to so few dimensions that a term's
#' vector is truncated to all zeros, which makes \code{mcs} come out as
#' \code{NaN} for documents built only from that term; \code{dimcalc_raw()}
#' (used below) keeps every dimension and avoids that on small corpora.
#'
#' @keywords NLP LSA clustering
#' @examples
#' docs<-c(
#'   "the dog and cat are friendly household pets",
#'   "our dog and cat sleep together every night",
#'   "the dog and cat chase each other in the yard",
#'   "a dog and cat can be very loyal pets",
#'   "people often keep a dog and a cat as pets",
#'   "the stock market rallied as earnings grew",
#'   "investors watched the stock market close higher",
#'   "the stock market fell after weak earnings",
#'   "analysts expect the stock market to recover",
#'   "the stock market reacted to rising interest rates",
#'   "the chef cooked pasta with a rich tomato sauce",
#'   "fresh pasta was served with garlic and basil",
#'   "the recipe calls for pasta and parmesan cheese",
#'   "grandma always makes pasta on Sunday dinner",
#'   "the restaurant is famous for its homemade pasta")
#' df<-data.frame(text=docs,stringsAsFactors=FALSE)
#' compute_cllsa(df,dimensions=lsa::dimcalc_raw(),k=3,name="demo")
compute_cllsa<-function(df,dimensions=dimcalc_share(),k=9,name) {
  require(tm)
  require(lsa)
  stoplist<-tm::stopwords("en")
  raw_corpus<-Corpus(VectorSource(df$text))
  dtm<-DocumentTermMatrix(raw_corpus,control=list(removePunctuation=TRUE,
                                                  removeNumbers=TRUE,
                                                  tolower=TRUE,
                                                  stopwords=TRUE,
                                                  stemming=TRUE,
                                                  removeSparseTerms=TRUE,
                                                  weighting=function(x) weightTfIdf(x,normalize=TRUE),
                                                  bounds=list(global=c(5,Inf))))
  mtdm<-as.matrix(dtm)
  distMatrix<-dist(mtdm,method="euclidean")
  hca<-hclust(distMatrix,method="ward.D")
  cluster_groups<-cutree(hca,k=k)
  lsar<-lsa(dtm,dims=dimensions)

  library(foreach)
  library(doSNOW)
  cl<-makeCluster(parallel::detectCores())
  registerDoSNOW(cl)
  pb<-txtProgressBar(max=nrow(mtdm),style=3)
  progress<-function(n) setTxtProgressBar(pb,n)
  opts<-list(progress=progress)
  mcs<-foreach(i=1:nrow(mtdm),.combine=c,.packages="LSAfun",.options.snow=opts) %dopar% {
    mean(multicos(names(mtdm[i,][mtdm[i,]>0]),tvectors=lsar$dk))
  }
  clmcsdf<-data.frame(cluster_groups=cluster_groups,mcs=mcs,tvectors=lsar$tk)
  stopCluster(cl)
  return(clmcsdf)
}
