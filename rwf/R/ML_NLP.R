##########################################################################################
# CLEAR TEXT
##########################################################################################
#' @title Clear text
#' @param text character vector
#' @keywords NLP
#' @export
#' @examples
#' text1<-"word_one word_two word_three"
#' text2<-"word_three word_four word_six"
#' text3<-"All the Lorem Ipsum generators on the Internet tend to repeat predefined 
#' chunks as necessary, making this the first true generator on the Internet."
#' text4<-"It uses a dictionary of over 200 Latin words, combined with a handful of 
#' model sentence structures, to generate Lorem Ipsum which looks reasonable."
#' text5<-"The generated Lorem Ipsum is therefore always free from repetition, 
#' injected humour, or non-characteristic words etc."
#' text<-c(text1,text2,text3,text4,text5)
#' clear_text(text)
clear_text<-function(text) {
  text<-tolower(trimws(gsub("\\s+"," ",gsub("[[:punct:]]+"," ",gsub("[[:digit:]]+"," ",text)))))
  return(text)
}
##########################################################################################
# REMOVE STOPWORDS
##########################################################################################
#' @title Remove stopwods
#' @param text character vector
#' @param stopwords character words to remove
#' @importFrom tm stopwords
#' @importFrom stringr str_replace_all
#' @keywords NLP
#' @export
#' @examples
#' text1<-"word_one word_two word_three"
#' text2<-"word_three word_four word_six"
#' text3<-"All the Lorem Ipsum generators on the Internet tend to repeat predefined 
#' chunks as necessary, making this the first true generator on the Internet."
#' text4<-"It uses a dictionary of over 200 Latin words, combined with a handful of 
#' model sentence structures, to generate Lorem Ipsum which looks reasonable."
#' text5<-"The generated Lorem Ipsum is therefore always free from repetition, 
#' injected humour, or non-characteristic words etc."
#' stopwords<-stopwords::stopwords("english")
#' text<-c(text1,text2,text3,text4,text5)
#' clear_stopwords(text,stopwords=stopwords)
clear_stopwords<-function(text,stopwords=stopwords::stopwords("english")) {
  stopwords_regex=paste(stopwords,collapse='\\b|\\b')
  stopwords_regex=paste0('\\b',stopwords_regex,'\\b')
  text<-stringr::str_replace_all(text,stopwords_regex,'')
  text<-clear_text(gsub(" *\\b[[:alpha:]]{1}\\b *"," ",text)) # Remove 1 letter words
  return(text)
}
##########################################################################################
# PART OF SPEECH TAGGING
##########################################################################################
#' @title Part of speech tagging
#' @param text character vector
#' @importFrom openNLP Maxent_Word_Token_Annotator
#' @importFrom NLP Annotation as.String
#' @keywords NLP
#' @export
#' @examples
#' text1<-"word_one word_two word_three"
#' text2<-"word_three word_four word_six"
#' text3<-"All the Lorem Ipsum generators on the Internet tend to repeat predefined 
#' chunks as necessary, making this the first true generator on the Internet."
#' text4<-"It uses a dictionary of over 200 Latin words, combined with a handful of 
#' model sentence structures, to generate Lorem Ipsum which looks reasonable."
#' text5<-"The generated Lorem Ipsum is therefore always free from repetition, 
#' injected humour, or non-characteristic words etc."
#' text<-c(text1,text2,text3,text4,text5)
#' tag_pos(text)
tag_pos<-function(text) {
  s<-NLP::as.String(text)
  word_token_annotator<-openNLP::Maxent_Word_Token_Annotator()
  a2<-NLP::Annotation(1L,"sentence",1L,nchar(s))
  a2<-NLP::annotate(s,word_token_annotator,a2)
  a3<-NLP::annotate(s,openNLP::Maxent_POS_Tag_Annotator(),a2)
  a3w<-a3[a3$type=="word"]
  POStags<-unlist(lapply(a3w$features,`[[`,"POS"))
  POStagged<-paste(sprintf("%s/%s",s[a3w],POStags),collapse=" ")
  list(POStagged=POStagged,POStags=POStags)
}
##########################################################################################
# TEXT SIMILARITY
##########################################################################################
#' @title Text similarity measures
#' @param text1 character vector
#' @param text2 character vector
#' @keywords NLP
#' @export
#' @examples
#' text1<-"word_one word_two word_three"
#' text2<-"word_three word_four word_six"
#' text3<-"All the Lorem Ipsum generators on the Internet tend to repeat predefined 
#' chunks as necessary, making this the first true generator on the Internet."
#' text4<-"It uses a dictionary of over 200 Latin words, combined with a handful of 
#' model sentence structures, to generate Lorem Ipsum which looks reasonable."
#' text5<-"The generated Lorem Ipsum is therefore always free from repetition, 
#' injected humour, or non-characteristic words etc."
#' text<-c(text1,text2,text3,text4,text5)
#' text<-unlist(strsplit(text,split=" "))
#' text1<-unlist(strsplit(text1,split=" "))
#' text2<-unlist(strsplit(text2,split=" "))
#' text3<-unlist(strsplit(text3,split=" "))
#' text4<-unlist(strsplit(text4,split=" "))
#' text5<-unlist(strsplit(text5,split=" "))
#' text_similarity(text1,text1)
#' text_similarity(text1,text2)
#' text_similarity(text1,text3)
#' text_similarity(text1,text4)
text_similarity<-function(text1,text2) {
  tversky<-compute_tversky_index(text1,text2)
  intersect<-length(intersect(text1,text2))
  tb1<-table(text1)
  tb2<-table(text2)
  df_intersect<-data.frame(rbind(tb1[intersect(names(tb1),names(tb2))],
                                 tb2[intersect(names(tb1),names(tb2))]))
  if(length(df_intersect)>0) {
    intersect_weight<-sum(df_intersect[1,]*df_intersect[2,])
  }
  else {
    intersect_weight<-0
  }
  setdiff1<-length(setdiff(text1,text2))
  setdiff2<-length(setdiff(text2,text1))
  lengtht1<-length(text1)
  lengtht2<-length(text2)
  df<-data.frame(tversky,intersect,intersect_weight,setdiff1,setdiff2,lengtht1,lengtht2)
  return(df)
}
##########################################################################################
# STATISTICS FOR CHARACTERS AND WORDS
##########################################################################################
#' @title Text similarity measures
#' @param text character vector
#' @importFrom future.apply future_sapply
#' @importFrom stats sd
#' @importFrom spelling spell_check_text
#' @keywords NLP
#' @export
#' @examples
#' text<-"There are many variations of passages of Lorem Ipsum available,
#' but the majority have suffered alteration in some form, by injected humour,
#' or randomised words which don't look even slightly believable."
#' stat_word_char(text)
stat_word_char<-function(text) {
  text<-clear_text(text)
  data<-strsplit(text," ")
  words<-future.apply::future_sapply(data,length)
  mean_char<-future.apply::future_sapply(data,function(x) mean(nchar(x)[!nchar(x)==0]))
  sd_char<-future.apply::future_sapply(data,function(x) stats::sd(nchar(x)[!nchar(x)==0]))
  max_char<-future.apply::future_sapply(data,function(x) max(nchar(x)[!nchar(x)==0]))
  min_char<-future.apply::future_sapply(data,function(x) min(nchar(x)[!nchar(x)==0]))
  spell_error<-future.apply::future_sapply(data,function(x)
    nrow(spelling::spell_check_text(x,ignore=character(),lang="en_US")))
  result<-data.frame(words,mean_char,sd_char,max_char,min_char,spell_error)
  return(result)
}
##########################################################################################
# STATISTICS FOR CHARACTERS AND WORDS
##########################################################################################
#' @title Compute the Tversky index
#'
#' @description Computes the Tversky index between two sets, a generalisation of the
#' Jaccard and Sørensen–Dice similarity coefficients. The index measures
#' the overlap between \code{x} and \code{y} relative to their differences,
#' weighted by \code{alpha} and \code{beta}.
#'
#' The Tversky index is defined as:
#'
#' \deqn{T(x, y) = \frac{|x \cap y|}{|x \cap y| + \alpha|x \setminus y| + \beta|y \setminus x|}}
#'
#' Special cases:
#' \itemize{
#'   \item \code{alpha = beta = 0.5} — Sørensen–Dice coefficient
#'   \item \code{alpha = beta = 1.0} — Jaccard index
#' }
#'
#' @param x A vector. Coerced to character before comparison.
#' @param y A vector. Coerced to character before comparison.
#' @param alpha Non-negative numeric. Weight applied to elements in \code{x}
#'   but not in \code{y}. Defaults to \code{0.5}.
#' @param beta Non-negative numeric. Weight applied to elements in \code{y}
#'   but not in \code{x}. Defaults to \code{0.5}.
#'
#' @return A single numeric value in the range \code{[0, 1]}, where \code{0}
#'   indicates no overlap and \code{1} indicates identical sets.
#'
#' @note Both \code{x} and \code{y} are treated as \emph{sets} — duplicate
#'   elements within each vector are ignored. Inputs are coerced to character
#'   before comparison, so \code{1L} and \code{"1"} are treated as equal.
#'
#' @keywords similarity distance set
#' @export
#'
#' @examples
#' x <- c("a", "b", "c", "d")
#' y <- c("b", "c", "d", "e")
#'
#' # default (Sorensen-Dice)
#' compute_tversky_index(x, y)
#'
#' # Jaccard index
#' compute_tversky_index(x, y, alpha = 1, beta = 1)
#'
#' # asymmetric: penalise x-only elements more heavily
#' compute_tversky_index(x, y, alpha = 0.9, beta = 0.1)
#'
#' # identical sets → 1
#' compute_tversky_index(x, x)
#'
#' # disjoint sets → 0
#' compute_tversky_index(c("a", "b"), c("c", "d"))
compute_tversky_index <- function(x, y, alpha = 0.5, beta = 0.5) {
  x <- as.character(x)
  y <- as.character(y)
  
  intersection <- length(intersect(x, y))
  x_only       <- length(setdiff(x, y))
  y_only       <- length(setdiff(y, x))
  
  intersection / (intersection + alpha * x_only + beta * y_only)
}
