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
#' @importFrom tcR tversky.index
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
  tversky<-tcR::tversky.index(text1,text2)
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

