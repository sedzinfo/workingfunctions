##########################################################################################
# LOAD
##########################################################################################
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
remove_misspelled<-function(text) {
  require(future.apply)
  plan(multiprocess)
  text<-as.character(text)
  result<-future_lapply(text,function(x) {
       tm::removeWords(x,as.character(spelling::spell_check_text(x,ignore=character(),lang="en_US")["word"][,1]))
    })
  return(unlist(result))
}
# remove_misspelled(text)
##########################################################################################
# WORD FREQUENCY
##########################################################################################
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
# word_frequency(text)
##########################################################################################
# 
##########################################################################################
tag_pos_df<-function(text){
  library(foreach)
  library(doSNOW)
  cl<-makeCluster(parallel::detectCores())
  registerDoSNOW(cl)
  pb<-txtProgressBar(max=length(text),style=3)
  progress<-function(n) setTxtProgressBar(pb,n)
  opts<-list(progress=progress)
  df_pos<-foreach(i=1:length(text),.combine=plyr::rbind.fill,.packages=c("psycholatefunctions","NLP","openNLP","plyr"),.options.snow=opts) %dopar% {
    tagged_text<-tag_pos(text[i])
    pos_tags<-gsub("[[:punct:]]","",tagged_text$POStags)
    pos_tag_table<-table(pos_tags[!pos_tags==""])
    df_pos_tags<-data.frame(pos_tag_table,row.names=names(pos_tag_table))
    df_pos_tags$Var1<-NULL
    data.frame(t(df_pos_tags))
  }
  return(df_pos)
}
# tag_pos_df(text)
##########################################################################################
# LSA PCA
##########################################################################################
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


