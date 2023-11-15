##########################################################################################
# 
##########################################################################################
stem_text<-"What is the sum of first [N1] [C1]?"
n1<-c(1:10)
c1<-c("natural numbers","non-zero positive integers")
N<-list(n1=n1)
C<-list(c1=c1)
formulae<-"Option_A ? 2*n1-1
           Option_B ? 3*n1-2
           Option_C ~ n1*(n1+1)/2
           Option_D ? n1*(n1-1)/2"
newitems<-QAIG::itemgen(stem_text=stem_text,formulae=formulae,N=N,C=C)
newitems
##########################################################################################
# 
##########################################################################################
stem_text<-"[C1] has $ [N1] and [C2] has $ [N2]. If [C2] takes $ [N3] from [C1] later,then how much more amount than [C1] does [C3] have now?"
c1<-c('Sam','Sean')
c2<-c('Max','Martha','Mandy')
c3<-c('he','she','she')
n1<-c(4,5,6,7)
n2<-c(8,9,10)
n3<-c(2,3)
C<-list(c1=c1,c2=c2,c3=c3)
N<-list(n1=n1,n2=n2,n3=n3)
formulae<-"Option_A ? (n1 + n2)
           Option_B ~ (n2 + 2*n3 - n1)
           Option_C ? (n1 + n2 + 1)
           Option_D ? (n1 + n2 - 2)
           Option_E ? (n2 + n3 - n1)"
options_affix<-list(Option_A=c('$ ',''),Option_B=c('$ ',''),Option_C=c('$ ',''),Option_D=c('$ ',''),Option_E=c('$ ',''),Difficulty='EASY')
newitems<-QAIG::itemgen(stem_text=stem_text,formulae=formulae,N=N,C=C,options_affix=options_affix)
newitems[,c(1,4,21,24)]
##########################################################################################
# 
##########################################################################################
stem_text<-"[C1] bought a [C2] at $ [N1]. [C3] spent $ [N2] for repairing it and then sold it at $ [N3]. What was [C4] percentage of profit or loss?"
c1<-c('Samuel','April')
c2<-c('motorcycle','moped')
c3<-c('He','She')
c4<-c('his','her')
n1<-c(925,862,784)
n2<-c(92,102)
n3<-1030
C<-list(c1=c1,c2=c2,c3=c3,c4=c4)
N<-list(n1=n1,n2=n2,n3=n3)
formulae<-"Option_A ? round((n2/n1)*100,2)
           Option_B ? round(((n3-n2-n1)/n3)*100,1)
           Option_C ? round(((n3-n2-n1+0)/n3)*100,1)
           Option_D ~ round((((n3-n2-n1)/(n1+n2))*100),2)"
options_affix<-list(Option_A=c('','% loss'),Option_B=c('','% profit'),Option_C=c('','% loss'),Option_D=c('','% profit'),Option_E='No profit no loss')
newitems<-QAIG::itemgen(stem_text=stem_text,formulae=formulae,C=C,N=N,options_affix=options_affix)
newitems[,c(1,2,3,6)]
##########################################################################################
# 
##########################################################################################
stem_text<-"A [C1] was delayed somewhere for [N1] minutes but made up for the delay on a section of [N2] km travelling at a speed of [N3] km per hour higher than that which accorded the schedule. What was the speed of the [C1] that accorded the schedule?"
c1<-c('car','bus','truck','train')
n1<-c(16,18,20,22,24)
n2<-c(80,90,100,110)
n3<-c(10,12,15,18)
C<-list(c1=c1)
N<-list(n1=n1,n2=n2,n3=n3)
formulae<-"p<-1
           Option_A? round((-n3 + sqrt(n3^2 - 4*p*(-60*n2*n3/n1)))/2,2)
           Option_B? round((-n3 - sqrt(n3^2 - 4*p*(-60*n2*n3/n1)))*(-1)/2-20,2)
           Option_C? round((-n3 - sqrt(n3^2 - 4*p*(-60*n2*n3/n1)))*(-1)/2,2)
           Option_D? round((-n3 + sqrt(n3^2 - 4*p*(-60*n2*n3/n1)))/2+30,2)"
options_affix<-list(Option_A=c('',' km/hr'),Option_B=c('',' km/hr'),Option_C=c('',' km/hr'),Option_D=c('',' km/hr'),Option_E='Cannot be determined',Difficulty='HARD')
newitems<-QAIG::itemgen(stem_text=stem_text,formulae=formulae,N=N,C=C,ans_key='Option_A',options_affix=options_affix)
newitems[,c(1,2,79,80)]
##########################################################################################
# 
##########################################################################################
stem_text<-"Sum of present ages of [C1] and [C2] [C3] is [N1]. After [N2] years [C2] [C3] will be thrice as old as [C1]. The present age of [C2] [C3] is"
n1<-c(74,80,72,68)
n2<-c(8,10)
c1<-c('Sophia','Viktor','Julia','Andy')
c2<-c('her','his')
c3<-c('father','mother')
N<-list(n1=n1,n2=n2)
C<-list(c1=c1,c2=c2,c3=c3)
formulae<-"a<-5
           sol<-function(x,y) {
            A<-matrix(c(1,1,1,-3),nrow=2)
            B<-matrix(c(x,2*y),nrow=2)
            return((as.matrix(solve(A)%*%B)))
           }
           Option_A ? round(sol(n1,n2)[1,]+a,2)
           Option_B ? round(sol(n1,n2)[2,]+a,2)
           Option_C ~ round(sol(n1,n2)[1,],2)
           Option_D ? round(sol(n1,n2)[2,],2)
           Option_E ? round(sol(n1,n2)[1,]-a,2)"
options_affix<-list(Option_A=c('',' years'),Option_B=c('',' years'),Option_C=c('',' years'),Option_D=c('',' years'),Option_E=c('',' years'),Difficulty='MEDIUM')
newitems<-QAIG::itemgen(stem_text=stem_text,formulae=formulae,N=N,C=C,options_affix=options_affix)
newitems<-item_generator(stem_text=stem_text,formulae=formulae,N=N,C=C,options_affix=options_affix)
newitems
##########################################################################################
# ARITHMETIC SEQUENCE
##########################################################################################
description<-"Find the missing number"
len<-100
fibvals<-numeric(len)
fibvals[1]<-1
fibvals[2]<-1
for (i in 3:len) {
  fibvals[i]<-fibvals[i-1]+fibvals[i-2]
} 

generate_arithmetic_distractor<-function(vector,correct=5,n=4) {
  result<-sample(c(vector,vector^2,vector+vector),n,replace=TRUE)
  return(result)
}

add<-substract<-multiply0<-multiply1<-multiply2<-fib<-list()
vector<-1:100
for (i in 1:10) {
  add[[i]]<-seq(from=i,length.out=6,by=i)
  substract[[i]]<-seq(from=i+20,length.out=6,by=-i)
  multiply0[[i]]<-seq(from=i,length.out=6,by=i*vector[i])
  multiply1[[i]]<-seq(from=i,length.out=6,by=round(i*(i/2),0))
  fib[[i]]<-fibvals[i:(i+5)]
}

sequences<-rbind(data.frame(matrix(unlist(add),nrow=length(add),byrow=T)),
                 data.frame(matrix(unlist(substract),nrow=length(substract),byrow=T)),
                 data.frame(matrix(unlist(multiply0),nrow=length(multiply0),byrow=T)),
                 data.frame(matrix(unlist(multiply1),nrow=length(multiply1),byrow=T)))

df_sequence<-data.frame()
for(i in 1:nrow(sequences)) {
  sequence<-as.numeric(sequences[i,(grep("X",names(sequences)))])
  distractor<-data.frame(t(generate_arithmetic_distractor(sequence,correct=6,n=6)))
  names(distractor)<-gsub("X","D",names(distractor),fixed=TRUE)
  correct=sequence[length(sequence)]
  df_sequence<-plyr::rbind.fill(df_sequence,data.frame(question="find the missing number",t(sequence),distractor,correct))
}

item_generator<-function(stem_text=stem_text,formulae=formulae,N=N,C,options_affix,ans_key,save.csv) {
  if (missing(stem_text) || missing(formulae) || missing(N)) stop("Inputs must be provided for all the default arguments: 'stem_text','formulae' and 'N'")
  num_inputs<-expand.grid(N)
  if (!missing(C)) {
    char_inputs<-list()
    for (i in 1:length(C))
      char_inputs[[i]]<-rep(C[[i]],nrow(num_inputs))[1:nrow(num_inputs)]
    inputs_frame<-data.frame(char_inputs,num_inputs)
    names(inputs_frame)<-c(names(C),names(N))
  }
  else {
    inputs_frame<-data.frame(num_inputs)
    names(inputs_frame)<-names(N)
  }
  formulae_split<-trimws(unlist(strsplit(formulae,split="\n")))
  response_split<-stringr::str_subset(formulae_split,"[~?]")
  support_values<-stringr::str_flatten(formulae_split[stringr::str_detect(formulae_split,"[?~]") == F],"\n")
  if (length(support_values) != 0)
    eval(parse(text=support_values))
  correct_optn<-response_split[grep("~",response_split)]
  if (missing(ans_key) && length(correct_optn) == 1) 
    correct_key<-trimws(gsub("\\~.*","",correct_optn))
  else if (!missing(ans_key) && length(correct_optn) == 0)
    correct_key<-ans_key
  else stop("Write formula for the correct option using single '~' symbol within formulae OR declare ans_key as function argument.")
  response_functions<-c()
  for (i in 1:length(response_split))
    response_functions[i]<-stringr::str_split(response_split,"[~?]")[[i]][2]
  option_names<-c()
  for (i in 1:length(response_split))
    option_names[i]<-trimws(stringr::str_split(response_split,"[~?]")[[i]][1])
  response_functions<-trimws(response_functions)
  if (missing(C)) {
    c1<-sample(LETTERS,nrow(inputs_frame),replace=T)
    inputs_frame<-data.frame(c1=c1,inputs_frame)
    model_formulae<-as.formula(stringr::str_flatten(c("c1 ~ .",paste("I(",response_functions,")")),"|"))
    F1<-Formula::Formula(model_formulae)
    inputs_model<-model.frame(F1,data=inputs_frame)
    inputs_model<-inputs_model[,-(which(colnames(inputs_model) == "c1"))]
    inputs_frame<-data.frame(inputs_frame[,-(which(colnames(inputs_frame) == "c1"))])
    names(inputs_frame)<-names(N)
  }
  else if (!missing(C)) {
    model_formulae<-as.formula(stringr::str_flatten(c("c1 ~ .",paste("I(",response_functions,")")),"|"))
    F1<-Formula::Formula(model_formulae)
    inputs_model<-do.call(rbind,lapply(seq_len(nrow(inputs_frame)),function(i) stats::model.frame(F1,data=inputs_frame[i,])))
  }
  if (ncol(inputs_model) == ncol(inputs_frame) + length(option_names))
    colnames(inputs_model)<-c(colnames(inputs_frame),option_names)
  else stop("Formulae-models used for the options must be distinct per option and each option-formula must produce single numeric value.")
  if (!missing(options_affix)) {
    if (all.equal(option_names,names(options_affix)[1:length(option_names)]) == TRUE) {
      for (i in option_names)
        inputs_model[[i]]<-paste0(options_affix[[i]][1],inputs_model[[i]],options_affix[[i]][2])
    }
    else stop("Affixes are NOT mapped sequentially to same option names specified within formulae.")
    if (length(options_affix) > length(option_names)) {
      for (i in names(options_affix[(length(option_names) + 1):length(options_affix)]))
        inputs_model[[i]]<-rep(options_affix[[i]],nrow(inputs_model))
    }
  }
  stem_words<-unlist(strsplit(stem_text,split=" "))
  vrble_positions<-intersect(grep("\\[",stem_words),grep("\\]",stem_words))
  raw_vrbles<-unique(stem_words[vrble_positions])
  replace_stem_positions<-list()
  for (j in 1:ncol(inputs_frame))
    replace_stem_positions[[j]]<-grep("\\TRUE",stringr::str_detect(tolower(stem_words),colnames(inputs_frame)[j]))
  if (length(inputs_frame) == length(raw_vrbles)) {
    stemclones<-c()
    for (i in 1:nrow(inputs_frame)) {
      for (j in 1:length(replace_stem_positions))
        stem_words[replace_stem_positions[[j]]]<-as.character(inputs_frame[i,j])
      stemclones[i]<-stringr::str_flatten(stem_words," ")
    }
  }
  else stop("Declared variables in stem should be kept in the format of [.] without space inside and declared variables in stem must be similar to the input variables.")
  newitems<-data.frame(Stem=stemclones,inputs_model[,-c(1:length(inputs_frame))],Answer_Key=rep(correct_key,length(stemclones)))
  rownames(newitems)<-paste0("[Q",1:nrow(newitems),"]")
  time_now<-gsub("\\:","..",Sys.time())
  if (!missing(save.csv)) {
    write.csv(newitems,paste(save.csv,time_now,".csv"))
    message("Newly generated items have been saved in a .csv file in the working directory")
  }
  return(newitems)
}
