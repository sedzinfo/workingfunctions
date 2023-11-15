reviews<-itunesr::getReviews(756904853,"GB",1)
text<-reviews$Review

paRsing<-function(corpus.tmp){
  stopifnot(require(NLP) && require(openNLP) && require(openNLPmodels.en))
  sent_token_annotator<-Maxent_Sent_Token_Annotator()
  word_token_annotator<-Maxent_Word_Token_Annotator()
  parse_annotator<-Parse_Annotator()
  Corpus<-lapply(corpus.tmp,function(x){ x<-as.String(x) })
  lapply(Corpus,function(x) {
    annotated<-annotate(x,list(sent_token_annotator,word_token_annotator))
    parsed<-parse_annotator(x,annotated)
    parsedtexts<-sapply(parsed$features,'[[',"parse")
    parsetrees<-lapply(parsedtexts,Tree_parse)
    gc()
    return(list(parsedtexts,parsetrees))
  })
}

parse2graph<-function(ptext,leaf.color='chartreuse4',label.color='blue4',title=NULL,cex.main=.9,...) {
  stopifnot(require(NLP) && require(igraph))
  ms<-gregexpr("[^() ]+",ptext)
  words<-regmatches(ptext,ms)[[1]]
  regmatches(ptext,ms)<-list(paste0(words,seq.int(length(words))))
  edgelist<-matrix('',nrow=length(words)-2,ncol=2)
  edgemaker<-(function() {
    i<-0
    g<-function(node) {
      if (inherits(node,"Tree")) {
        if ((val<-node$value) != 'TOP1') {
          for (child in node$children) {
            childval<-if(inherits(child,"Tree")) child$value else child
            i<<-i+1
            edgelist[i,1:2]<<-c(val,childval)
          }
        }
        invisible(lapply(node$children,g))
      }
    }
  })()
  edgemaker(Tree_parse(ptext))
  g<-graph_from_edgelist(edgelist)
  vertex_attr(g,'label.color')<-label.color
  vertex_attr(g,'label.color',V(g)[!degree(g,mode='out')])<-leaf.color
  V(g)$label<-sub("\\d+",'',V(g)$name)
  plot(g,layout=layout.reingold.tilford,...)
  if (!missing(title)) title(title,cex.main=cex.main)
}

parsetest<-paRsing(tm::Corpus(tm::VectorSource(text[[1]])))
parsetest[[1]][[1]][[1]]
parse2graph(parsetest[[1]][[1]][[1]],
            title="",margin=-0.05,
            vertex.color=NA,vertex.frame.color=NA,
            vertex.label.font=2,vertex.label.cex=1.5,asp=0.5,
            edge.width=1.5,edge.color='black',edge.arrow.size=0)

