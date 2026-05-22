##########################################################################################
# GENERATE RANDOM NUMBERS
##########################################################################################
#' @title Generate dataframe with random numbers
#' @param ncols number of collumns to generate
#' @param nrows number of rows to generate
#' @param mean mean of generated vectors
#' @param sd standard deviation of generated vectors
#' @param min minimum value in generated vector
#' @param max maximum value in generated vector
#' @param type character "normal" "uniform"
#' @importFrom stats rnorm
#' @keywords functions generate data
#' @export
#' @examples
#' generate_data(nrows=10,ncols=5,mean=0,sd=1,type="normal")
#' generate_data(nrows=10,ncols=5,min=1,max=5,type="uniform")
generate_data<-function(nrows=10,ncols=5,mean=0,sd=1,min=1,max=5,type="normal") {
  df<-data.frame(matrix(NA,ncol=ncols,nrow=nrows))
  if(type=="normal")
    df[]<-sapply(df,function(x) x=stats::rnorm(n=nrows,mean=mean,sd=sd))
  if(type=="uniform")
    df[]<-sapply(df,function(x) x=sample(min:max,nrows,replace=TRUE))
  return(df)
}
##########################################################################################
# GENERATE FACTOR
##########################################################################################
#' @title Generate dataframe of factors
#' @param vector factor pool
#' @param ncols number of collumns to generate
#' @param nrows number of rows to generate
#' @param type "balanced" or "random" "balanced" generates balanced factor vectrors, "random" generates random factor vectors
#' @keywords functions generate data
#' @export
#' @examples
#' generate_factor(vector=LETTERS[1:5],ncols=5,nrows=10,type="random")
#' generate_factor(vector=LETTERS[1:5],ncols=5,nrows=10,type="balanced")
#' generate_factor(vector=LETTERS[1:5],ncols=1,nrows=10,type="balanced")
#' generate_factor(vector=LETTERS[1:5],ncols=1,nrows=10,type="random")
generate_factor<-function(vector=LETTERS[1:5],nrows=2,ncols=10,type="random") {
  df<-data.frame(matrix(NA,ncol=ncols,nrow=nrows))
  result<-data.frame(sapply(df, function(x) {
    df<-factor()
    if(type=="balanced") {
      for (i in 1:length(vector))
        df<-c(df,rep(vector[i],nrows/length(vector)))
      result<-factor(df,levels=vector)
    }
    if(type=="random")
      df<-sample(vector,size=nrows,replace=TRUE)
    result<-factor(df,levels=vector)
  },
  simplify=FALSE,USE.NAMES=FALSE))
  if(ncols==1)
    result<-as.factor(result[,1])
  return(result)
}
##########################################################################################
# GENERATE RANDOM STRING
##########################################################################################
#' @title Generate random strings
#' @param vector character pool
#' @param vector_length number of strings to generate
#' @param nchar Length of generated strings
#' @keywords functions generate data
#' @export
#' @examples
#' generate_string(nchar=10)
#' generate_string(nchar=10,vector_length=10)
generate_string<-function(vector=c(LETTERS,letters,0:9),vector_length=1,nchar=5) {
  result<-c()
  for (i in 1:vector_length)
    result[i]<-paste(sample(vector,nchar,replace=TRUE),collapse="")
  return(result)
}
##########################################################################################
# GENERATE MULTIPLE RESPONCE VECTOR
##########################################################################################
#' @title Generate multiple responce vector
#' @param responces unique categories allowed
#' @param responded number of categories observed in iteration
#' @param length length of returned vector
#' @keywords functions generate data
#' @export
#' @examples
#' generate_multiple_responce_vector(responces=1:4,responded=1:4,length=10)
generate_multiple_responce_vector<-function(responces=1:4,responded=1:4,length=10) {
  result<-c()
  for (i in 1:length)
    result<-c(result,toString(paste0(sample(responces,sample(responded,1)))))
  return(result)
}
##########################################################################################
# SIMULATE CORRELATION MATRIX
##########################################################################################
#' @title Generate dataframe which outputs a predetermined correlation matrix
#' @param correlation_martix correlation matrix of resulting dataframe
#' @param nrows number of rows to generate
#' @importFrom stats rnorm
#' @keywords functions generate data
#' @export
#' @examples
#' df<-data.frame(matrix(.999,ncol=2,nrow=2))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-generate_correlation_matrix(correlation_martix,nrows=100)
#' stats::cor(df)
generate_correlation_matrix<-function(correlation_martix,nrows=10) {
  if (missing(correlation_martix)) {
    correlation_martix<-symmetric_matrix(as.matrix(generate_data(ncols=nrows,nrows=nrows,min=0.1,max=1,type="uniform")))
    diag(correlation_martix)<-1
  }
  L=chol(correlation_martix)
  nvars=dim(L)[1]
  t(L)%*%L
  r=t(L)%*%matrix(stats::rnorm(nvars*nrows),nrow=nvars,ncol=nrows)
  r=data.frame(t(r))
  return(r)
}
##########################################################################################
# SIMULATE DATA FROM SAMPLE
##########################################################################################
#' @title Generate a dataframe that produces the same correlation matrix as the input dataframe
#' @param cordata dataframe
#' @param nrows number of rows to generate
#' @importFrom MASS mvrnorm
#' @keywords functions generate data
#' @export
#' @examples
#' correlation_matrix<-generate_correlation_matrix()
#' stats::cor(correlation_matrix)
#' simulate_correlation_from_sample(correlation_matrix,nrows=1000)
#' stats::cor(simulate_correlation_from_sample(correlation_matrix,nrows=1000))
simulate_correlation_from_sample<-function(cordata,nrows=10) {
  cordata_cov<-cov(cordata,use="pairwise.complete.obs")
  cordata_means<-colMeans(cordata,na.rm=TRUE)
  result<-data.frame(MASS::mvrnorm(nrows,Sigma=cordata_cov,mu=cordata_means))
  return(result)
}
##########################################################################################
# SIMULATE MISSING DATA
##########################################################################################
#' @title Generate missing data
#' @param df vector or dataframe
#' @param missing number of missing data per vector
#' @keywords functions generate data
#' @export
#' @examples
#' generate_missing(rnorm(10),missing=5)
#' generate_missing(generate_data(nrow=10,ncol=2),missing=5)
generate_missing<-function(df,missing=5) {
  if(is.null(dim(df)))
    df[sample(1:length(df),missing,replace=FALSE)]<-NA
  else {
    for (i in names(df))
      df[sample(1:nrow(df),missing,replace=FALSE),i]<-NA
  }
  return(df)
}
