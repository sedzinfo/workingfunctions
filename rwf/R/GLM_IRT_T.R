##########################################################################################
# GENERATE MATRIX A
##########################################################################################
#' @title Generate Matrix A
#' @param blocks number of blocks
#' @param items number of items per block
#' @keywords tirt irt
#' @export
#' @examples
#' generate_matrix_A(blocks=3,items=3)
generate_matrix_A<-function(blocks=3,items=3) {
  comparison<-generate_comparisons_matrix(items)
  row_length<-nrow(comparison)
  alpha<-matrix(nrow=row_length*blocks,ncol=items*blocks)
  alpha[is.na(alpha)]<-0
  col_index<-increase_index(blocks=blocks,items=items)
  row_index<-increase_index(blocks=blocks,items=row_length)
  for(i in 1:blocks)
    alpha[row_index[i,],col_index[i,]]<-comparison
  return(alpha)
}
##########################################################################################
# GENERATE INDEX FOR ITEM COMPARISONS
##########################################################################################
#' @title Compute number of dummy comparisons
#' @param items number of items per block
#' @keywords tirt irt
#' @export
#' @examples
#' compute_dummy_comparisons(1)
#' compute_dummy_comparisons(2)
#' compute_dummy_comparisons(3)
#' compute_dummy_comparisons(4)
#' compute_dummy_comparisons(5)
#' compute_dummy_comparisons(6)
compute_dummy_comparisons<-function(items) {
  comparisons<-items*(items-1)/2
  return(comparisons)
}
##########################################################################################
# GENERATE INDEX FOR ITEM COMPARISONS
##########################################################################################
#' @title Generate index for unique comparisons
#' @param items number of items
#' @importFrom plyr rbind.fill
#' @keywords tirt irt
#' @export
#' @examples
#' generate_unique_comparisons_index(1)
#' generate_unique_comparisons_index(2)
#' generate_unique_comparisons_index(3)
#' generate_unique_comparisons_index(4)
#' generate_unique_comparisons_index(5)
#' generate_unique_comparisons_index(6)
generate_unique_comparisons_index<-function(items) {
  result<-data.frame()
  for (i1 in 1:items){
    for (i2 in 1:items)
      result<-plyr::rbind.fill(result,data.frame(i1=i1,i2=i2))
  }
  result<-result[apply(result,1,function(x) length(unique(x[!is.na(x)]))!=1),]
  result<-result[result$i1<result$i2,]
  return(as.matrix(result))
}
##########################################################################################
# INCREASE INDEX
##########################################################################################
#' @title index dataframe picks
#' @inheritParams generate_matrix_A
#' @keywords tirt irt
#' @export
#' @examples
#' increase_index(3,3)
increase_index<-function(blocks,items){
  start<-1
  stop<-items
  index<-matrix(seq(start,stop,1),ncol=items)
  for(i in 2:blocks){
    start<-start+items
    stop<-stop+items
    index<-rbind(index,seq(start,stop,1))
  }
  return(index)
}
##########################################################################################
# GENERATE COMPARISONS MATRIX
##########################################################################################
#' @title Generate comparisons matrix
#' @inheritParams generate_unique_comparisons_index
#' @keywords tirt irt
#' @export
#' @examples
#' generate_comparisons_matrix(2)
#' generate_comparisons_matrix(3)
#' generate_comparisons_matrix(4)
#' generate_comparisons_matrix(5)
#' generate_comparisons_matrix(6)
generate_comparisons_matrix<-function(items) {
  comparisons<-generate_unique_comparisons_index(items)
  result<-matrix(nrow=nrow(comparisons),ncol=items)
  result[is.na(result)]<-0
  for (i in 1:nrow(comparisons)){
    result[i,comparisons[i,1]]<-1
    result[i,comparisons[i,2]]<--1
  }
  return(result)
}
##########################################################################################
# GENERATE MATRIX LAMBDA HAT
##########################################################################################
#' @title Generate matrix lambda for spesified number of comparisons
#' @inheritParams generate_matrix_A
#' @keywords tirt irt
#' @export
#' @examples
#' generate_matrix_lambda_hat(blocks=3,items=4)
generate_matrix_lambda_hat<-function(blocks=3,items=3) {
  lambda<-matrix(nrow=0,ncol=items)
  for (i in 1:blocks)
    lambda<-rbind(lambda,generate_comparisons_matrix(items))
  return(lambda)
}
##########################################################################################
# RANK BLOCK TO BINARY
##########################################################################################
#' @title Convert scale to thurstonian binary with n items per ranking block
#' @param mydata dataframe
#' @param items number of items in block
#' @param reverse if TRUE assumes that the highest value is first item in rank if FALSE the lowest value is the first item in rank
#' @keywords tirt irt
#' @export
#' @examples
#' set.seed(12345)
#' mydata<-data.frame(i1=round(rnorm(10,mean=2,sd=1),2),
#'                    i2=round(rnorm(10,mean=2,sd=1),2),
#'                    i3=round(rnorm(10,mean=2,sd=1),2),
#'                    i4=round(rnorm(10,mean=2,sd=1),2),
#'                    i5=round(rnorm(10,mean=2,sd=1),2),
#'                    i6=round(rnorm(10,mean=2,sd=1),2))
#' rank_to_binary(mydata[,c("i1","i2","i3")],items=3)
#' rank_to_binary(mydata[,c("i1","i2","i3")],items=3,reverse=FALSE)
#' rank_to_binary(mydata,items=3)
rank_to_binary<-function(mydata,items,reverse=TRUE) {
  if(missing(items))
    items<-length(mydata)
  index<-generate_unique_comparisons_index(items)
  binary<-index_name<-list()
    for(i in 1:nrow(index)) {
      index_name[[i]]<-paste0("i",index[i,1],index[i,2])
      binary[[index_name[[i]]]]<-rep(0,nrow(mydata))
      binary[[index_name[[i]]]][which(mydata[,index[i,1]]>mydata[,index[i,2]])]<-1
    }
  binary<-matrix(unlist(binary),nrow=length(binary[[1]]),dimnames=list(NULL,names(binary)))
  if(!reverse)
    binary<-+(!binary)
  return(binary)
}
##########################################################################################
# RANK DATAFRAME TO BINARY
##########################################################################################
#' @title Convert scale to thurstonian binary with n items per block and n blocks
#' @inheritParams rank_to_binary
#' @keywords tirt irt
#' @export
#' @examples
#' set.seed(12345)
#' mydata<-data.frame(i1=rnorm(10,mean=2,sd=.5),
#'                    i2=rnorm(10,mean=2,sd=.5),
#'                    i3=rnorm(10,mean=2,sd=.5),
#'                    i4=rnorm(10,mean=2,sd=.5),
#'                    i5=rnorm(10,mean=2,sd=.5),
#'                    i6=rnorm(10,mean=2,sd=.5))
#' rank_df_to_binary(mydata[,c("i1","i2","i3","i4")],4)
#' rank_df_to_binary(mydata,3)
rank_df_to_binary<-function(mydata,items,reverse=TRUE) {
  binary<-data.frame(index=1:nrow(mydata))
  blocks<-length(mydata)/items
  index<-increase_index(blocks=blocks,items=items)
  for (i in 1:blocks)
    binary<-data.frame(binary,rank_to_binary(mydata[,index[i,]],items,reverse))
  binary$index<-NULL
  return(binary)
}
##########################################################################################
# RANK BINARY TO TRIPLETS
##########################################################################################
#' @title Convert thurstonian binary triplets to scale
#' @param mydata dataframe
#' @keywords tirt irt
#' @export
#' @examples
#' set.seed(12345)
#' mydata<-data.frame(i1=rnorm(10,mean=2,sd=.5),
#'                    i2=rnorm(10,mean=2,sd=.5),
#'                    i3=rnorm(10,mean=2,sd=.5),
#'                    i4=rnorm(10,mean=2,sd=.5),
#'                    i5=rnorm(10,mean=2,sd=.5),
#'                    i6=rnorm(10,mean=2,sd=.5))
#' result<-rank_to_binary(mydata[,1:3])
#' rank3_to_triplets(result)
rank3_to_triplets<-function(mydata) {
  item1<-item2<-item3<-rep(NA,nrow(mydata))
  result<-data.frame(item1,item2,item3)
  result[mydata[,1]==1&mydata[,2]==1,]$item1<-3
  result[mydata[,1]==1&mydata[,2]==0,]$item1<-2
  result[mydata[,1]==0&mydata[,2]==1,]$item1<-2
  result[mydata[,1]==0&mydata[,2]==0,]$item1<-1
  result[mydata[,1]==0&mydata[,3]==1,]$item2<-3
  result[mydata[,1]==0&mydata[,3]==0,]$item2<-2
  result[mydata[,1]==1&mydata[,3]==1,]$item2<-2
  result[mydata[,1]==1&mydata[,3]==0,]$item2<-1
  result[mydata[,2]==0&mydata[,3]==0,]$item3<-3
  result[mydata[,2]==1&mydata[,3]==0,]$item3<-2
  result[mydata[,2]==0&mydata[,3]==1,]$item3<-2
  result[mydata[,2]==1&mydata[,3]==1,]$item3<-1
  return(result)
}
##########################################################################################
# RESPONSE DIMENSION
##########################################################################################
#' @title index parameter and items relative to their dimensions
#' @param response vector one to number of items
#' @param dimensions number of dimensions
#' @param items item comparisons
#' @keywords tirt irt
#' @export
#' @examples
#' response_dimension(c(1:18),3,c(1,2))
#' response_dimension(c(1:18),3,c(1,3))
#' response_dimension(c(1:18),3,c(2,3))
response_dimension<-function(response,dimensions,items) {
  response_vector<-c()
  for (i in 1:(length(response)/dimensions)) {
    response_vector<-c(response_vector,response[items])
    response<-response[dimensions+1:length(response)]
  }
  return(response_vector)
}
##########################################################################################
# INDEX FROM LAVAAN TO THURSTONIAN
##########################################################################################
#' @title index of items to convert from lavaan to thurstonian order for analysis
#' @param nitems number of items in the questionnaire
#' @param nfactors number of factors
#' @keywords tirt irt
#' @export
#' @examples
#' cfa_icc_index(nitems=18,nfactors=3)
cfa_icc_index<-function(nitems,nfactors=3) {
  index<-nitems/nfactors
  start_index<-list(1)
  for (i in 2:nfactors)
    start_index[[i]]<-index*(i-1)+1
  for(i in 1:length(start_index))
    start_index[[i]]<-start_index[[i]]:(start_index[[i]]+index-1)
  comparison_matrix<-matrix(unlist(start_index),ncol=nfactors,byrow=FALSE)
  index<-c()
  for(i in 1:nrow(comparison_matrix))
    index<-c(index,as.numeric(comparison_matrix[i,]))
  result<-list(index_vector=index,index_matrix=comparison_matrix)
  return(result)
}
##########################################################################################
# ITEM CHARACTERISTIC CURVE FOR CFA INPUT
##########################################################################################
#' @title Select responses for each dimension
#' @param eta eta or ability
#' @param gamma gamma or threshold
#' @param lambda lambda or loading
#' @param psi psi or error
#' @keywords tirt irt
#' @export
#' @examples
#' icc_cfa(seq(-6,6,.1),1,1,1)
icc_cfa<-function(eta,gamma,lambda,psi) {
  result<-pnorm((-gamma+lambda*eta)/(sqrt(psi)))
  return(result)
}
##########################################################################################
# PLOT ICC THURSTONIAN
##########################################################################################
#' @title Plot thurstonian icc
#' @description Plot icc curves for binary thurstonian coded items for a single dimension using the compute_icc_thurstonian function
#' @param mydata dataframe from compute_icc_thurstonian function
#' @param title plot title
#' @import ggplot2
#' @importFrom reshape2 melt
#' @keywords tirt irt
#' @export
#' @examples
#' gamma<-c(0.556,-1.253,-1.729,0.618,0.937,0.295,-0.672,-1.127,-0.446,0.632,1.147,0.498)
#' psi<-c(2.172,1.883,2.055,1.869,2.231,2.100,1.762,1.803,1.565,1.892,1.794,1.686)
#' lambda<-c(1.082,1.082,-1.297,-1.297,0.802,0.802,1.083,1.083)
#' gamma<-gamma[response_dimension(c(1:12),3,c(1,2))]
#' psi<-psi[response_dimension(c(1:12),3,c(1,2))]
#' eta<-seq(-6,6,by=1)
#' result<-compute_icc_thurstonian(eta=eta,gamma=gamma,lambda=lambda,psi=psi,plot=TRUE)
#' plot_icc_thurstonian(result$icc)
plot_icc_thurstonian<-function(mydata,title="Item Characteristic Curve") {
  value<-eta<-variable<-NULL
  mydata<-reshape2::melt(mydata,id.vars="eta")
  plot<-ggplot(mydata,aes(y=value,x=eta,group=variable,color=variable))+geom_point(alpha=.1)+geom_line()+theme_bw()+
    labs(y=expression(P(eta)),x=expression(eta),title=title)
  return(plot)
}
##########################################################################################
# COMPUTE ICC THURSTONIAN
##########################################################################################
#' @title Compute item characteristic curves for thurstonian models
#' @description Computes icc curves for binary thurstonian coded items for a single dimension
#' @param eta eta or ability
#' @param gamma gamma or threshold
#' @param lambda lambda or loading
#' @param psi psi or error
#' @param plot if TRUE plots icc curves using the plot_icc_thurstonian function
#' @keywords tirt irt
#' @export
#' @examples
#' gamma<-c(0.556,-1.253,-1.729,0.618,0.937,0.295,-0.672,-1.127,-0.446,0.632,1.147,0.498)
#' psi<-c(2.172,1.883,2.055,1.869,2.231,2.100,1.762,1.803,1.565,1.892,1.794,1.686)
#' lambda<-c(1.082,1.082,-1.297,-1.297,0.802,0.802,1.083,1.083)
#' gamma<-gamma[response_dimension(c(1:12),3,c(1,2))]
#' psi<-psi[response_dimension(c(1:12),3,c(1,2))]
#' eta<-seq(-6,6,by=0.01)
#' compute_icc_thurstonian(eta=eta,gamma=gamma,lambda=lambda,psi=psi,plot=FALSE)
compute_icc_thurstonian<-function(eta,gamma,lambda,psi,plot=FALSE) {
  result<-data.frame(eta)
  for(i in 1:length(lambda)) {
    item<-icc_cfa(eta,gamma[i],lambda[i],psi[i])
    result<-data.frame(result,item)
  }
  result<-result[,c(grep("item",names(result),value=TRUE),"eta")]
  names(result)[1:length(lambda)]<-paste0("item",1:length(lambda))
  if(plot)
    plot<-plot_icc_thurstonian(result,title="Item Characteristic Curve")
  return(list(icc=result,plot=plot))
}
##########################################################################################
# COMPUTE ABILITY
##########################################################################################
#' @title Compute subject ability for thurstonian models
#' @description Computes person ability for binary thurstonian coded items for a single dimension
#' @param eta eta or ability
#' @param gamma gamma or threshold
#' @param lambda lambda or loading
#' @param psi psi or error
#' @param plot if TRUE plots icc curves using the plot_icc_thurstonian function
#' @param response item responses
#' @param map vector from compute_map
#' @keywords tirt irt
#' @export
#' @examples
#' gamma<-c(0.556,-1.253,-1.729,0.618,0.937,0.295,-0.672,-1.127,-0.446,0.632,1.147,0.498)
#' psi<-c(2.172,1.883,2.055,1.869,2.231,2.100,1.762,1.803,1.565,1.892,1.794,1.686)
#' lambda<-c(1.082,1.082,-1.297,-1.297,0.802,0.802,1.083,1.083)
#' gamma<-gamma[response_dimension(c(1:12),3,c(1,2))]
#' psi<-psi[response_dimension(c(1:12),3,c(1,2))]
#' eta<-seq(-6,6,by=0.1)
#' response1<-c(0,0,0,0,0,0,0,0)
#' response2<-c(1,1,1,1,1,1,1,1)
#' response3<-c(1,0,1,0,1,0,1,0)
#' response4<-c(0,1,0,1,0,1,0,1)
#' map<-compute_map(eta=eta,mean=0,sd=1)
#' compute_ability(response1,eta,gamma,lambda,psi,map=map,plot=FALSE)
#' compute_ability(response2,eta,gamma,lambda,psi,map=map,plot=FALSE)
#' compute_ability(response3,eta,gamma,lambda,psi,map=map,plot=FALSE)
#' compute_ability(response4,eta,gamma,lambda,psi,map=map,plot=FALSE)
compute_ability<-function(response,eta,gamma,lambda,psi,plot=FALSE,map=compute_map(eta=eta,mean=0,sd=1)) {
  result<-compute_icc_thurstonian(eta,gamma,lambda,psi,plot=plot)
  product=1
  for(i in 1:length(response)) {
    product<-product*(result$icc[,i]^response[i])*((1-result$icc[,i])^(1-response[i]))
  }
  product_map=product*map
  ability_ml<-result$icc$eta[which(product==max(product))]
  ability_map<-result$icc$eta[which(product_map==max(product_map))]
  if(plot) {
    repeat {
      if(max(product)<.1)
        product<-product*10
      if(max(product)>=.1)
        break()
    }
    repeat {
      if(max(product_map)<.1)
        product_map<-product_map*10
      if(max(product_map)>=.1)
        break()
    }
    icc_df<-data.frame(product=product,map=product_map,result$icc)
    print(plot_icc_thurstonian(icc_df,title=paste(title="Item Characteristic Curve",
                                                  "ML:",round(ability_ml,2),
                                                  "MAP:",round(ability_map,2),
                                                  "\nResponse:",sum(response),
                                                  "Response Length:",length(response))))
  } else {
    icc_df<-data.frame(product=product,map=product_map,result$icc)
  }
  return(list(product=product,icc=icc_df,ability_ml=ability_ml,ability_map=ability_map))
}
##########################################################################################
# COMPUTE ICC THURSTONIAN
##########################################################################################
#' @title Compute subject ability for thurstonian models
#' @description Computes person ability for binary thurstonian coded items for a single dimension
#' @param mydata item responses
#' @param ... arguments passed to compute_ability
#' @keywords IRT Thurstonian
#' @export
#' @examples
#' gamma<-c(0.556,-1.253,-1.729,0.618,0.937,0.295,-0.672,-1.127,-0.446,0.632,1.147,0.498)
#' psi<-c(2.172,1.883,2.055,1.869,2.231,2.100,1.762,1.803,1.565,1.892,1.794,1.686)
#' lambda<-c(1.082,1.082,-1.297,-1.297,0.802,0.802,1.083,1.083)
#' gamma<-gamma[response_dimension(c(1:12),3,c(1,2))]
#' psi<-psi[response_dimension(c(1:12),3,c(1,2))]
#' eta<-seq(-6,6,by=0.1)
#' map<-compute_map(eta=eta,mean=0,sd=1)
#' response_df<-data.frame(matrix(nrow=0,ncol=8))
#' response_df[1,]<-c(0,0,0,0,0,0,0,0)
#' response_df[2,]<-c(1,1,1,1,1,1,1,1)
#' response_df[3,]<-c(1,0,1,0,1,0,1,0)
#' response_df[4,]<-c(0,1,0,1,0,1,0,1)
#' compute_scores(response_df,eta,gamma,lambda,psi,map=map,plot=FALSE)
compute_scores<-function(mydata,...) {
  pb<-txtProgressBar(min=0,max=nrow(mydata),style=3)
  ability<-c()
  for (i in 1:nrow(mydata)){
    setTxtProgressBar(pb,i)
    response<-as.numeric(mydata[i,])
    ability<-c(ability,compute_ability(response,...)$ability_map)
  }
  close(pb)
  return(ability)
}
##########################################################################################
# COMPUTE MAP
##########################################################################################
#' @title Simulate prior distribution
#' @param eta vector
#' @param mean numeric
#' @param sd numeric
#' @keywords tirt irt 
#' @export
#' @examples
#' eta<-seq(-6,6,by=0.1)
#' compute_map(eta=eta,mean=0,sd=1)
compute_map<-function(eta,mean=0,sd=1) {
  prior_density=stats::dnorm(eta,mean=mean,sd=sd)
  map<-prior_density/sum(prior_density)    # after division sum(AXr)=1
  return(map)
}
# ##########################################################################################
# # GET DATA FROM OUTPUT MODEL
# ##########################################################################################
# #' @title Simulate prior distribution
# #' @param model mplus thurstonian cfa model with 3 traits
# #' @import MplusAutomation
# #' @keywords tirt irt
# #' @export
# get_mplus_thu_3t<-function(model) {
#   names_model<-model$parameters$unstandardized[model$parameters$unstandardized$paramHeader %in% "Residual.Variances",]$param
#   psi<-model$parameters$unstandardized[model$parameters$unstandardized$paramHeader %in% "Residual.Variances",]$est #PSI
#   gamma<-model$parameters$unstandardized[model$parameters$unstandardized$paramHeader %in% "Thresholds",]$est #GAMMA
#   l1<-model$parameters$unstandardized[model$parameters$unstandardized$paramHeader %in% "TRAIT1.BY",]$est # LAMBDA
#   l2<-model$parameters$unstandardized[model$parameters$unstandardized$paramHeader %in% "TRAIT2.BY",]$est # LAMBDA
#   l3<-model$parameters$unstandardized[model$parameters$unstandardized$paramHeader %in% "TRAIT3.BY",]$est # LAMBDA
#   items<-length(names_model)
#   d1<-model$savedata[,response_dimension(c(1:items),3,c(1,2))]
#   d2<-model$savedata[,response_dimension(c(1:items),3,c(1,3))]
#   d3<-model$savedata[,response_dimension(c(1:items),3,c(2,3))]
#   g1<-gamma[response_dimension(c(1:items),3,c(1,2))]
#   g2<-gamma[response_dimension(c(1:items),3,c(1,3))]
#   g3<-gamma[response_dimension(c(1:items),3,c(2,3))]
#   psi1<-psi[response_dimension(c(1:items),3,c(1,2))]
#   psi2<-psi[response_dimension(c(1:items),3,c(1,3))]
#   psi3<-psi[response_dimension(c(1:items),3,c(2,3))]
#   result<-list(names_model=names_model,psi=psi,gamma=gamma,l1=l1,l2=l2,l3=l3,d1=d1,d2=d2,d3=d3,g1=g1,g2=g2,g3=g3,psi1=psi1,psi2=psi2,psi3=psi3)
#   return(result)
# }
##########################################################################################
# Pure base R Thurstonian IRT scoring (MAP / Empirical Bayes Modal)
# Reproduces thurstonianIRT::predict() for lavaan-fitted TIRT models.
##########################################################################################

##########################################################################################
# Extract TIRT parameters All pieces are aligned to the row order of Lambda
##########################################################################################
#' @title Extract Thurstonian IRT Parameters from a lavaan-Fitted Model
#' @description Extracts and aligns the core parameter blocks needed for
#' Thurstonian IRT scoring from a fitted lavaan object (as stored in
#' \code{fit_lavaan_obj$fit}). The returned pieces are aligned to the row
#' order of \code{Lambda}, which is critical for correct downstream scoring.
#'
#' Specifically, this function returns:
#' \itemize{
#' \item \code{Lambda}: factor loading matrix.
#' \item \code{theta_diag}: residual variances (diagonal of \code{theta}),
#' reordered to \code{rownames(Lambda)} when names are available.
#' \item \code{tau}: thresholds from \code{tau}, with row suffixes like
#' \code{"|t1"} removed and reordered to \code{rownames(Lambda)}.
#' \item \code{nu}: indicator intercepts; defaults to zero when unavailable.
#' \item \code{Psi}: latent covariance matrix (\code{psi}).
#' }
#'
#' @param fit_lavaan_obj A fitted object containing a lavaan fit in
#' \code{$fit}. For example, an object produced by a wrapper that stores
#' a lavaan model under \code{fit_lavaan_obj$fit}.
#'
#' @return A named list with elements:
#' \code{Lambda}, \code{theta_diag}, \code{tau}, \code{nu}, and \code{Psi}.
#'
#' @details Threshold rows in lavaan are often named like \code{"item|t1"}.
#' This function strips everything after \code{"|"} before matching thresholds
#' to indicators.
#'
#' @keywords tirt lavaan irt scoring
#' @export
#'
#' @examples
#' library(thurstonianIRT)
#' data("triplets")
#' # define the blocks of items
#' blocks <-
#'   set_block(c("i1", "i2", "i3"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, 1, 1)) +
#'   set_block(c("i4", "i5", "i6"), traits = c("t1", "t2", "t3"),
#'             signs = c(-1, 1, 1)) +
#'   set_block(c("i7", "i8", "i9"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, 1, -1)) +
#'   set_block(c("i10", "i11", "i12"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, -1, 1))
#' # generate the data to be understood by 'thurstonianIRT'
#' triplets_long <- make_TIRT_data(
#'   data = triplets, blocks = blocks, direction = "larger",
#'   format = "pairwise", family = "bernoulli", range = c(0, 1)
#' )
#' # fit the data using lavaan
#' fit <- fit_TIRT_lavaan(triplets_long)
#' pars <- extract_tirt_params(fit)
extract_tirt_params<-function(fit_lavaan_obj) {
  est<-lavaan::lavInspect(fit_lavaan_obj$fit, "est")
  Lambda<-est$lambda
  ind<-rownames(Lambda)
  
  # residual variances -- align to Lambda by indicator name
  theta_diag<-diag(est$theta)
  if(!is.null(names(theta_diag))) {
    theta_diag<-theta_diag[ind]
  }
  
  # thresholds -- tau rownames look like "i12b1|t1"; strip "|t..." and align
  tau_mat<-est$tau
  tau_names<-sub("\\|.*$", "", rownames(tau_mat))
  tau_vec<-as.numeric(tau_mat)
  names(tau_vec)<-tau_names
  tau_vec<-tau_vec[ind]
  
  # intercepts -- usually fixed at 0 for binary indicators, but check anyway
  nu_vec<-rep(0, length(ind))
  names(nu_vec)<-ind
  if (!is.null(est$nu)) {
    nu_named<-as.numeric(est$nu)
    names(nu_named)<-rownames(est$nu)
    if (all(ind %in% names(nu_named))) {
      nu_vec<-nu_named[ind]
    }
  }
  list(Lambda=Lambda,theta_diag=theta_diag,tau=tau_vec,nu=nu_vec,Psi=est$psi)
}
##########################################################################################
# SCORE RESPONSE PATTERN
##########################################################################################
#' @title Score a Single Thurstonian IRT Response Pattern (MAP / EBM)
#' @description Computes the maximum a posteriori (MAP), also called empirical
#' Bayes modal (EBM), estimate of latent traits for one binary/ordinal
#' response pattern under a Thurstonian IRT parameterization.
#'
#' Missing responses are allowed and are ignored in the likelihood.
#'
#' @param pattern Numeric vector of observed responses for one person, typically
#' coded 0/1, with optional NA values for missing responses. Its order must
#' match the row order of Lambda (or be pre-aligned before calling).
#' @param Lambda Matrix of factor loadings with rows as observed indicators and
#' columns as latent traits.
#' @param theta_diag Numeric vector of residual variances (diagonal of theta),
#' aligned to rows of Lambda.
#' @param tau Numeric vector of thresholds, aligned to rows of Lambda.
#' @param Psi Latent covariance matrix (traits x traits), positive definite.
#' @param nu Optional numeric vector of indicator intercepts aligned to rows of
#' Lambda. If NULL, a zero vector is used.
#' @param init Optional numeric vector of starting values for optimization.
#' If NULL, starts at zeros.
#' @param control Named list of control arguments passed to optim, merged with
#' defaults reltol = 1e-10 and maxit = 500.
#'
#' @return Named numeric vector of latent trait MAP estimates, with names taken
#' from colnames(Lambda).
#'
#' @details The function maximizes the posterior:
#' likelihood from a probit measurement model plus a multivariate normal
#' prior on traits with covariance Psi. Optimization uses BFGS via optim
#' with an analytic gradient.
#'
#' @keywords tirt irt map ebm scoring
#' @export
#'
#' @examples
#' library(thurstonianIRT)
#' data("triplets")
#' # define the blocks of items
#' blocks <-
#'   set_block(c("i1", "i2", "i3"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, 1, 1)) +
#'   set_block(c("i4", "i5", "i6"), traits = c("t1", "t2", "t3"),
#'             signs = c(-1, 1, 1)) +
#'   set_block(c("i7", "i8", "i9"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, 1, -1)) +
#'   set_block(c("i10", "i11", "i12"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, -1, 1))
#' # generate the data to be understood by 'thurstonianIRT'
#' triplets_long <- make_TIRT_data(
#'   data = triplets, blocks = blocks, direction = "larger",
#'   format = "pairwise", family = "bernoulli", range = c(0, 1)
#' )
#' # fit the data using lavaan
#' fit <- fit_TIRT_lavaan(triplets_long)
#' pars <- extract_tirt_params(fit)
#' pattern<-as.numeric(triplets[1,])
#' score_tirt_pattern(pattern,Lambda=pars$Lambda,theta_diag=pars$theta_diag,
#'                    tau=pars$tau,Psi=pars$Psi,nu=NULL,init=NULL,
#'                    control=list())
score_tirt_pattern<-function(pattern,Lambda,theta_diag,tau,Psi,nu=NULL,init=NULL,control=list()) {
  n_traits<-ncol(Lambda)
  if (is.null(nu)) 
    nu<-rep(0,nrow(Lambda))
  obs<-!is.na(pattern)
  L<-Lambda[obs, , drop=FALSE]
  y<-as.numeric(pattern[obs])
  s<-sqrt(theta_diag[obs])
  th<-tau[obs]-nu[obs]
  # iPsi<-solve(Psi)
  iPsi<-my_solve(Psi)
  nll<-function(eta) {
    z <-(as.numeric(L %*% eta) - th) / s
    p <-pmin(pmax(pnorm(z), 1e-15), 1 - 1e-15)
    ll<-sum(y * log(p) + (1 - y) * log1p(-p))
    lp<--0.5 * sum(eta * (iPsi %*% eta))
    -(ll + lp)
  }
  gnll<-function(eta) {
    z<-(as.numeric(L %*% eta) - th) / s
    p<-pmin(pmax(pnorm(z), 1e-15), 1 - 1e-15)
    phi<-dnorm(z)
    w<-(y - p) * phi / (p * (1 - p)) / s
    -(as.numeric(crossprod(L, w)) - as.numeric(iPsi %*% eta))
  }
  
  if (is.null(init)) init<-rep(0, n_traits)
  ctrl<-modifyList(list(reltol=1e-10, maxit=500), control)
  
  res<-optim(init,nll,gr=gnll,hessian=TRUE,method="BFGS",control=ctrl)
  setNames(res$par, colnames(Lambda))
}
##########################################################################################
# SOLVE
##########################################################################################
#' @title Solve Linear Systems or Invert a Matrix (Gauss-Jordan)
#' @description Solves matrix equations of the form A X = B using
#' Gauss-Jordan elimination with partial pivoting.
#'
#' In simple terms:
#' this function takes a square matrix A and finds X so that multiplying
#' A by X gives B.
#'
#' If B is not provided, it uses the identity matrix and returns
#' the inverse of A.
#'
#' @param a Numeric square matrix A.
#' @param b Optional numeric vector or matrix B. Must have the same number
#' of rows as A. If omitted, B is set to the identity matrix.
#'
#' @return
#' If b is a matrix, returns matrix X solving A X = B.
#'
#' If b is a vector, returns vector x solving A x = b.
#'
#' If b is missing, returns the inverse of A.
#'
#' @details
#' Mathematical meaning:
#' \deqn{A X = B}
#' where A is known, B is known, and X is unknown.
#'
#' Algebra meaning:
#' each column of X is the solution to one linear system with the same A.
#'
#' Computational method:
#' the function builds the augmented matrix [A | B], then applies
#' row operations until the left side becomes the identity matrix.
#' At that point, the right side is X.
#'
#' Partial pivoting is used for better numerical stability:
#' in each column, it swaps in the row with the largest absolute pivot.
#'
#' The function stops with an error if A is singular (non-invertible).
#'
#' @keywords algebra matrix linear-equations
#' @export
#'
#' @examples
#' # Example 1: solve A x = b
#' A <- matrix(c(2, 1,
#' 1, 3), nrow = 2, byrow = TRUE)
#' b <- c(1, 2)
#' x <- my_solve(A, b)
#' x
#' # Check: A %% x should equal b
#' A %% x
#'
#' # Example 2: solve A X = B (multiple right-hand sides)
#' B <- cbind(c(1, 2), c(0, 1))
#' X <- my_solve(A, B)
#' X
#' # Check: A %% X should equal B
#' A %% X
#'
#' # Example 3: inverse of A (when b is omitted)
#' A_inv <- my_solve(A)
#' A_inv
#' # Check: A %% A_inv should be identity
#' A %% A_inv
my_solve <- function(a, b) {
  a <- as.matrix(a)
  n <- nrow(a)
  if (ncol(a) != n) stop("'a' must be square")
  
  if (missing(b)) b <- diag(n)        # default: invert a
  b <- as.matrix(b)
  if (nrow(b) != n) stop("'a' and 'b' have incompatible dimensions")
  
  # Build augmented matrix [a | b]
  M <- cbind(a, b)
  
  # Gauss-Jordan elimination with partial pivoting
  for (i in seq_len(n)) {
    # Find pivot row (largest absolute value in column i, from row i down)
    pivot <- which.max(abs(M[i:n, i])) + i - 1
    if (M[pivot, i] == 0) stop("matrix is singular")
    
    # Swap rows if needed
    if (pivot != i) M[c(i, pivot), ] <- M[c(pivot, i), ]
    
    # Normalize pivot row
    M[i, ] <- M[i, ] / M[i, i]
    
    # Eliminate column i in all other rows
    for (j in seq_len(n)) {
      if (j != i) M[j, ] <- M[j, ] - M[j, i] * M[i, ]
    }
  }
  
  # Right half is the solution
  x <- M[, (n + 1):ncol(M)]
  if (ncol(b) == 1) x <- as.vector(x)
  x
}
##########################################################################################
# SCORE MULTIPLE RESPONSE PATTERNS
##########################################################################################
#' @title Score Multiple Thurstonian IRT Response Patterns (MAP / EBM)
#' @description Scores many respondents at once using Thurstonian IRT
#'   parameters and returns MAP (empirical Bayes modal) latent trait estimates
#'   for each row in patterns.
#'
#'   In simple terms:
#'   this function applies score_tirt_pattern to every respondent, after first
#'   checking and aligning item columns so they match Lambda row order.
#'
#' @param patterns A matrix or data.frame of response patterns
#'   (rows = respondents, columns = pair/items).
#' @param Lambda Loading matrix (rows = pair/items, columns = latent traits).
#' @param theta_diag Numeric vector of residual variances aligned to
#'   rows of Lambda.
#' @param tau Numeric vector of thresholds aligned to rows of Lambda.
#' @param Psi Latent covariance matrix (traits x traits).
#' @param nu Optional numeric vector of indicator intercepts aligned to rows
#'   of Lambda. If NULL, zeros are used in score_tirt_pattern.
#'
#' @return A numeric matrix of latent scores:
#'   rows correspond to respondents in patterns,
#'   columns correspond to traits in colnames(Lambda).
#'
#' @details
#' Name alignment is the key safeguard:
#' if both rownames(Lambda) and colnames(patterns) are present,
#' patterns is reordered to match Lambda row order before scoring.
#'
#' If required Lambda names are missing from patterns, the function stops
#' with an informative error. If names are unavailable, positional alignment
#' is assumed and a warning is issued.
#'
#' @keywords tirt irt scoring map ebm
#' @export
#' @examples
#' library(thurstonianIRT)
#' data("triplets")
#' # define the blocks of items
#' blocks <-
#'   set_block(c("i1", "i2", "i3"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, 1, 1)) +
#'   set_block(c("i4", "i5", "i6"), traits = c("t1", "t2", "t3"),
#'             signs = c(-1, 1, 1)) +
#'   set_block(c("i7", "i8", "i9"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, 1, -1)) +
#'   set_block(c("i10", "i11", "i12"), traits = c("t1", "t2", "t3"),
#'             signs = c(1, -1, 1))
#' # generate the data to be understood by 'thurstonianIRT'
#' triplets_long <- make_TIRT_data(
#'   data = triplets, blocks = blocks, direction = "larger",
#'   format = "pairwise", family = "bernoulli", range = c(0, 1)
#' )
#' # fit the data using lavaan
#' fit <- fit_TIRT_lavaan(triplets_long)
#' pars <- extract_tirt_params(fit)
#' patterns<-as.matrix(triplets)
#' score_tirt(patterns,Lambda=pars$Lambda,theta_diag=pars$theta_diag,
#'            tau=pars$tau,Psi=pars$Psi,nu=NULL)
#' # Check same scores from thurstonianIRT package
#' triplets_long <- make_TIRT_data(data = triplets,
#'                                 blocks = blocks, 
#'                                 direction = "larger",
#'                                 format = "pairwise", 
#'                                 family = "bernoulli", 
#'                                 range = c(0, 1))
#' scores_thurstonianIRT<-predict(fit)
#' scores<-score_tirt(patterns,Lambda=pars$Lambda,theta_diag=pars$theta_diag,
#'         tau=pars$tau,Psi=pars$Psi,nu=NULL)
#' head(reshape2::recast(scores_thurstonianIRT,formula=id~trait,id.var=1:2))
#' head(scores)
score_tirt<-function(patterns, Lambda, theta_diag, tau, Psi, nu=NULL) {
  patterns<-as.matrix(patterns)
  
  # align columns to Lambda rows by name -- the critical step
  if (is.null(rownames(Lambda)) || is.null(colnames(patterns))) {
    warning("Lambda rows or pattern columns are unnamed; assuming positional alignment.")
  } else {
    miss<-setdiff(rownames(Lambda), colnames(patterns))
    if (length(miss) > 0) {
      stop("Pair names in Lambda not found in patterns:\n  ",
           paste(miss, collapse=", "))
    }
    patterns<-patterns[, rownames(Lambda), drop=FALSE]
  }
  
  scores<-t(apply(patterns,1,score_tirt_pattern,
                  Lambda=Lambda,
                  theta_diag=theta_diag,
                  tau=tau,
                  Psi=Psi,
                  nu=nu))
  colnames(scores)<-colnames(Lambda)
  scores
}
##########################################################################################
# Diagnostic -- run this first if scores still don't match predict()
##########################################################################################
# tirt_diagnose<-function(fit_lavaan_obj, df_rank) {
#   est<-lavaan::lavInspect(fit_lavaan_obj$fit, "est")
#   cat("Lambda rows  (first 5):", head(rownames(est$lambda), 5), "\n")
#   cat("Tau    rows  (first 5):", head(rownames(est$tau),    5), "\n")
#   cat("Theta  rows  (first 5):", head(rownames(est$theta),  5), "\n")
#   cat("df_rank cols (first 5):", head(colnames(df_rank),    5), "\n\n")
#   
#   same_lt<-identical(rownames(est$lambda),sub("\\|.*$", "",rownames(est$tau)))
#   same_lr<-identical(rownames(est$lambda),colnames(df_rank))
#   cat("lambda vs tau   row order identical:", same_lt, "\n")
#   cat("lambda vs df_rank col order identical:", same_lr, "\n")
#   
#   cat("\nFactor mean (alpha) present?", !is.null(est$alpha), "\n")
#   if (!is.null(est$alpha)) {
#     cat("alpha:\n"); print(as.numeric(est$alpha))
#   }
#   cat("Indicator intercept (nu) max abs:",
#       if (is.null(est$nu)) 0 else max(abs(as.numeric(est$nu))), "\n")
# }





