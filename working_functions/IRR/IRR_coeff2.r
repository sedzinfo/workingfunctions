##########################################################################################
#
##########################################################################################
#AGREE.COEFF2.R (September 26, 2015)
#Description: This script file contains a series of R functions for computing various agreement coefficients
#for 2 raters when the input data file is in the form of a q x q contingency table 
#(q being the number of categories) showing the distribution of subjects by rater, and by category.
#Author: Kilem L. Gwet, Ph.D. (Send comments to: gwet@agreestat.com. Thank you)
##########################################################################################
#kappa2.table: Cohen's kappa (Cohen(1960)) coefficient and its standard error for 2 raters when input dataset is a contingency table 
#The input data "ratings" is a qxq contingency table showing the distribution of subjects by rater, when q is the number of categories.
##########################################################################################
kappa2.table<-function(ratings,weights=diag(ncol(ratings)),conflev=0.95,N=Inf,print=TRUE){
  if(dim(ratings)[1] != dim(ratings)[2]) {
    stop('The contingency table should have the same number of rows and columns!') 
  }
  n<-sum(ratings) # number of subjects
  f<-n/N # final population correction  
  q<-ncol(ratings) # number of categories 
  pa<-sum(weights * ratings/n) # percent agreement
  pk.<-(ratings%*%rep(1,q))/n
  p.l<-t((t(rep(1,q))%*%ratings)/n)
  pe<-sum(weights*(pk.%*%t(p.l)))
  kappa<-(pa - pe)/(1 - pe) # weighted kappa
  # 2 raters special case variance
  pkl<-ratings/n
  pb.k<-weights %*% p.l
  pbl.<-t(weights) %*% pk.
  sum1<-0
  for(k in 1:q){
    for(l in 1:q){
      sum1<-sum1 + pkl[k,l]* (weights[k,l]-(1-kappa)*(pb.k[k] + pbl.[l]))^2
    }
  }
  var.kappa<-((1-f)/(n*(1-pe)^2)) * (sum1 - (pa-2*(1-kappa)*pe)^2)
  stderr<-sqrt(var.kappa)# kappa standard error
  p.value<-2*(1-pt(kappa/stderr,n-1))
  
  lcb<-kappa - stderr*qt(1-(1-conflev)/2,n-1) # lower confidence bound
  ucb<-min(1,kappa + stderr*qt(1-(1-conflev)/2,n-1)) # upper confidence bound
  if(print==TRUE){
    cat("Cohen's Kappa Coefficient\n")
    cat('=========================\n')	
    cat('Kappa coefficient:',kappa,'\n')
    cat('Standard error:',stderr,'\n')
    cat(conflev*100,'% Confidence Interval: (',lcb,',',ucb,')\n')
    cat('P-value: ',p.value,'\n')
    cat('Percent agreement:',pa,'\n')
    cat('Percent chance agreement:',pe,'\n')
    if (abs(sum(weights)-q)>1e-13){
      cat("\n Weights\n")
      cat(' ********\n')
      print(weights)	
    }
  }
  invisible(c(pa,pe,kappa,stderr,p.value))
}
##########################################################################################
#
##########################################################################################
#scott2.table: Scott's pi coefficient (Scott(1955)) and its standard error for 2 raters when input dataset is a contingency table 
#-------------
#The input data "ratings" is a qxq contingency table showing the distribution of
#subjects by rater, when q is the number of categories.
#======================================================================================
scott2.table<-function(ratings,weights=diag(ncol(ratings)),conflev=0.95,N=Inf,print=TRUE){
  if(dim(ratings)[1] != dim(ratings)[2]){
    stop('The contingency table should have the same number of rows and columns!') 
  }
  n<-sum(ratings) # number of subjects
  f<-n/N # final population correction  
  q<-ncol(ratings) # number of categories 
  pa<-sum(weights * ratings/n) # percent agreement
  
  pk.<-(ratings%*%rep(1,q))/n
  p.l<-t((t(rep(1,q))%*%ratings)/n)
  pi.k<-(pk.+p.l)/2
  pe<-sum(weights*(pi.k%*%t(pi.k)))
  scott<-(pa - pe)/(1 - pe) # weighted scott's pi coefficint
  # 2 raters special case variance
  pkl<-ratings/n	     #p_{kl}	
  pb.k<-weights %*% p.l    #\ov{p}_{+k}
  pbl.<-t(weights) %*% pk. #\ov{p}_{l+}
  pbk <-(pb.k + pbl.)/2    #\ov{p}_{k}
  sum1<-0
  for(k in 1:q){
    for(l in 1:q){
      sum1<-sum1 + pkl[k,l] * (weights[k,l]-(1-scott)*(pbk[k] + pbk[l]))^2
    }
  }
  var.scott<-((1-f)/(n*(1-pe)^2)) * (sum1 - (pa-2*(1-scott)*pe)^2)
  stderr<-sqrt(var.scott)# Scott's standard error
  p.value<-2*(1-pt(scott/stderr,n-1))
  lcb<-scott - stderr*qt(1-(1-conflev)/2,n-1) # lower confidence bound
  ucb<-min(1,scott + stderr*qt(1-(1-conflev)/2,n-1)) # upper confidence bound
  if(print==TRUE){
    cat("Scott's Pi Coefficient\n")
    cat('======================\n')	
    cat('Scott coefficient:',scott,'\n')
    cat('Standard error:',stderr,'\n')
    cat(conflev*100,'% Confidence Interval: (',lcb,',',ucb,')\n')
    cat('P-value: ',p.value,'\n')
    cat('Percent agreement:',pa,'\n')
    cat('Percent chance agreement:',pe,'\n')
    if (abs(sum(weights)-q)>1e-13){
      cat("\n Weights\n")
      cat(' ********\n')
      print(weights)	
    }
  }
  invisible(c(pa,pe,scott,stderr,p.value))
}
##########################################################################################
#
##########################################################################################
#gwet.ac1.table: Gwet's AC1/Ac2 coefficient (Gwet(2008)) and its standard error for 2 raters when input dataset is a contingency table 
#-------------
#The input data "ratings" is a qxq contingency table showing the distribution of
#subjects by rater, when q is the number of categories.
#======================================================================================
gwet.ac1.table<-function(ratings,weights=diag(ncol(ratings)),conflev=0.95,N=Inf,print=TRUE){
  if(dim(ratings)[1] != dim(ratings)[2]){
    stop('The contingency table should have the same number of rows and columns!') 
  }
  n<-sum(ratings) # number of subjects
  f<-n/N # final population correction  
  q<-ncol(ratings) # number of categories 
  pa<-sum(weights * ratings/n) # percent agreement
  pk.<-(ratings%*%rep(1,q))/n
  p.l<-t((t(rep(1,q))%*%ratings)/n)
  pi.k<-(pk.+p.l)/2
  tw<-sum(weights)
  pe<-tw * sum(pi.k *(1-pi.k))/(q*(q-1))
  gwet.ac1<-(pa - pe)/(1 - pe) # gwet's ac1/ac2 coefficint
  # calculation of variance - standard error - confidence interval - p-value
  pkl<-ratings/n	     #p_{kl}	
  sum1<-0
  for(k in 1:q){
    for(l in 1:q){
      sum1<-sum1 + pkl[k,l] * (weights[k,l]-2*(1-gwet.ac1)*tw*(1-(pi.k[k] + pi.k[l])/2)/(q*(q-1)))^2
    }
  }
  var.gwet<-((1-f)/(n*(1-pe)^2)) * (sum1 - (pa-2*(1-gwet.ac1)*pe)^2)
  stderr<-sqrt(var.gwet)# ac1's standard error
  p.value<-2*(1-pt(gwet.ac1/stderr,n-1))
  lcb<-gwet.ac1 - stderr*qt(1-(1-conflev)/2,n-1) # lower confidence bound
  ucb<-min(1,gwet.ac1 + stderr*qt(1-(1-conflev)/2,n-1)) # upper confidence bound
  if(print==TRUE){
    if (abs(sum(weights)-q)>1e-13){
      cat("Gwet's AC2 Coefficient\n")
      cat('==========================\n')	
      cat('AC2 coefficient:',gwet.ac1,'\n')
      cat('Standard error:',stderr,'\n')
      cat(conflev*100,'% Confidence Interval: (',lcb,',',ucb,')\n')
      cat('P-value: ',p.value,'\n')
      cat('Percent agreement:',pa,'\n')
      cat('Percent chance agreement:',pe,'\n')
      cat("\n Weights\n")
      cat(' ********\n')
      print(weights)	
    }else{
      cat("Gwet's AC1 Coefficient\n")
      cat('==========================\n')	
      cat('AC1 coefficient:',gwet.ac1,'\n')
      cat('Standard error:',stderr,'\n')
      cat(conflev*100,'% Confidence Interval: (',lcb,',',ucb,')\n')
      cat('P-value: ',p.value,'\n')
      cat('Percent agreement:',pa,'\n')
      cat('Percent chance agreement:',pe,'\n')
    }
  }
  invisible(c(pa,pe,gwet.ac1,stderr,p.value))
}
##########################################################################################
#
##########################################################################################
#bp2.table: Brennan-Prediger coefficient (Brennan & Prediger (1981)) and its standard error for 2 raters when input dataset is a contingency table 
#-------------
#The input data "ratings" is a qxq contingency table showing the distribution of
#subjects by rater, when q is the number of categories.
#======================================================================================
bp2.table<-function(ratings,weights=diag(ncol(ratings)),conflev=0.95,N=Inf,print=TRUE){
  if(dim(ratings)[1] != dim(ratings)[2]){
    stop('The contingency table should have the same number of rows and columns!') 
  }
  n<-sum(ratings) # number of subjects
  f<-n/N # final population correction  
  q<-ncol(ratings) # number of categories 
  pa<-sum(weights * ratings/n) # percent agreement
  tw<-sum(weights)
  pe<-tw/(q^2)
  bp.coeff<-(pa - pe)/(1 - pe) # Brennan-Prediger coefficint
  # calculation of variance - standard error - confidence interval - p-value
  pkl<-ratings/n	     #p_{kl}	
  sum1<-0
  for(k in 1:q){
    for(l in 1:q){
      sum1<-sum1 + pkl[k,l] * weights[k,l]^2
    }
  }
  var.bp<-((1-f)/(n*(1-pe)^2)) * (sum1 - pa^2)
  stderr<-sqrt(var.bp)# bp's standard error
  p.value<-2*(1-pt(bp.coeff/stderr,n-1))
  lcb<-bp.coeff - stderr*qt(1-(1-conflev)/2,n-1) # lower confidence bound
  ucb<-min(1,bp.coeff + stderr*qt(1-(1-conflev)/2,n-1)) # upper confidence bound
  if(print==TRUE){
    cat("Brennan-Prediger Coefficient\n")
    cat('============================\n')	
    cat('B-P coefficient:',bp.coeff,'\n')
    cat('Standard error:',stderr,'\n')
    cat(conflev*100,'% Confidence Interval: (',lcb,',',ucb,')\n')
    cat('P-value: ',p.value,'\n')
    cat('Percent Agreement:',pa,'\n')
    cat('Percent Chance Agreement:',pe,'\n')
    if (abs(sum(weights)-q)>1e-13){
      cat("\n Weights\n")
      cat(' ********\n')
      print(weights)	
    }
  }
  invisible(c(pa,pe,bp.coeff,stderr,p.value))
}
##########################################################################################
#
##########################################################################################
#krippen2.table:	Krippendorff's alpha coefficient and its standard error for 2 raters when input dataset is a contingency table. 
#Krippendorff, K. (1970). "Estimating the reliability, systematic error, and random error of interval data," Educational and Psychological Measurement, 30, 61-70.
#--------------------------------------------------------------------------------------
#The input data "ratings" is a qxq contingency table showing the distribution of subjects by rater, when q is the number of categories.
#======================================================================================
krippen2.table<-function(ratings,weights=diag(ncol(ratings)),conflev=0.95,N=Inf,print=TRUE){
  if(dim(ratings)[1] != dim(ratings)[2]){
    stop('The contingency table should have the same number of rows and columns!') 
  }
  n<-sum(ratings) # number of subjects
  f<-n/N # final population correction  
  q<-ncol(ratings) # number of categories 
  epsi = 1/(2*n)
  pa0<-sum(weights * ratings/n)
  pa<-(1-epsi)*pa0 + epsi # percent agreement
  pk.<-(ratings%*%rep(1,q))/n
  p.l<-t((t(rep(1,q))%*%ratings)/n)
  pi.k<-(pk.+p.l)/2
  pe<-sum(weights*(pi.k%*%t(pi.k)))
  kripp.coeff<-(pa - pe)/(1 - pe) # weighted Krippen's alpha coefficint
  # calculating variance
  kcoeff<-(pa0 - pe)/(1 - pe)
  pkl<-ratings/n	     #p_{kl}	
  pb.k<-weights %*% p.l    #\ov{p}_{+k}
  pbl.<-t(weights) %*% pk. #\ov{p}_{l+}
  pbk <-(pb.k + pbl.)/2    #\ov{p}_{k}
  sum1<-0
  for(k in 1:q){
    for(l in 1:q){
      sum1<-sum1 + pkl[k,l] * (weights[k,l]-(1-kcoeff)*(pbk[k] + pbk[l]))^2
    }
  }
  var.kripp<-((1-f)/(n*(1-pe)^2)) * (sum1 - (pa0-2*(1-kcoeff)*pe)^2)
  stderr<-sqrt(var.kripp)# Kripp. alpha's standard error
  p.value<-2*(1-pt(kripp.coeff/stderr,n-1))
  lcb<-kripp.coeff - stderr*qt(1-(1-conflev)/2,n-1) # lower confidence bound
  ucb<-min(1,kripp.coeff + stderr*qt(1-(1-conflev)/2,n-1)) # upper confidence bound
  if(print==TRUE){
    cat("Krippendorff's alpha Coefficient\n")
    cat('================================\n')	
    cat('Alpha coefficient:',kripp.coeff,'\n')
    cat('Standard error:',stderr,'\n')
    cat(conflev*100,'% Confidence Interval: (',lcb,',',ucb,')\n')
    cat('P-value: ',p.value,'\n')
    cat('Percent agreement:',pa,'\n')
    cat('Percent chance agreement:',pe,'\n')
    if (abs(sum(weights)-q)>1e-13){
      cat("\n Weights\n")
      cat(' ********\n')
      print(weights)	
    }
  }
  invisible(c(pa,pe,kripp.coeff,stderr,p.value))
}
