##########################################################################################
# MATRIX DISPLAY DIAGONAL
##########################################################################################
#' @title Return upper or lower matrix triangle
#' @param m matrix
#' @param off_diagonal off diagonal value
#' @param diagonal diagonal value. If NULL it returns the diagonal of the input matrix
#' @param type "upper" displays upper triangle, "lower" displays lower triangle
#' @keywords functions matrix
#' @export
#' @examples
#' m<-matrix(1:9,nrow=3,ncol=3)
#' matrix_triangle(m=m)
#' matrix_triangle(m=m,diagonal=NA,type="lower")
#' matrix_triangle(m=m,diagonal=NULL,type="lower")
#' matrix_triangle(m=m,diagonal=NA,type="upper")
#' matrix_triangle(m=m,diagonal=NULL,type="upper")
matrix_triangle<-function(m,off_diagonal=NA,diagonal=NULL,type="lower") {
  m<-as.matrix(m)
  if (!is.null(dim(m))) {
    matrix_diagonal<-diag(m)
    if(type=="lower") {
      md<-lower.tri(m,diag=TRUE)*m
      md[upper.tri(md)]<-off_diagonal
    }
    if(type=="upper") {
      md<-upper.tri(m,diag=TRUE)*m
      md[lower.tri(md)]<-off_diagonal
    }
    if(!is.null(diagonal))
      diag(md)<-diagonal
    return(md)
  } else
    return(m)
}
##########################################################################################
# MATRIX DISPLAY UPPER LOWER TRIANGLE
##########################################################################################
#' @title Return upper diagonal from one matrix and lower diagonal from another matrix
#' @param m_upper matrix
#' @param m_lower matrix
#' @param diagonal if "upper" it returns upper diagonal if "lower" it returns lower diagonal if NA returns NA in diagonal otherwise it returns any value spesified
#' @keywords functions matrix
#' @export
#' @examples
#' m1<-matrix(1:9,nrow=3,ncol=3)
#' m2<-matrix(11:19,nrow=3,ncol=3)
#' display_upper_lower_triangle(m_upper=m1,m_lower=m2,diagonal="upper")
#' display_upper_lower_triangle(m_upper=m1,m_lower=m2,diagonal="lower")
#' display_upper_lower_triangle(m_upper=m1,m_lower=m2,diagonal=NA)
#' display_upper_lower_triangle(m_upper=m1,m_lower=m2,diagonal=1)
#' display_upper_lower_triangle(m_upper=m1,m_lower=m2,diagonal=c("X1","X2","X3"))
#' display_upper_lower_triangle(m_upper=m1,m_lower=m2,diagonal=c(1,2,3))
#' display_upper_lower_triangle(m_upper=m1,m2)
display_upper_lower_triangle<-function(m_upper,m_lower,diagonal=NA) {
  upper<-matrix_triangle(m_upper,diagonal=NULL,type="upper")
  lower<-matrix_triangle(m_lower,diagonal=NULL,type="lower")
  lower[upper.tri(lower)]<-upper[upper.tri(upper)]
  m<-as.matrix(data.frame(lower))
  if(unique(is.na(diagonal)))
    diag(m)<-NA
  else if(unique(diagonal=="upper"))
    diag(m)<-diag(m_upper)
  else if(unique(diagonal=="lower"))
    diag(m)<-diag(m_lower)
  else
    diag(m)<-diagonal
  return(m)
}
##########################################################################################
# MAKE SYMMETRIC MATRIX
##########################################################################################
#' @title Symmetric Matrix
#' @param matrix matrix
#' @param duplicate "upper" duplicates upper triangle "lower" duplicates lower triangle
#' @param diagonal diagonal values
#' @keywords functions matrix
#' @export
#' @examples
#' m_lower<-matrix_triangle(matrix(1:9,nrow=3,ncol=3),type="lower",diagonal=NA)
#' m_upper<-matrix_triangle(matrix(11:19,nrow=3,ncol=3),type="upper",diagonal=NA)
#' symmetric_matrix(matrix=m_lower,duplicate="lower",diagonal=NA)
#' symmetric_matrix(matrix=m_upper,duplicate="upper",diagonal=NA)
symmetric_matrix<-function(matrix,duplicate="lower",diagonal=NULL) {
  if (missing(diagonal))
    diagonal<-diag(matrix)
  if(duplicate=="lower")
    matrix[upper.tri(matrix)]<-t(matrix)[upper.tri(matrix)]
  if(duplicate=="upper")
    matrix[lower.tri(matrix)]<-t(matrix)[lower.tri(matrix)]
  rownames(matrix)<-colnames(matrix)
  diag(matrix)<-diagonal
  return(matrix)
}
##########################################################################################
# INDEX OFF DIAGONAL
##########################################################################################
#' @title index of off diagonal
#' @param length length of diagonal
#' @keywords functions matrix
#' @export
#' @examples
#' off_diagonal_index(length=6)
off_diagonal_index<-function(length){
  index<-data.frame(x1=0,x2=0,x3=0,x4=0)
  for (i in 1:length) {
    p<-i+1
    m<-i-1
    index[i,]=c(i,i,p,m)
  }
  return(index)
}

