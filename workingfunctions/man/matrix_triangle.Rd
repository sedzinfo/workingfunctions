% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTIONS_MATRIX.R
\name{matrix_triangle}
\alias{matrix_triangle}
\title{Return upper or lower matrix triangle}
\usage{
matrix_triangle(m, off_diagonal = NA, diagonal = NULL, type = "lower")
}
\arguments{
\item{m}{matrix}

\item{off_diagonal}{off diagonal value}

\item{diagonal}{diagonal value. If NULL it returns the diagonal of the input matrix}

\item{type}{"upper" displays upper triangle, "lower" displays lower triangle}
}
\description{
Return upper or lower matrix triangle
}
\examples{
m<-matrix(1:9,nrow=3,ncol=3)
matrix_triangle(m=m)
matrix_triangle(m=m,diagonal=NA,type="lower")
matrix_triangle(m=m,diagonal=NULL,type="lower")
matrix_triangle(m=m,diagonal=NA,type="upper")
matrix_triangle(m=m,diagonal=NULL,type="upper")
}
\keyword{functions}
\keyword{matrix}
