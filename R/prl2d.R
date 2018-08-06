#' projection of 2d line on a straight line
#'
#' @param co a numeric two-column matrix with the coordinates of the line in columns
#' @param alpha angle of the projection line with respect to the x-axis
#' @return A 2-column matrix with the projected x and y coordinates
#' ('xp' and 'yp').
#' @description The orthogonal projection of a line v onto the line s
#' (both represented by 2d vectors)
#' is ( (v's)/(s's) ) * s
#' Here v's is the inner product of vectors v' and s, and s's is the inner
#' product of s.
#' The line s is assumed to cross v at the first coordinate in v.
#' @examples
#' co <- cbind( x=1:5, y=c(3,5,6,4,7))
#' prl2d(co)
#' prl2d(co,alpha=10)
#' @export

prl2d <- function(co,alpha=0){

  if(is.data.frame(co)){
    co <- as.matrix(co)
  }

  # matrix with first coordinate
  startpt <- matrix(rep(co[1,],nrow(co)),ncol=ncol(co),byrow=TRUE)

  V <- diff(co)
  s <- c(1, tan(pi*alpha/180))

  VinS <- apply(V,1,FUN='%*%',s)
  out <- (VinS/c(s%*%s)) %o% s
  out <- apply( rbind(c(0,0), out),2, FUN=cumsum ) + startpt
  colnames(out) <- c('xp','yp')
  return(out)
}
