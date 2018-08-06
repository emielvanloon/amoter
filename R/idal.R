#' inverse distance along a line --> still unfinished
#'
#' @param co a numeric matrix with the coordinates of the line in columns
#' (a 2D line would result in a 2-column matrix).
#' @param d the distances along the line for which the coordinates are desired
#' @return A vector with distance along the line from first point in the input matrix. \code{x} and \code{y}.
#' @examples
#' co <- cbind( x=1:5, y=c(3,5,6,4,7))
#' dal(co)

idal <- function(co,d) {

  if(is.data.frame(co)){
     co<-as.matrix(co)
     }

  print(co)  # unfinished
  print(d)

}
