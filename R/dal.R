#' distance along a line
#'
#' @param co a numeric matrix with the coordinates of the line in columns
#' (a 2D line would result in a 2-column matrix).
#' @return A vector with distance along the line from first point in the input matrix. \code{x} and \code{y}.
#' @examples
#' co <- cbind( x=1:5, y=c(3,5,6,4,7))
#' dal(co)
#' @export

dal <- function(co) {

  if(is.data.frame(co)){
     co<-as.matrix(co)
     }

  cumsum( c(0, sqrt( rowSums( diff(co)^2 ) )) )

  # to clarify the code: a version for only two coordinates would e.g. be:
  #   cumsum( c(0, sqrt(diff(co[1,])^2 + diff(co[2,])^2)) )
}
