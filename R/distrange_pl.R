#' min and maximum distance of a point with a 2D line-segment
#'
#' @param co a numeric matrix with two columns and two rows, defining a line
#' segment; the x-coordinates are given in the first and the y-coordinate
#' in the second column
#' @param pt a 2-element vector with the x and y coordinate of the point
#' @return the distance between the point and the two endpoints of the
#' line segment
#' @examples
#' co <- cbind( x=c(8,14), y=c(15,11) )
#' pt <- c(7,15)
#' distrange_pl(co,pt)
#' @export

distrange_pl <- function(co,pt) {
  d1 <- sqrt( (co[1,]-pt)%*%(co[1,]-pt) )
  d2 <- sqrt( (co[2,]-pt)%*%(co[2,]-pt) )
  c(d1,d2)
}
