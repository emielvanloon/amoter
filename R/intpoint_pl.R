#' intersection point with a 2D line, given a reference point and distance
#'
#' @param co a numeric 2-column matrix with the two coordinates defining the
#' line (x-coordinate in the first column and the y-coordinate in the second)
#' @param pt a 2-element vector with the x and y coordinate of the point
#' @param dist the length between the point and interesection with the line
#'
#' @return A 2-element vector with the x and y coordinate of the intersection point
#' @examples
#' co <- cbind( x=c(8,14), y=c(15,11) )
#' pt <- c(7,15)
#' ip1 <- intpoint_pl(co,pt,dist=2)
#' ip2 <- intpoint_pl(co,pt,dist=4)
#' plot(co[,1], co[,2], type='l', xlim=c(5,15), ylim=c(10,17))
#' points(pt[1], pt[2], col='red')
#' points(ip1[1],ip1[2], col='blue', pty=3)
#' points(ip2[1],ip2[2], col='green', pty=3)
#'
#' # check distance from pt to ip1
#' sqrt((ip1-pt)%*%(ip1-pt))
#``
#' # check distance from pt to ip2
#' sqrt((ip2-pt)%*%(ip2-pt))
#' @export

intpoint_pl <- function(co,pt,dist=1) {

  # k is the solution to equation:
  # ( x - (a+b*k) )^2 + (y - (c+d*k) )^2 = dist, for k
  # with:
  x <- pt[1]
  y <- pt[2]
  a <- co[1,1]
  b <- -(co[2,1] - co[1,1])
  c <- co[1,2]
  d <- -(co[2,2] - co[1,2])
  s <- dist^2

  k_part <- (2*a*b - 2*b*x + 2*c*d - 2*d*y)^2 -
               4*(b^2 + d^2)*(a^2 - 2*a*x + c^2 - 2*c*y + x^2 + y^2 - s)

  if(k_part<0){
    warning('The Euclidean distance between the point and line is
            larger than "dist": no output returned')
    out <- NULL
  }else{
    k = (-0.5*sqrt( k_part ) - a*b + b*x - c*d + d*y) / (b^2 + d^2)

    # the coordinates of the intersection point
    #   xi = x1 + dx*k
    #   yi = y1 + dy*k
    out <- c( a + b*k, c + d*k)
  }
  return(out)
}

