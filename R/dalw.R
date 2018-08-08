#' wrapper function for \code{dal}, using output from \code{eal}
#'
#' @param coi the output from the function eal: a numeric matrix with x,y,z
#' coordinates in the first three columns and a fourth column labelled 'seg'
#' with the ids of the line segments to which each point belongs
#' @param d a value 2 or 3, referring to the calculation of distance in 2D or
#'  3D respectively
#' @return A vector with distance along every line segment in \code{coi}.
#' @examples
#' require(raster)
#' elev <- raster(ncol=30, nrow=30,ext=extent(0,30,0,30),crs=NA)
#' values(elev) <- runif(ncell(elev))*10
#' p1 <- cbind( x=c(1,8,14,18,23), y=c(3,5,11,4,17))
#' p2 <- cbind( x=c(1,3,4,15,21), y=c(3,12,21,24,16))
#
#  # add elevation and subsequently calculate distance along this line
#' ( p1e <- eal(p1,elev) )
#' ( p1er <- eal(p1,elev,step=1) )
#'
#' dalw(p1er,2)  # distance along line, considering 2D
#' dalw(p1er,3)  # distance along line, considering 3D
#'
#' # note that, for calculating the distance along the line, the steplength does
#' # influence the result in 3D but does not influence the 2D result:
#' dalw(p1e,2)
#' dalw(p1e,3)
#'
#' @author Emiel van Loon, \email{e.e.vanloon@@uva.nl}
#' @seealso \code{\link{salw}}
#' @export

dalw <- function(coi,d=2) {

  dseg <- function(x,pt,d){
    dal( pt[x, 1:d] )[length(x)]
  }

  segidx <- ptseg(coi)
  mapply(dseg, x=segidx, MoreArgs=list(pt=coi,d=d) )
}
