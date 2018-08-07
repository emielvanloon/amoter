#' elevation along a 2D line
#'
#' @param co a numeric 2-column matrix with the x-coordinate in the first
#' column and the y-coordinate in the second
#' @param elev a raster object which contains the elevation values for a domain
#' which covers the x- and y-coordinates
#' @param step the length between the steps on the line
#' @param endpt a binary value to indicate whether the endpoints of each line
#' segment should be kept in the resampled data (if set to TRUE, the endpoints
#' are kept in the output)
#' @return a 4-column matrix with the first two columns
#' identical to co, and the z in the third column, the fourth column contains
#' integers which refer to the line segments in co. The end-point of each line
#' segment is also the starting point of the subsequent line segment, and
#' and a choice has been made to number these points by the id of the
#' new segment.
#' @examples
#' require(raster)
#' elev <- raster(ncol=30, nrow=30,ext=extent(0,30,0,30),crs=NA)
#' values(elev) <- runif(ncell(elev))*10
#' p1 <- cbind( x=c(1,8,14,18,23), y=c(3,5,11,4,17))
#' p2 <- cbind( x=c(1,3,4,15,21), y=c(3,12,21,24,16))
#
#  # elevation added, no resampling between observation points
#' p1e <- eal(p1,elev)
#' p2e <- eal(p2,elev)
#'
#' # elevation added with resampling between observation points
#' p1er <- eal(p1,elev,step=1)
#' p2er <- eal(p2,elev,step=1)
#' @export

eal <- function(co,elev,step=NULL,endpt=TRUE) {

  if(is.data.frame(co)){
    co<-as.matrix(co)
  }

  if(is.null(step)){
    seg <- c(1:nrow(co))
    return( cbind(co, z=raster::extract(elev,co), seg=seg) )
  }else{
    coi <- ed_resamp(co, step=step,endpt=endpt)
    zi <- raster::extract(elev,coi[,c(1,2)])
    coi <- cbind(x=coi[,1], y=coi[,2], z=zi, seg=coi[,3])
    return(coi)
  }

}
