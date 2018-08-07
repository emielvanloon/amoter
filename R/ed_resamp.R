#' find x,y coordinates at equal distances along a piecewise linear line
#'
#' @param co a numeric 2-column matrix with the x-coordinates in the first
#' column and the y-coordinates in the second
#' @param step the length between the steps on the line
#' @param endpt a boolean value which indicates whether the nodes of the
#' piecewise linear line have to be included
#'
#' @return A 3-column matrix with in the first two columns x and y coordinates
#' of the equidistant points and in the third columns an integer which specifies
#' in which line-segment of co each point falls. The end-point of each line
#' segment is also the starting point of the subsequent line segment, and
#' and a choice has been made to number these points by the id of the
#' new segment. To keep this pattern consistent, the very last point of the
#' trajectory has a new id
#' @import stats
#' @examples
#' co <- cbind(x=c(1,3,7,8,5),y=c(12,7,6,10,14))
#'
#' coi1 <- ed_resamp(co, step=1, endpt=FALSE)
#' coi2 <- ed_resamp(co, step=2, endpt=FALSE)
#' plot(co[,1],co[,2],type='l',col='red')
#' points(coi1[,1],coi1[,2],pch=3)
#' points(coi2[,1],coi2[,2],pch=1,col='blue')
#'
#' coi1 <- ed_resamp(co, step=1, endpt=TRUE)
#' coi2 <- ed_resamp(co, step=2, endpt=TRUE)
#' plot(co[,1],co[,2],type='l',col='red')
#' points(coi1[,1],coi1[,2],pch=3)
#' points(coi2[,1],coi2[,2],pch=1,col='blue')
#' @export

ed_resamp <- function(co, step=1, endpt=TRUE) {

  # for storing outputs
  coit <- matrix(nrow=0,ncol=3)
  colnames(coit) <- c('xi','yi','seg')
  xpart <- FALSE

  for(i in 1:(nrow(co)-1)){

    xpart <- FALSE
    # determine x-coordinate of last point
    if(i==1){
      pt_start <- co[1,]
      xi_start <- co[1,1]
    }else{
      if( coi$x[length(xi)] == co[i,1] | endpt){
        # last point falls on next node OR
        # explicit setting that each node should be part of interpolation
        pt_start <- co[i,]
        xi_start <- co[i,1]
      }else{
        xpart <- TRUE
        pt_start <- c(coi$x[length(xi)], coi$y[length(xi)])
        xi_start <- pt_start[1]
      }
    }

    # check if furthest point in next segment is more than 1 step
    # away from lastpoint, if not: move on to next segment
    # this part is only relevant if endpt == FALSE
    nextsegment <- co[c(i,i+1),]
    rngmax <- max( distrange_pl(co=nextsegment, pt=pt_start) )
    if((step>rngmax) & !endpt){next()}

    # if the remaining distance on the last line segment is smaller than
    # step, new starting x-value on the new segment is determined
    if(xpart){
      # print(nextsegment)
      # print(xi_start)
      xi_start <- intpoint_pl(co=nextsegment, pt=pt_start, dist=step)[1]
      xpart <- FALSE
    }

    # the step length along the x-asis is calculated,
    # when the step length on the path is set to 'step'
    totstep <- diff( dal(nextsegment) )
    xstep <- diff(nextsegment[,1])
    pr_xstep <- step*xstep/totstep

    # the points on next segment are determined by linear interpolation
    xi_end <- nextsegment[2,1]
    xi <- seq(from=xi_start, to=xi_end, by=pr_xstep)

    coi <- approx(nextsegment, xout=xi)

    # add result to output
    coit <- rbind(coit, cbind(coi$x,coi$y,rep(i,length(xi))))
  }

  # adding the very last coordinate of track at the end
  coit <- rbind(coit, c(nextsegment[2,],i+1))

  return(coit)
}

