#' Elevation data
#'
#' Synthetic elevation data on a 30x30 grid, stored as a raster object.
#'
#' @docType data
#'
#' @usage data(dem_ex1)
#'
#' @format An object of class \code{"raster"}; see \code{\link[raster]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references ABC et al. (2013) DEMS 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
#'
#' @examples
#' data(dem_ex1)
#' \donttest{image(dem_ex1)}
"dem_ex1"
