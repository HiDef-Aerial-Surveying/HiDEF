#' Custom function to read and fortify a polygon
#'
#'
#' Reads a polygon shapefile and fortifies it for ggplot
#' @param filepath a character string. Filepath to the shapefile
#' @param epsg an integer. The EPSG of the output
#' @param return.read a boolean. Should the shapefile also be returned as read
#' @return Either a list (if return.read=T) with the shapefile and the fortified data frame.
#' or just the fortified data frame.
#' @export
#' @import sf
#' @import sp
#' @import ggplot2
#' @examples
#' X_UTM30 <- read.fortify("Temp/Shapefile.shp",epsg=32630,return.read=T)
#' X_WGS84 <- read.fortify("Temp/Shapefile.shp",epsg=4326,return.read=F)

read.fortify <- function(filepath,epsg=4326,return.read=F){
  ### If return.read = True, then return the transformed shape
  EPSG <- sf::st_crs(epsg)$proj4string
  Shape <- raster::shapefile(filepath)
  ShapeT <- spTransform(Shape,EPSG)
  ShapeF <- fortify(ShapeT)
  if(return.read==T){
    return(list(ShapeT=ShapeT,ShapeF=ShapeF))
  }else{
    return(ShapeF)
  }
}
