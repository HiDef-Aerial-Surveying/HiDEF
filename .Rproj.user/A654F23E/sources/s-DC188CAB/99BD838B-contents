#' Custom function to load a list of libraries
#'
#'
#' Reads a list of libraries and installs them if they aren't already
#' @param LIBS a character vector. A list of libraries in the form c("x", "y")
#' @return installs and loads libraries
#' @export
#' @examples
#' LIBS <- c("sp", "sf", "rgeos")
#' Load_Libs(LIBS)


Load_Libs <- function(LIBS){
  for(l in LIBS){
    if(require(l,character.only=TRUE,quietly=TRUE)){
      print(paste(l, "Loaded"))
    }else{
      if(l == "INLA"){
        install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
      }
      install.packages(l)
      require(l,character.only=TRUE,quietly=TRUE)
    }
  }
}
