#' Create displacement matrices
#'
#'
#' Takes a value and then generates a decay-matrix for use in displacement assessments
#' @param Species a character string. Name of the species
#' @param Season a character string. The season the matrix applies to
#' @param SMP a numeric value. The seasonal mean peak or population estimate to generate the matrix from
#' @param writexlsx a boolean. If TRUE, an xlsx file will be created in the outdir
#' @param outdir a character string. The output directory for the xlsx file
#' @return A matrix. The displacement matrix for the species and season of interest
#'
#' @export
#' @import openxlsx

#' @examples
#' Kittwake_Breeding <- Disp.Matrix("Kittiwake","Breeding",2500,writexlsx=TRUE,outdir="C:/Temp/")


Disp.Matrix <- function(Species,Season,SMP,writexlsx=TRUE,outdir=NULL){

  colvals <- c(0,0.01,0.02,0.03,0.04,0.05,0.1,0.15,0.20,0.3,0.5,0.8,1)
  rowvals <- seq(0,1,by=.1)

  dismat <- matrix(ncol=13,nrow=11)

  for(i in 1:length(colvals)){
    for(j in 1:length(rowvals)){
      dismat[j,i] <- ceiling(SMP * colvals[i] * rowvals[j])
    }
  }
  dismat <- data.frame(dismat)
  names(dismat) <- paste0(colvals*100,"%")
  rownames(dismat) <- paste0(rowvals*100,"%")

  ### Write out xlsx file if true
  if(writexlsx){
    if(is.null(outdir)){
      stop("output directory is NULL, please define outdir")
    }else{
      filen <- paste0(outdir,"/",Species,"_",Season,".xlsx")
      openxlsx::write.xlsx(dismat,filen,row.names=T)
      }
  }
  return(dismat)
}



