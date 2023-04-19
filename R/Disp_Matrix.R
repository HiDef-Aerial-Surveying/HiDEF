#' Create displacement matrices
#'
#'
#' Takes a value and then generates a decay-matrix for use in displacement assessments
#' @param Species a character string. Name of the species
#' @param Season a character string. The season the matrix applies to
#' @param MSP a numeric value. The mean seasonal peak or population estimate to generate the matrix from
#' @param MSPlowerCI a numeric value. The mean seasonal peak lower CI value which will be copied into output as (lowerCI - upperCI)
#' @param MSPupperCI a numeric value. The mean seasonal peak upper CI value which will be copied into output as (lowerCI - upperCI)
#' @param writeout If NA (default) then no output is written to outdir, if "excel" the .xlsx file output, if "word" then .docx file output
#' @param outdir a character string. The output directory for the xlsx file
#' @return A matrix. The displacement matrix for the species and season of interest
#'
#' @export
#' @import openxlsx
#' @import tibble
#' @import flextable
#' @import officer

#' @examples
#' Kittiwake_Breeding <- Disp.Matrix("Kittiwake","Breeding",MSP=2500,writeout=NA,outdir="C:/Temp/")
#'
#' #with lower and upper CI
#' Kittiwake_Breeding <- Disp.Matrix("Kittiwake","Breeding",MSP=2500,MSPlowerCI=1500,MSPupperCI=2750,writeout=NA,outdir="C:/Temp/")
#'
#' #with lower and upper CI and excel print out
#' Kittiwake_Breeding <- Disp.Matrix("Kittiwake","Breeding",MSP=2500,MSPlowerCI=1500,MSPupperCI=2750,writeout="excel",outdir="C:/Temp/")
#'
#' #with lower and upper CI and word print out
#' Kittiwake_Breeding <- Disp.Matrix("Kittiwake","Breeding",MSP=2500,MSPlowerCI=1500,MSPupperCI=2750,writeout="word",outdir="C:/Temp/")


Disp.Matrix <- function(Species,Season,MSP,MSPlowerCI=NA,MSPupperCI=NA,writeout=NA,outdir=NULL){

  colvals <- c(0,0.01,0.02,0.03,0.04,0.05,0.1,0.15,0.20,0.3,0.5,0.8,1)
  rowvals <- seq(0,1,by=.1)

  dismat <- matrix(ncol=13,nrow=11)

  for(i in 1:length(colvals)){
    for(j in 1:length(rowvals)){
      dismat[j,i] <- format(ceiling(MSP * colvals[i] * rowvals[j]), big.mark=",")
    }
  }
  dismat <- data.frame(dismat)
  names(dismat) <- paste0(colvals*100,"%")
  rownames(dismat) <- paste0(rowvals*100,"%")

  #repeat for lower CI
  if(!is.na(MSPlowerCI)){
    dismat2 <- matrix(ncol=13,nrow=11)

    for(i in 1:length(colvals)){
      for(j in 1:length(rowvals)){
        dismat2[j,i] <- format(ceiling(MSPlowerCI * colvals[i] * rowvals[j]), big.mark=",")
      }
    }
    dismat2 <- data.frame(dismat2)
    names(dismat2) <- paste0(colvals*100,"% - 2")
    rownames(dismat2) <- paste0(rowvals*100,"%")
  }

  #repeat for upper CI
  if(!is.na(MSPupperCI)){
    dismat3 <- matrix(ncol=13,nrow=11)

    for(i in 1:length(colvals)){
      for(j in 1:length(rowvals)){
        dismat3[j,i] <- format(ceiling(MSPupperCI * colvals[i] * rowvals[j]), big.mark=",")
      }
    }
    dismat3 <- data.frame(dismat3)
    names(dismat3) <- paste0(colvals*100,"% - 3")
    rownames(dismat3) <- paste0(rowvals*100,"%")
  }

  if(!is.na(MSPlowerCI)&!is.na(MSPupperCI)){
    #combine to same dataframe for easy concatenate
    out <- cbind(dismat, dismat2, dismat3)
    #loop through columns and combine the estimates
    for (c in names(dismat)) {
      out[,c] <- paste0(out[,c], "\n(", out[, paste0(c, " - 2")], ", ", out[, paste0(c, " - 3")], ")")
    }
    out <- out[,c(1:13)]
    dismat <- out
  }

  return(dismat)

  ### Write out xlsx file if writeout == "excel"
  if(writeout == "excel"){
    if(is.null(outdir)){
      stop("output directory is NULL, please define outdir")
    }else{
      filen <- paste0(outdir,"/",Species,"_",Season,".xlsx")
      openxlsx::write.xlsx(dismat,filen,rowNames=T)
    }
  }

  ### Write out word file if writeout == "word"
  if(writeout == "word"){
    if(is.null(outdir)){
      stop("output directory is NULL, please define outdir")
    }else{
      filen <- paste0(outdir,"/",Species,"_",Season,".docx")
      flextable(Kittiwake_Breeding %>% rownames_to_column("mort")) %>%
        align(align = "center") %>%
        style(pr_t= fp_text(font.family='Gill Sans MT'), part = "all") %>%
        fontsize(size = 8, part = "all") %>%
        save_as_docx(path = filen,
                     pr_section = prop_section(
                       page_size = page_size(orient = "landscape")),
                     align = "center")
    }
  }
}

