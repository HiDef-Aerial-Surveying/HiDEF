#' Creates a Batch CSV for us in the Data Aggregation Tool (version 1+)
#'
#'
#' Will take a project folder, and then create a batch spreadsheet for use in data aggregation
#' @param path a character string. The path of the project
#' @param Zone a character string. The Zone number (in quotation marks)
#' @param shape a character string. The shapefile name to use in the batch spreadsheet
#' @param outdir a character string. The path for the output
#' @return A data frame. The data frame with the information for the batch spreadsheet
#'
#' @export
#' @import foreach

#' @examples
#' create_batch_spreadsheet(path = "R:/Projects/HP00000 - Projects/HP00104 - Seagreen Phase 2 and 3/",Zone = "85", shape = "Seagreen_Phase_2_and_3_16_km_buffer_WGS84.shp",outdir = "C:/Temp/")


create_batch_spreadsheet <- function(path, Zone, shape, outdir){
  ## Looks for the 500 Data Processing folder in the path...
  if(!dir.exists(paste0(path,'/500 - Data Processing'))){stop("500 data processing cannot be found")}
  ## Lists all folders that follow a xxxx - Month structure
  SurvList <- list.files(path=paste0(path,"500 - Data Processing/"),
                         pattern="\\d{4} - Month")

  ## Loops through the list of files and creates the character strings to populate the spreadsheet
  outdf <- foreach(i=1:length(SurvList),.combine="rbind")%do%{
    x <- SurvList[i]
    yr <- substr(x,3,4)
    mn <- substr(x,14,15)
    sv <- substr(x,26,27)
    projnm <- paste0("Zone",Zone,"_M",mn,"_S",sv,"_D01_",yr)

    dfo <- data.frame("Shapefile location" = paste0(path,"/500 - Data Processing/",
                                                    SurvList[i],"/Density Estimates/Shapefile"),
                      "Shapefile Name" = shape,
                      "Project Name" = projnm
    )
  }
  names(outdf) <- c("Shapefile location","Shapefile Name","Project Name")
  ## Creates the output name and writes out CSV

  outcsv <- paste0(outdir,"/Zone",Zone,"_DAT_batch.csv")

  write.csv(outdf,outcsv,row.names=F)

}


