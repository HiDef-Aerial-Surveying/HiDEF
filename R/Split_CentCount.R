#' CentCount splitter
#'
#'
#' Splits and summarises cent count for apportioned and unapportioned species
#' @param CentCount the CentCount as an sf object
#' @param Behaviour One of "Sitting", "Flying" or "All". The behaviour associated
#' with the CentCount to be summarized.
#' @return A list of size = 2. Unapportioned species counts and apportioned counts
#' @importFrom reshape2 melt
#' @import sf
#' @import dplyr
#' @export
#' @examples
#' All <- sf::st_read("D:/TEMP/Zone85_M01_S02_D01_21_Output.shp")
#' CentOut_All <- Split_CentCount(CentCount=All,Behaviour="All")
#' Flying <- sf::st_read("D:/TEMP/Zone85_M01_S02_D01_21_Flying.shp")
#' CentOut_Flying <- Split_CentCount(CentCount=Flying,Behaviour="Flying")
#' Sitting <- sf::st_read("D:/TEMP/Zone85_M01_S02_D01_21_Sitting.shp")
#' CentOut_Sitting <- Split_CentCount(CentCount=Sitting,Behaviour="Sitting")


Split_CentCount <-  function(CentCount,Behaviour=c("Sitting","Flying","All")){
  ## This function takes a single argument
  ## and returns an apportioned and unapportioned
  ## melted data frame, summarised by species
  ## CentCount = the CentCount shapefile to be split and summarised as an sf
  ## Behaviour = the behaviour of the CentCount birds (flying/sitting or all combined)
  if(class(CentCount)[1] != 'sf'){
    stop("Error, CentCount is not an sf object, please open with st_read()")
  }
  if(!(Behaviour %in% c("Sitting","Flying","All"))){
    stop("Error, Behaviour must be one of 'Sitting','Flying', or 'All'")
  }

  DF <- data.frame(CentCount)
  # Indices of columns with NOID attached
  NoID.names.indices <- grep(colnames(DF),pattern='*_NOID_')
  # Indices of the species groups columns
  SG.names.indices <- grep(colnames(DF),pattern="SG_*")
  # Indices of the apportioned species
  APP.names.indices <- grep(colnames(DF),pattern="*_APP")
  # Create data frame with unapportioned data and then melt it
  ## Different columns for sitting and flying
  if(Behaviour == "All"){
    shape_b <- DF[,c(-NoID.names.indices,-SG.names.indices,-APP.names.indices)] %>%
      dplyr::select(-A_Birds,-A_NonAvianA,-seg_len,-seg_area,-transect,-reel,-geometry)
  }else{
    shape_b <- DF[,c(-NoID.names.indices,-SG.names.indices,-APP.names.indices)] %>%
      dplyr::select(-A_Birds,-seg_len,-seg_area,-transect,-reel,-geometry)
  }
  Melted <- reshape2::melt(shape_b)
  Melted$variable <- substring(as.character(Melted$variable),3,4)

  ## Summarised for unapportioned species
  Unapportioned <- Melted %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(sum(value))

  # Create data frame with apportioned data and then melt it
  shape_c <- DF[,c(APP.names.indices)]
  App_Melted <- reshape2::melt(shape_c)
  App_Melted$variable <- substring(as.character(App_Melted$variable),3,4)

  ## Summarised for apportioned species
  Apportioned <- App_Melted %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(sum(value))

  names(Unapportioned) <- c("BTO_CODE","Unapportioned")
  names(Apportioned) <- c("BTO_CODE","Apportioned")

  return(list(Unapportioned = Unapportioned, Apportioned = Apportioned))

}
