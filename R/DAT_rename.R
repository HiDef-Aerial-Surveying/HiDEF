#' Custom function to rename the MATLAB created shapefiles
#'
#'
#' Changes some of the usual problem field names so they comply with the 10
#' character limit of filetype Shapefile (.shp).
#' @param DAT A centcount file output from the MATLAB Data Aggregation Tool. It
#' will be a shapefile (.shp)
#' @return renames field names to within 10 character limit
#' @export
#'
#' @import sf
#'
#' @examples
#' DAT <- "R:\\Projects\\HP00000 - Projects\\HP00119 - Cierco Atlantic Cluster (Llyr)\\700 - Analysis Reports\\files for EIA"
#' DAT_rename(DAT)


DAT_rename <- function(DAT) {

  #take a copy of geometry
  geom <- st_geometry(DAT)
  #remove the geometry to create a data.frame
  temp_DAT <- st_drop_geometry(DAT)
  column.no <- grep( "S_|SG_|A_" , names(temp_DAT))
  column.names <- colnames(temp_DAT[column.no])
  #find the double NOID columns
  doubles <- grep("_.1", column.names)
  #if there are doubles run for loop
  if(length(doubles) >= 1){
    #go through each double
    for(d in 1:length(doubles)){
      #break up name
      name.parts <- strsplit(column.names[doubles[d]], "_")
      #change NOID_.1 to NO2
      new.name <- paste(name.parts[[1]][1], name.parts[[1]][2], "NO2", sep = "_")
      #replace name in the list
      column.names[doubles[d]] <- paste(new.name)
    }

  }

  #find the NOID columns which are causing saving problems
  NOID <- grep("NOID", column.names)
  #if there are NOIDs run for loop
  if(length(NOID) >= 1){
    #go through each double
    for(n in 1:length(NOID)){
      #break up name
      name.parts <- strsplit(column.names[NOID[n]], "_")
      #change NOID_.1 to NO2
      new.name <- paste(name.parts[[1]][1], name.parts[[1]][2], "NO1", sep = "_")
      #replace name in the list
      column.names[NOID[n]] <- new.name
    }
  }

  #fix Non AVian name
  Avian <- grep("A_NonAvian", column.names)
  column.names[Avian] <- "A_NonAvian"
  #rename the columns of dataset c
  names(temp_DAT)[column.no] <- column.names


  #put geometry back
  st_geometry(temp_DAT) <- geom
  return(temp_DAT)
}
