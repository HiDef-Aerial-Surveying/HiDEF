#' Summarise observations versus Cent Count
#'
#'
#' This will take the observation spreadsheet, clip it to a boundary,
#' summarize the information by species and then compare. Useful for quick
#' Observation spreadsheet checks
#'
#' @param Observations A data frame or tibble. The observation spreadsheet to be
#' opened and compared
#' @param Obs_Shape Observations opened as an 'sf' object
#' @param CentCount the CentCount as an sf object
#' @param SPP_lookup The species lookup spreadsheet opened as a data frame. This
#' is normally found at "R:/Overhead/HA17000 - R Code/5 - Bootstrap_Shiny/Hidef_bootstrap/data/Species_Lookup.xlsx"
#' @param Behaviour One of "Sitting", "Flying" or "All". The behaviour associated
#' with the CentCount to be summarized.
#' @param Boundary_Buffer The boundary as an "sf" object that encompasses all the
#' observations in the cent count
#'
#' @return A data frame. Summaries of the cent count and observations
#'
#' @export
#' @import tidyverse
#' @importFrom rmapshaper ms_erase

#' @examples
#' Observations <- openxlsx::read.xlsx("D:/TEMP/19_SG23_HiDef_Zone85_M12_S01_alldays_19_Observations.xlsx")
#' Obs_Shape <- sf::st_as_sf(Observations,coords=c("Latitude","Longitude"),crs=4326)
#' CentCount <- sf::st_read("D:/TEMP/Zone85_M12_S01_D01_19_Output.shp")
#' SPP_lookup <- openxlsx::read.xlsx("R:/Overhead/HA17000 - R Code/5 - Bootstrap_Shiny/Hidef_bootstrap/data/Species_Lookup.xlsx")
#' Boundary_Buffer <- sf::st_read("D:/TEMP/Development_area_4kmBuffer_WGS84.shp")
#' Output <- Obs_v_CentCount(Observations,Obs_Shape,CentCount,SPP_lookup,Behaviour="All",Boundary_Buffer)



# Summarise observations v CentCounts -------------------------------------

Obs_v_CentCount <- function(Observations,
                            Obs_Shape,
                            CentCount,
                            SPP_lookup,
                            Behaviour="All",
                            Boundary_Buffer){


  if(!("data.frame" %in% class(Observations))){
    stop("Observations must be a data frame or tibble")
  }
  ## Test if this is an observation spreadsheet
  nametest <- c("Survey","Survey Date","Camera","Reel Ref",
                "Time","Frame Ref","Species","Behaviour") %in% names(Observations)
  if(isFALSE(all(nametest))){
    stop("Are you sure this is an observation spreadsheet?")
  }

  if(class(Obs_Shape)[1] != 'sf'){
    stop("Error, Obs_Shape is not an sf object, please open with st_read()")
  }

  if(class(CentCount)[1] != 'sf'){
    stop("Error, CentCount is not an sf object, please open with st_read()")
  }

  if(!("data.frame" %in% class(SPP_lookup))){
    stop("SPP_lookup must be a data frame or tibble")
  }

  if(!(Behaviour %in% c("Sitting","Flying","All"))){
    stop("Error, Behaviour must be one of 'Sitting','Flying', or 'All'")
  }

  if(class(Boundary_Buffer)[1] != 'sf'){
    stop("Error, Boundary_Buffer is not an sf object, please open with st_read()")
  }


  ## First summarise observations by species
  Observation_Summary <- Observations %>%
    dplyr::filter(Species != "N/A") %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(Observations = n())

  ## Join the observation summary to the spp_lookup to get the species codes
  Code_Sheet <- left_join(Observation_Summary,SPP_lookup,by="Species")

  ## Using the Split_CentCount function, return a summary of species in the centcount
  CC_summaries <- Split_CentCount(CentCount = CentCount,Behaviour=Behaviour)

  ## Left join the centcount summaries to the original observation summary
  UnapportionJoin <- left_join(Code_Sheet,CC_summaries$Unapportioned)
  ApportionJoin <- left_join(UnapportionJoin,CC_summaries$Apportioned)


  ### Clip the observations to the boundary and see how many fall out
  Clipped_Obs <- rmapshaper::ms_erase(Obs_Shape,erase = Boundary_Buffer)

  Clipped_Observation_Summary <- data.frame(Clipped_Obs) %>%
    dplyr::filter(Species != "N/A") %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(Outside_Boundary = n())

  ## Join to the table and reorder the columns
  Final_Join <- left_join(ApportionJoin,Clipped_Observation_Summary,by="Species")
  Final_Join <- Final_Join %>% dplyr::select(Species,
                                             BTO_CODE,
                                             Observations,
                                             Outside_Boundary,
                                             Unapportioned,
                                             Apportioned)
  ## NAs in the Outside_Boundary column are 0s
  Final_Join$Outside_Boundary[is.na(Final_Join$Outside_Boundary)] <- 0
  Final_Join$Unapportioned[is.na(Final_Join$Unapportioned)] <- 0

  ## Do a check on how things match up. For example:
  ## Observations (# birds in observation spreadsheet) -
  ## birds outside the boundary should approximately equal
  ## the number of birds in the unapportioned column
  Final_Join$App_v_Obs <- with(Final_Join,
                               dplyr::case_when(
                                 Observations - Outside_Boundary == Unapportioned ~ "EXACT",
                                 Observations - Outside_Boundary > (Unapportioned - 2) &
                                   Observations - Outside_Boundary < (Unapportioned + 2) ~ "CLOSE",
                                 TRUE ~ "FALSE"
                               )
  )


  return(Final_Join)

}
