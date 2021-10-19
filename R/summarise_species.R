#' Summarise species data for internal reporting
#'
#'
#' Will take an observation spreadsheet and summarise by a specific species
#' @param path a character string. The path of the observation spreadsheet
#' @param Species a character string. Name of the species
#' @return A matrix. The displacement matrix for the species and season of interest
#'
#' @export
#' @import tidyverse

#' @examples
#' test <- summarise_species(path = "R://Projects//HP00000 - Projects//HP00113 - SimplyBlue Erebus//700 - Analysis Reports//Observation spreadsheets//FOR_BEHAVIOUR_Zone86_M10_19_to_M09_21_ALL_OBS.xlsx",species = "Guillemot")

#define function
summarise_species <- function(path, species){

  #load in file
  dat <- readxl::read_xlsx(path)
  dat <- dat %>% filter(Species == species)

  #format date
  dat <- dat %>% mutate(`Survey Date` = as.Date(`Survey Date`, format = "%d/%m/%y"))

  #filter behaviours of interest
  dat <- dat %>% filter(Behaviour %in% c("Taking off", "Sitting", "Diving", "Flying S", "Flying NW", "Flying W", "Flying NE", "Flying SE", "Flying E", "Flying SW", "Flying N", "Flying (direction unknown)"))

  #create new behaviour class based on grouping flying
  #this could be better
  dat$new_beh <- NA
  dat$new_beh[dat$Behaviour == "Taking off"] <- "Taking off"
  dat$new_beh[dat$Behaviour == "Sitting"] <- "Sitting"
  dat$new_beh[dat$Behaviour == "Diving"] <- "Diving"
  dat$new_beh[dat$Behaviour %in% c("Flying S", "Flying NW", "Flying W", "Flying NE", "Flying SE", "Flying E", "Flying SW", "Flying N", "Flying (direction unknown)")] <- "Flying"
  dat$new_beh <- as.factor(dat$new_beh)

  #use tally to count rows grouped by species, survey date and behaviour
  output <- dat %>% group_by(Species, `Survey Date`, new_beh) %>% tally
  names(output) <- c("Species", "SurveyDate", "Behaviour", "Count")
  #pivot table so that behaviours are columns not rows
  output <- output %>% pivot_wider(names_from = Behaviour, values_from = Count, values_fill = 0)

  #create total and % flying columns
  output <- output %>% mutate("Total" = sum(Flying, Sitting, `Taking off`, Diving),
                              "% Flying" = Flying/Total*100)
  #sort columns
  output <- output %>% select("Species", "SurveyDate",  "Diving", "Flying", "Sitting", "Taking off", "% Flying", "Total")
  #ungroup
  output <- output %>% ungroup

  #return output
  return(output)
}

