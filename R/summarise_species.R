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
  dat <- dat %>% mutate(`Survey Date` = as.Date(`Survey Date`, format = "%d/%m/%Y"))

  #filter behaviours of interest
  #filter by birds...
  dat <- dat %>% filter(`Broad Category` == "Bird")

  #create new behaviour class based on grouping flying
  dat <- dat %>% mutate(new_beh = case_when(Behaviour == "Taking off" ~ "Taking Off",
                                            Behaviour %in% c("Sitting","Sitting on man-made object") ~ "Sitting",
                                            Behaviour == "Diving" ~ "Diving",
                                            Behaviour %in% c("Flying S", "Flying NW", "Flying W", "Flying NE", "Flying SE", "Flying E", "Flying SW", "Flying N", "Flying (direction unknown)") ~ "Flying",
                                            TRUE ~ "Other")) %>%
    mutate(new_beh = as.factor(new_beh))

  #use tally to count rows grouped by species, survey date and behaviour
  output <- dat %>% group_by(Species, `Survey Date`, new_beh) %>% tally
  names(output) <- c("Species", "SurveyDate", "Behaviour", "Count")

  #pivot table so that behaviours are columns not rows
  output <- output %>% pivot_wider(names_from = Behaviour, values_from = Count, values_fill = 0)

  # issue when not all behaviours are observed for a species
  # still want to output column but with zeros

  # create a dummy dataframe with the four behaviour groups
  columns = c("Taking Off", "Sitting", "Diving", "Other") %>%
    map_dfr( ~tibble(!!.x := logical() ) )

  # combining this with the output file will add missing columns
  output <- output %>% bind_rows(columns)
  output[is.na(output)] <- 0

  #create total and % flying columns
  output <- output %>% mutate("Total" = sum(Flying, Sitting, `Taking Off`, Diving),
                              "Flying%" = round((Flying/Total*100), digits = 0))

  #sort columns
  output <- output %>% select("Species", "SurveyDate",  "Diving", "Flying", "Sitting", "Taking Off", "Flying%", "Total")
  #ungroup
  output <- output %>% ungroup

  #return output
  return(output)
}

