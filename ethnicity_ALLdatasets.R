### Ethnicity information NBR 
if(NBR == TRUE){
  nbr.ethnicity.raw <- read_csv("../data_raw/nbr/coping_participants_export_20200730_hashed.csv")
  dim(nbr.ethnicity.raw)
  colnames(nbr.ethnicity.raw)
}


if(NBR == TRUE) {
  exclude_cols_ethnicity_nbr <- c("ID")
  
  nbr.ethnicity.raw.id <- nbr.ethnicity.raw %>%
    drop_na(`NBR ID`) %>% # Drop NAs
    distinct(`NBR ID`, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #mutate(externalDataReference = as.numeric(externalDataReference)) %>%
    #add_column(cohort_name = "nbr", .after = "`NBR ID`") %>% #create new column 
    select(
      ID = `NBR ID`, # ID
      #cohort_name, # Sample
      #endDate,
      ethnicity.unc = ethnicity # Ethnicity
    ) %>%
    add_numeric(., exclude = exclude_cols_ethnicity_nbr) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(nbr.ethnicity.raw.id)
  # Inspect colnames
  colnames(nbr.ethnicity.raw.id)
  #Differences
  dim(nbr.ethnicity.raw)[1]-dim(nbr.ethnicity.raw.id)[1]
}


if(NBR == TRUE) {
  #Recode details ethnicity info
  nbr.ethnicity.raw.id$ethnicity_full <- recode_factor(nbr.ethnicity.raw.id$ethnicity.unc,
                                                       "A" = "White - British",
                                                       "B" = "White - Irish",
                                                       "C" = "White - Other",
                                                       "D" = "Mixed - White & Black Caribbean",
                                                       "E" = "Mixed - White & Black African",
                                                       "F" = "Mixed - White & Asian",
                                                       "G" = "Mixed - Other",
                                                       "H" = "Asian - Indian",
                                                       "J" = "Asian - Pakistani",
                                                       "K" = "Asian - Bangladeshi",
                                                       "L" = "Asian - Other",
                                                       "M" = "Black - Caribbean",
                                                       "N" = "Black - African",
                                                       "P" = "Black - Other",
                                                       "R" = "Chinese - Chinese",
                                                       "S" = "Chinese - Other",
                                                       "W" = "White - not disclosed", ## Asking NBR what this level means
                                                       "Z" = NULL,
                                                       "99" = NULL)
  
  #Match GLAD/EDGI/RAMP categories
  nbr.ethnicity.raw.id$ethnicity_unc <- recode_factor(nbr.ethnicity.raw.id$ethnicity.unc,
                                                      "A" = "White, white European or Caucasian",
                                                      "B" = "White, white European or Caucasian",
                                                      "C" = "White, white European or Caucasian",
                                                      "D" = "Mixed or multiple ethnic origins",
                                                      "E" = "Mixed or multiple ethnic origins",
                                                      "F" = "Mixed or multiple ethnic origins",
                                                      "G" = "Mixed or multiple ethnic origins",
                                                      "H" = "Asian or Asian British (including Chinese)",
                                                      "J" = "Asian or Asian British (including Chinese)",
                                                      "K" = "Asian or Asian British (including Chinese)",
                                                      "L" = "Asian or Asian British (including Chinese)",
                                                      "M" = "Black or Black British",
                                                      "N" = "Black or Black British",
                                                      "P" = "Black or Black British",
                                                      "R" = "Asian or Asian British (including Chinese)",
                                                      "S" = "Asian or Asian British (including Chinese)",
                                                      "W" = "White, white European or Caucasian",
                                                      "Z" = NULL,
                                                      "99" = NULL)
}


###Combine ethnicity info with nbr demographics 
if(NBR == TRUE){
  nbr.dem.raw.id.complete <- left_join(nbr.dem.raw.id, nbr.ethnicity.raw.id, by = c("ID"))
  # Inspect dimensions
  dim(nbr.dem.raw.id.complete)
  # Inspect colnames
  colnames(nbr.dem.raw.id.complete)
  #Differences
  dim(nbr.dem.raw.id)[1]-dim(nbr.dem.raw.id.complete)[1]
}


###Ethnicity recoding - all datasets
# Ethnicity recoding EDGI
if(EDGI == TRUE) {
  edgi.dem3.raw.id <- edgi.dem3.raw.id %>%
    mutate(
      ethnicity =
        recode_factor(ethnicity_unc,
                      "White" = "White",
                      "Mixed" = "Mixed or multiple ethnic origins",
                      "Asian or Asian British" = "Asian or Asian British",
                      "Black or Black British" = "Black or Black British",
                      "Arab" = "Arab",
                      "Other ethnic group:" = "Other ethnic group"
        )
    )
}




# Ethnicity recoding GLAD
if(GLAD == TRUE) {
  glad.dem3.raw.id <- glad.dem3.raw.id %>%
    mutate(
      ethnicity =
        recode_factor(ethnicity_unc,
                      "White" = "White",
                      "Mixed" = "Mixed or multiple ethnic origins",
                      "Asian or Asian British" = "Asian or Asian British",
                      "Black or Black British" = "Black or Black British",
                      "Arab" = "Arab",
                      "Other ethnic group:" = "Other ethnic group"
        )
    )
}


# Ethnicity recoding RAMP
if(RAMP == TRUE) {
  ramp.dem.raw.id <- ramp.dem.raw.id %>%
    mutate(
      ethnicity =
        recode_factor(ethnicity_unc,
                      "White" = "White",
                      "Mixed" = "Mixed or multiple ethnic origins",
                      "Asian or Asian British" = "Asian or Asian British",
                      "Black or Black British" = "Black or Black British",
                      "Arab" = "Arab",
                      "Other ethnic group:" = "Other ethnic group"
        )
    )
}


# Ethnicity recoding NBR
if(NBR == TRUE) {
  nbr.dem.raw.id.complete <- nbr.dem.raw.id.complete %>%
    mutate(
      ethnicity =
        recode_factor(ethnicity_unc,
                      "White, white European or Caucasian" = "White",
                      "Mixed or multiple ethnic origins" = "Mixed or multiple ethnic origins",
                      "Asian or Asian British (including Chinese)" = "Asian or Asian British",
                      "Black or Black British" = "Black or Black British",
                      "Arab" = "Arab",
                      "Other ethnic group (please specify)" = "Other ethnic group"
        )
    ) %>%
    mutate(
      ethnicity_numeric =
        recode(ethnicity.unc_numeric,
               "White" = 1,
               "Mixed or multiple ethnic origins" = 2,
               "Asian or Asian British" = 3,
               "Black or Black British" = 4,
               "Arab" = 5,
               "Other ethnic group" = 6
        )
    )
}



