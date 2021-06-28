#Losing someone due to COVID - GLAD

  glad.loss.baseline.raw <- readRDS(file = "../data_raw/glad/grief_coping_glad.rds")
  dim(glad.loss.baseline.raw)
  colnames(glad.loss.baseline.raw)


  exclude_cols_dem <- c("ID",
                        "cohort_name",
                        "endDate")
  
  glad.loss.baseline.raw.id <- glad.loss.baseline.raw %>% #new dataset with ID
    drop_na(externalDataReference) %>% # Drop NAs
    #distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #mutate(externalDataReference = as.numeric(externalDataReference)) %>%
    add_column(cohort_name = "coping_glad", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      grief.covid_lost_due_close,
      grief.suffered_past_month_loss
      
    ) %>%
    add_numeric(., exclude = exclude_cols_dem) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    #mutate_if(is.numeric, ~na_if(., -99)) %>%
    #mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    # mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) # Drop empty factor levels
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  
  # Inspect dimensions
  dim(glad.loss.baseline.raw.id)
  # Inspect colnames
  colnames(glad.loss.baseline.raw.id)
  #Differences
  dim(glad.loss.baseline.raw.id)[1]-dim(glad.loss.baseline.raw)[1]


  edgi.loss.baseline.raw <- readRDS(file = "../data_raw/edgi/grief_coping_edgi.rds")
  dim(edgi.loss.baseline.raw)
  colnames(edgi.loss.baseline.raw)



  exclude_cols_dem <- c("ID",
                        "cohort_name",
                        "endDate")
  
  edgi.loss.baseline.raw.id <- edgi.loss.baseline.raw %>% #new dataset with ID
    drop_na(externalDataReference) %>% # Drop NAs
    #distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #mutate(externalDataReference = as.numeric(externalDataReference)) %>%
    add_column(cohort_name = "coping_edgi", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      grief.covid_lost_due_close = grief.covid_due_lost_past,
      grief.suffered_past_month_loss = grief.significant_relationship_suffered_loss
      
    ) %>%
    add_numeric(., exclude = exclude_cols_dem) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    #mutate_if(is.numeric, ~na_if(., -99)) %>%
    #mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    # mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) # Drop empty factor levels
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  
  # Inspect dimensions
  dim(edgi.loss.baseline.raw.id)
  # Inspect colnames
  colnames(edgi.loss.baseline.raw.id)
  #Differences
  dim(edgi.loss.baseline.raw.id)[1]-dim(edgi.loss.baseline.raw)[1]


  
  nbr.loss.baseline.raw<- readRDS(file = "../data_raw/nbr/grief_coping_nbr.rds")
  dim(nbr.loss.baseline.raw)
  colnames(nbr.loss.baseline.raw)

  exclude_cols_dem <- c("ID",
                        "cohort_name",
                        "endDate")
  
  nbr.loss.baseline.raw.id <- nbr.loss.baseline.raw %>% #new dataset with ID
    drop_na(externalDataReference) %>% # Drop NAs
    #distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #mutate(externalDataReference = as.numeric(externalDataReference)) %>%
    add_column(cohort_name = "coping_nbr", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      grief.covid_lost_due_close,
      grief.suffered_past_month_loss = grief.significant_relationship_suffered_past
      
    ) %>%
    add_numeric(., exclude = exclude_cols_dem) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    #mutate_if(is.numeric, ~na_if(., -99)) %>%
    #mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    # mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) # Drop empty factor levels
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  
  # Inspect dimensions
  dim(nbr.loss.baseline.raw.id)
  # Inspect colnames
  colnames(nbr.loss.baseline.raw.id)
  #Differences
  dim(nbr.loss.baseline.raw.id)[1]-dim(nbr.loss.baseline.raw)[1]

  ramp.loss.baseline.raw<- readRDS(file = "../data_raw/ramp/grief_ramp.rds")
  dim(ramp.loss.baseline.raw)
  colnames(ramp.loss.baseline.raw)

  exclude_cols_dem <- c("ID",
                        "cohort_name",
                        "endDate")
  
  ramp.loss.baseline.raw.id <- ramp.loss.baseline.raw %>% #new dataset with ID
    drop_na(externalDataReference) %>% # Drop NAs
    #distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #mutate(externalDataReference = as.numeric(externalDataReference)) %>%
    add_column(cohort_name = "ramp", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      grief.covid_lost_due_close,
      grief.suffered_past_month_loss = grief.significant_relationship_suffered_past
      
    ) %>%
    add_numeric(., exclude = exclude_cols_dem) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    #mutate_if(is.numeric, ~na_if(., -99)) %>%
    #mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    # mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    # mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) # Drop empty factor levels
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
  # mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  
  # Inspect dimensions
  dim(ramp.loss.baseline.raw.id)
  # Inspect colnames
  colnames(ramp.loss.baseline.raw.id)
  #Differences
  dim(ramp.loss.baseline.raw.id)[1]-dim(ramp.loss.baseline.raw)[1]

  
  
ramp.loss <- ramp.loss.baseline.raw.id %>%
    select(ID, 
           cohort_name,
           grief.covid_lost_due_close,
           grief.suffered_past_month_loss)

glad.loss <-  glad.loss.baseline.raw.id %>%
  select(ID, 
         cohort_name,
         grief.covid_lost_due_close,
         grief.suffered_past_month_loss)

edgi.loss <-  edgi.loss.baseline.raw.id %>%
  select(ID, 
         cohort_name,
         grief.covid_lost_due_close,
         grief.suffered_past_month_loss)

nbr.loss <-  nbr.loss.baseline.raw.id %>%
  select(ID, 
         cohort_name,
         grief.covid_lost_due_close,
         grief.suffered_past_month_loss)