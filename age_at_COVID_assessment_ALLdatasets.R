#CREATING AGE variable (age_unc)
#GLAD
###age from birthday

if(GLAD == TRUE) {
  #add 1900 to birthyear
  glad.dem3.raw.id$birthday_year_numeric <- glad.dem3.raw.id$dem.year_numeric + 1900
  
  #create dob as date format
  glad.dem3.raw.id$Birthdate <- as.Date(paste(glad.dem3.raw.id$birthday_year_numeric,
                                              glad.dem3.raw.id$dem.month_numeric,
                                              glad.dem3.raw.id$dem.day_numeric,
                                              sep = "-"))
  
  head(glad.dem3.raw.id$Birthdate)
  
  #Need end date from when they completed COPING baseline questionnaire
  endDate_data <- glad.coping.dem.raw.id %>%
    select(ID,
           endDate
    )
  
  #Merge so that glad.coping.dem.raw.id contains Birthdate
  glad.coping.dem.raw.id <- merge(glad.dem3.raw.id,
                                  glad.coping.dem.raw.id,
                                  by ="ID"
                                  
                                  
  )
  
  
  #Calculate dem.age(remove NAs as it throws an error)
  ages <- eeptools::age_calc(na.omit(glad.coping.dem.raw.id$Birthdate),
                             enddate = as.Date(glad.coping.dem.raw.id$endDate),
                             units = "years")
  
  #Create dem.agecolumn
  glad.coping.dem.raw.id$age_unc<- NA
  
  #Populate with values (excluding NAs)
  glad.coping.dem.raw.id$age_unc[!is.na(glad.coping.dem.raw.id$Birthdate)] <- ages
  
  #Round age down
  glad.coping.dem.raw.id$age_unc <- floor(glad.coping.dem.raw.id$age_unc)
  
  #Visualise
  head(glad.coping.dem.raw.id$age_unc)
  head(glad.coping.dem.raw.id$Birthdate)
}

# Clean Age variable
#Age outlier
if(GLAD == TRUE) {
  # Define age limits
  age_upper_limit = 117 # The oldest person in the world is 117 years
  age_lower_limit = 16
  
  #Identify number of outliers
  length(
    which(
      glad.coping.dem.raw.id$age_unc > age_upper_limit |
        glad.coping.dem.raw.id$age_unc < age_lower_limit
    )
  )
  
  #Remove age outliers
  glad.coping.dem.raw.id <- glad.coping.dem.raw.id %>%
    mutate(
      age_in_COPING =
        if_else(
          age_unc > age_upper_limit |
            age_unc < age_lower_limit,
          true = NA_real_,
          false = age_unc,
          missing = NA_real_
        )
    )
  
  # Inspect variable
  glad.coping.dem.raw.id %>%
    descr(
      age_in_COPING,
      #stats = "common"
    )
}

## Categorise age into groups
#In RAMP, age is indicated categorically: 16-18, 19-25, 26-35, 36-45, 46-55, 56-65, 66-70, 71-75, 76-80, 81-85, 86-90, 91-100, 100+
#Age categories in GLAD, EDGI and NBR have been created to reflect this
if(GLAD == TRUE) {
  # Create categorical age groups per 10 years
  glad.coping.dem.raw.id <- glad.coping.dem.raw.id %>%
    mutate(
      age_category_COPING_numeric =
        case_when(
          age_in_COPING >= 16 & age_in_COPING <= 18 ~ 1,
          age_in_COPING >= 19 & age_in_COPING <= 25 ~ 2,
          age_in_COPING >= 26 & age_in_COPING <= 35 ~ 3,
          age_in_COPING >= 36 & age_in_COPING <= 45 ~ 4,
          age_in_COPING >= 46 & age_in_COPING <= 55 ~ 5,
          age_in_COPING >= 56 & age_in_COPING <= 65 ~ 6,
          age_in_COPING >= 66 & age_in_COPING <= 70 ~ 7,
          age_in_COPING >= 71 & age_in_COPING <= 75 ~ 8,
          age_in_COPING >= 76 & age_in_COPING <= 80 ~ 9,
          age_in_COPING >= 81 & age_in_COPING <= 85 ~ 10,
          age_in_COPING >= 86 & age_in_COPING <= 90 ~ 11,
          age_in_COPING >= 91 & age_in_COPING <= 100 ~ 12,
          age_in_COPING >= 101 & age_in_COPING <= 120 ~ 13 # oldest person in the world is 117 years
        )
    )
  
  glad.coping.dem.raw.id <- glad.coping.dem.raw.id %>%
    mutate(
      age_category_COPING =
        recode_factor(
          age_category_COPING_numeric,
          "1" = "16-18",
          "2" = "19-25",
          "3" = "26-35",
          "4" = "36-45",
          "5" = "46-55",
          "6" = "56-65",
          "7" = "66-70",
          "8" = "71-75",
          "9" = "76-80",
          "10" = "81-85",
          "11" = "86-90",
          "12" = "91-100",
          "13" = "100+"
        )
    )
  glad.coping.dem.raw.id %>%
    freq(age_category_COPING)
}



##EDGI
###Age at COVID baseline from DOB

if(EDGI == TRUE) {
  #add 1900 to birthyear
  edgi.dem3.raw.id$birthday_year_numeric <- edgi.dem3.raw.id$dem.year_numeric + 1900
  
  #create dob as date format
  edgi.dem3.raw.id$Birthdate <- as.Date(paste(edgi.dem3.raw.id$birthday_year_numeric,
                                              edgi.dem3.raw.id$dem.month_numeric,
                                              edgi.dem3.raw.id$dem.day_numeric,
                                              sep = "-"))
  
  head(edgi.dem3.raw.id$Birthdate)
  
  
  
  #Need end date from when they completed COPING baseline questionnaire
  endDate_data <- edgi.coping.dem.raw.id %>%
    select(ID,
           endDate
    )
  
  #Merge so that glad.coping.dem.raw.id contains Birthdate
  edgi.coping.dem.raw.id <- merge(edgi.dem3.raw.id,
                                  edgi.coping.dem.raw.id,
                                  by ="ID"
                                  
                                  
  )
  
  
  #Calculate dem.age(remove NAs as it throws an error)
  ages <- eeptools::age_calc(na.omit(edgi.coping.dem.raw.id$Birthdate),
                             enddate = as.Date(edgi.coping.dem.raw.id$endDate),
                             units = "years")
  
  #Create dem.agecolumn
  edgi.coping.dem.raw.id$age_unc<- NA
  
  #Populate with values (excluding NAs)
  edgi.coping.dem.raw.id$age_unc[!is.na(edgi.coping.dem.raw.id$Birthdate)] <- ages
  
  #Round age down
  edgi.coping.dem.raw.id$age_unc <- floor(edgi.coping.dem.raw.id$age_unc)
  
  #Visualise
  head(edgi.coping.dem.raw.id$age_unc)
  head(edgi.coping.dem.raw.id$Birthdate)
}

# Clean EDGI Age variable
#Age outlier
if(EDGI == TRUE) {
  # Define age limits
  age_upper_limit = 117 # The oldest person in the world is 117 years
  age_lower_limit = 16
  
  #Identify number of outliers
  length(
    which(
      edgi.coping.dem.raw.id$age_unc > age_upper_limit |
        edgi.coping.dem.raw.id$age_unc < age_lower_limit
    )
  )
  
  #Remove age outliers
  edgi.coping.dem.raw.id <- edgi.coping.dem.raw.id %>%
    mutate(
      age_in_COPING =
        if_else(
          age_unc > age_upper_limit |
            age_unc < age_lower_limit,
          true = NA_real_,
          false = age_unc,
          missing = NA_real_
        )
    )
  
  # Inspect variable
  edgi.coping.dem.raw.id %>%
    descr(
      age_in_COPING,
      #stats = "common"
    )
}

## Categorise age into groups
#In RAMP, age is indicated categorically: 16-18, 19-25, 26-35, 36-45, 46-55, 56-65, 66-70, 71-75, 76-80, 81-85, 86-90, 91-100, 100+
#Age categories in GLAD, EDGI and NBR have been created to reflect this

if(EDGI== TRUE) {
  # Create categorical age groups per 10 years
  edgi.coping.dem.raw.id <- edgi.coping.dem.raw.id %>%
    mutate(
      age_category_COPING_numeric =
        case_when(
          age_in_COPING >= 16 & age_in_COPING <= 18 ~ 1,
          age_in_COPING >= 19 & age_in_COPING <= 25 ~ 2,
          age_in_COPING >= 26 & age_in_COPING <= 35 ~ 3,
          age_in_COPING >= 36 & age_in_COPING <= 45 ~ 4,
          age_in_COPING >= 46 & age_in_COPING <= 55 ~ 5,
          age_in_COPING >= 56 & age_in_COPING <= 65 ~ 6,
          age_in_COPING >= 66 & age_in_COPING <= 70 ~ 7,
          age_in_COPING >= 71 & age_in_COPING <= 75 ~ 8,
          age_in_COPING >= 76 & age_in_COPING <= 80 ~ 9,
          age_in_COPING >= 81 & age_in_COPING <= 85 ~ 10,
          age_in_COPING >= 86 & age_in_COPING <= 90 ~ 11,
          age_in_COPING >= 91 & age_in_COPING <= 100 ~ 12,
          age_in_COPING >= 101 & age_in_COPING <= 120 ~ 13 # oldest person in the world is 117 years
        )
    )
  
  edgi.coping.dem.raw.id <- edgi.coping.dem.raw.id %>%
    mutate(
      age_category_COPING =
        recode_factor(
          age_category_COPING_numeric,
          "1" = "16-18",
          "2" = "19-25",
          "3" = "26-35",
          "4" = "36-45",
          "5" = "46-55",
          "6" = "56-65",
          "7" = "66-70",
          "8" = "71-75",
          "9" = "76-80",
          "10" = "81-85",
          "11" = "86-90",
          "12" = "91-100",
          "13" = "100+"
        )
    )
  edgi.coping.dem.raw.id %>%
    freq(age_category_COPING)
}


