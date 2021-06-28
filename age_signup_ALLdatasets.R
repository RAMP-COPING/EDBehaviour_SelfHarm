#CREATING AGE variable (age_unc)
##NBR
###age from birthday

if(NBR == TRUE) {
  #add 1900 to birthyear
  nbr.dem.raw.id$birthday_year_numeric <- nbr.dem.raw.id$birthday_year_numeric + 1900
  
  #create dob as date format
  nbr.dem.raw.id$Birthdate <- as.Date(paste(nbr.dem.raw.id$birthday_year_numeric,
                                            nbr.dem.raw.id$birthday_month_numeric,
                                            nbr.dem.raw.id$birthday_day_numeric,
                                            sep = "-"))
  
  head(nbr.dem.raw.id$Birthdate)
  
  
  #Calculate dem.age(remove NAs as it throws an error)
  ages <- eeptools::age_calc(na.omit(nbr.dem.raw.id$Birthdate),
                             enddate = as.Date(nbr.dem.raw.id$endDate),
                             units = "years")
  
  #Create dem.agecolumn
  nbr.dem.raw.id$age_unc<- NA
  
  #Populate with values (excluding NAs)
  nbr.dem.raw.id$age_unc[!is.na(nbr.dem.raw.id$Birthdate)] <- ages
  
  #Round age down
  nbr.dem.raw.id$age_unc <- floor(nbr.dem.raw.id$age_unc)
  
  #Visualise
  head(nbr.dem.raw.id$age_unc)
  head(nbr.dem.raw.id$Birthdate)
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
      glad.dem3.raw.id$age_unc_numeric > age_upper_limit |
        glad.dem3.raw.id$age_unc_numeric < age_lower_limit
    )
  )
  
  #Remove age outliers
  glad.dem3.raw.id <- glad.dem3.raw.id %>%
    mutate(
      age =
        if_else(
          age_unc_numeric > age_upper_limit |
            age_unc_numeric < age_lower_limit,
          true = NA_real_,
          false = age_unc_numeric,
          missing = NA_real_
        )
    )
  
  # Inspect variable
  glad.dem3.raw.id %>%
    descr(
      age,
      #stats = "common"
    )
}


##EDGI
###Cleaning age

# Clean Age variable
#Age outlier
if(EDGI == TRUE) {
  # Define age limits
  age_upper_limit = 117 # The oldest person in the world is 117 years
  age_lower_limit = 16
  
  #Identify number of outliers
  length(
    which(
      edgi.dem3.raw.id$age_unc_numeric > age_upper_limit |
        edgi.dem3.raw.id$age_unc_numeric < age_lower_limit
    )
  )
  
  #Remove age outliers
  edgi.dem3.raw.id <- edgi.dem3.raw.id %>%
    mutate(
      age =
        if_else(
          age_unc_numeric > age_upper_limit |
            age_unc_numeric < age_lower_limit,
          true = NA_real_,
          false = age_unc_numeric,
          missing = NA_real_
        )
    )
  
  # Inspect variable
  edgi.dem3.raw.id %>%
    descr(
      age
      #stats = "common"
    )
}

##NBR
###Cleaning age
# Clean Age variable
#Age outlier
if(NBR == TRUE) {
  # Define age limits
  age_upper_limit = 117 # The oldest person in the world is 117 years
  age_lower_limit = 16
  
  #Identify number of outliers
  length(
    which(
      nbr.dem.raw.id$age_unc > age_upper_limit |
        nbr.dem.raw.id$age_unc < age_lower_limit
    )
  )
  
  #Remove age outliers
  nbr.dem.raw.id <- nbr.dem.raw.id %>%
    mutate(
      age =
        if_else(
          age_unc > age_upper_limit |
            age_unc < age_lower_limit,
          true = NA_real_,
          false = age_unc,
          missing = NA_real_
        )
    )
  
  # Inspect variable
  nbr.dem.raw.id %>%
    descr(
      age,
      #stats = "common"
    )
}


## Categorise age into groups
#In RAMP, age is indicated categorically: 16-18, 19-25, 26-35, 36-45, 46-55, 56-65, 66-70, 71-75, 76-80, 81-85, 86-90, 91-100, 100+
  #Age categories in GLAD, EDGI and NBR have been created to reflect this
if(GLAD == TRUE) {
  # Create categorical age groups per 10 years
  glad.dem3.raw.id <- glad.dem3.raw.id %>%
    mutate(
      age_category_numeric =
        case_when(
          age >= 16 & age <= 18 ~ 1,
          age >= 19 & age <= 25 ~ 2,
          age >= 26 & age <= 35 ~ 3,
          age >= 36 & age <= 45 ~ 4,
          age >= 46 & age <= 55 ~ 5,
          age >= 56 & age <= 65 ~ 6,
          age >= 66 & age <= 70 ~ 7,
          age >= 71 & age <= 75 ~ 8,
          age >= 76 & age <= 80 ~ 9,
          age >= 81 & age <= 85 ~ 10,
          age >= 86 & age <= 90 ~ 11,
          age >= 91 & age <= 100 ~ 12,
          age >= 101 & age <= 120 ~ 13 # oldest person in the world is 117 years
        )
    )
  
  glad.dem3.raw.id <- glad.dem3.raw.id %>%
    mutate(
      age_category =
        recode_factor(
          age_category_numeric,
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
  glad.dem3.raw.id %>%
    freq(age_category)
}


## Categorise age into groups EDGI
#In RAMP, age is indicated categorically: 16-18, 19-25, 26-35, 36-45, 46-55, 56-65, 66-70, 71-75, 76-80, 81-85, 86-90, 91-100, 100+
  #Age categories in GLAD, EDGI and NBR have been created to reflect this
if(EDGI == TRUE) {
  # Create categorical age groups per 10 years
  edgi.dem3.raw.id <- edgi.dem3.raw.id %>%
    mutate(
      age_category_numeric =
        case_when(
          age >= 16 & age <= 18 ~ 1,
          age >= 19 & age <= 25 ~ 2,
          age >= 26 & age <= 35 ~ 3,
          age >= 36 & age <= 45 ~ 4,
          age >= 46 & age <= 55 ~ 5,
          age >= 56 & age <= 65 ~ 6,
          age >= 66 & age <= 70 ~ 7,
          age >= 71 & age <= 75 ~ 8,
          age >= 76 & age <= 80 ~ 9,
          age >= 81 & age <= 85 ~ 10,
          age >= 86 & age <= 90 ~ 11,
          age >= 91 & age <= 100 ~ 12,
          age >= 101 & age <= 120 ~ 13 # oldest person in the world is 117 years
        )
    )
  
  edgi.dem3.raw.id <-   edgi.dem3.raw.id %>%
    mutate(
      age_category =
        recode_factor(
          age_category_numeric,
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
  edgi.dem3.raw.id %>%
    freq(age_category)
}

## Categorise age into groups NBR
#In RAMP, age is indicated categorically: 16-18, 19-25, 26-35, 36-45, 46-55, 56-65, 66-70, 71-75, 76-80, 81-85, 86-90, 91-100, 100+
  #Age categories in GLAD, EDGI and NBR have been created to reflect this
if(NBR == TRUE) {
  # Create categorical age groups per 10 years
  nbr.dem.raw.id <- nbr.dem.raw.id %>%
    mutate(
      age_category_numeric =
        case_when(
          age >= 16 & age <= 18 ~ 1,
          age >= 19 & age <= 25 ~ 2,
          age >= 26 & age <= 35 ~ 3,
          age >= 36 & age <= 45 ~ 4,
          age >= 46 & age <= 55 ~ 5,
          age >= 56 & age <= 65 ~ 6,
          age >= 66 & age <= 70 ~ 7,
          age >= 71 & age <= 75 ~ 8,
          age >= 76 & age <= 80 ~ 9,
          age >= 81 & age <= 85 ~ 10,
          age >= 86 & age <= 90 ~ 11,
          age >= 91 & age <= 100 ~ 12,
          age >= 101 & age <= 120 ~ 13 # oldest person in the world is 117 years
        )
    )
  
  nbr.dem.raw.id  <- nbr.dem.raw.id  %>%
    mutate(
      age_category =
        recode_factor(
          age_category_numeric,
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
  nbr.dem.raw.id  %>%
    freq(age_category)
}